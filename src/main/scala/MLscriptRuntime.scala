package mlscript
package dsp

import mlscript.interpreter.{
  Class,
  Cont,
  Done,
  Environment,
  Field,
  Interpreter,
  InterpreterError,
  Paused,
  Record,
  Tracer,
  Tuple,
  Value,
  Values
}
import mlscript.utils.shorthands.Str
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsPromisesMod as fs
import typings.vscodeDebugprotocol.anon.{BreakpointsArray, Scopes, StackFrames, Variables}
import typings.vscodeDebugprotocol.mod.DebugProtocol.{Breakpoint, Scope, Source, SourceBreakpoint, StackFrame, Variable}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

class MLscriptRuntime(reporter: MLscriptReporter):
  private val interpreter: Interpreter             = Interpreter(reporter.output(_, "stdout"))
  private var fph: FastParseHelpers                = FastParseHelpers(IndexedSeq.empty)
  private var breakpoints: Map[String, Breakpoint] = Map.empty
  private var bpSource: Option[Source]             = None

  private var curLabel: Option[String]                  = None
  private var curEnv: Option[Environment]               = None
  private val curVariables: mutable.Map[Int, Variables] = mutable.Map.empty

  val threadId: Int      = 1
  val frameIdOffset: Int = 1024
  val scopeIdOffset: Int = 2048
  val varIdOffset: Int   = 4096

  def setBreakpoints(srcBreakpoints: Seq[SourceBreakpoint], source: Source): BreakpointsArray =
    bpSource = Some(source)
    breakpoints = (for (srcBp, idx) <- srcBreakpoints.zipWithIndex
    yield
      val bp = Breakpoint(true)
      bp.setId(idx)
      bp.setLine(srcBp.line)
      bp.setSource(source)
      srcBp.column.foreach(bp.setColumn(_))
      s"bp$idx" -> bp
    ).toMap
    BreakpointsArray(breakpoints.values.toJSArray)

  def insertBreakpoints(text: String): IndexedSeq[String] =
    text.linesIterator.zipWithIndex
      .foldLeft(IndexedSeq.empty[String]) { case (acc, (line, idx)) =>
        breakpoints.find { case (_, bp) =>
          bp.line.toOption match
            case None         => false
            case Some(bpLine) => bpLine == idx + 1
        } match
          case None => line +: acc
          case Some(label, bp) =>
            val curWhitespace = line.takeWhile(_.isWhitespace)
            val pauseLine     = curWhitespace + s"pause(\"$label\")"
            line +: pauseLine +: acc
      }
      .reverse

  def getStackFrames: StackFrames =
    val stackframes =
      for
        env                                  <- curEnv.toList
        ((stackName, stackValues, loc), idx) <- env.toStackFrames.zipWithIndex
      yield
        val (line, _, col) = fph.getLineColAt(loc.map(_.spanStart).getOrElse(0))
        val adjustedLine   = line - breakpoints.values.count(_.line.toOption.exists(_ < line))
        StackFrame(col, idx + frameIdOffset, adjustedLine, stackName)

    for
      source     <- bpSource
      stackframe <- stackframes
    do stackframe.setSource(source)

    for
      label      <- curLabel
      bp         <- breakpoints.get(label)
      line       <- bp.line.toOption
      stackFrame <- stackframes.headOption
    do
      stackFrame.setLine(line)
      stackFrame.setColumn(0)

    for globalFrame <- stackframes.lastOption
    do
      globalFrame.setLine(0)
      globalFrame.setColumn(0)

    for stackFrame <- stackframes
    do reporter.output(s"Stackframe: ${js.JSON.stringify(stackFrame)}}")
    StackFrames(stackframes.toJSArray)

  def getScopes(frameId: Int): Scopes =
    val scopeId     = frameId - frameIdOffset + scopeIdOffset
    val singleScope = Scope(false, "MLscript Scope", scopeId)

    curVariables.clear()
    val scopeVals =
      for
        env                         <- curEnv.toArray
        (stackName, scopeValues, _) <- env.toStackFrames.lift(scopeId - scopeIdOffset).toArray
        (name, value)               <- scopeValues
      yield insertVariable(name, value)
    curVariables += scopeId -> Variables(scopeVals.toJSArray)

    Scopes(js.Array(singleScope))

  def insertVariable(name: String, value: Values): Variable =
    value match
      case Tuple(fields) if fields.size > 0 =>
        val tupId = curVariables.size + varIdOffset

        curVariables += tupId -> Variables(js.Array())
        val tupVars =
          for ((_, Field(_, fldVal)), idx) <- fields.zipWithIndex
          yield insertVariable(s"$idx", fldVal)
        curVariables += tupId -> Variables(tupVars.toJSArray)

        Variable(name, Tuple(fields).toTerm.toString, tupId)

      case Record(fields) if fields.size > 0 =>
        val rcdId = curVariables.size + varIdOffset

        curVariables += rcdId -> Variables(js.Array())
        val rcdVars =
          for ((Var(fldName), Field(_, fldVal)) <- fields)
            yield insertVariable(fldName, fldVal)
        curVariables += rcdId -> Variables(rcdVars.toJSArray)

        Variable(name, Record(fields).toTerm.toString, rcdId)

      case classVal @ Class(typeName, Tuple(fields), _) if fields.size > 0 =>
        val clsId = curVariables.size + varIdOffset

        curVariables += clsId -> Variables(js.Array())
        val clsVars =
          for case ((Some(Var(fldName)), Field(_, fldVal))) <- fields
          yield insertVariable(fldName, fldVal)
        curVariables += clsId -> Variables(clsVars.toJSArray)

        Variable(name, classVal.toTerm.toString, clsId)

      case _ =>
        Variable(name, value.toTerm.toString, 0)

  def getVariables(id: Int): Variables =
    curVariables.getOrElse(id, Variables(js.Array()))

  def setVariable(containerId: Int, value: String): Value =
    val fph    = FastParseHelpers(IndexedSeq(value))
    val origin = Origin("setVariable", 0, fph)
    val lexer  = NewLexer(origin, _ => (), false)
    val tokens = lexer.bracketedTokens

    val parser = new NewParser(origin, tokens, true, _ => (), false, None):
      override protected def doPrintDbg(msg: => Str): Unit = ()
    val ast = parser.parseAll(parser.typingUnit)

    interpreter.resolveStmts(ast.entities)(using curEnv.get, Tracer(_.toString)) match
      case Done(retVal)             => retVal
      case Paused(label, env, rest) => throw InterpreterError(s"Unexpected pause at $label")

  def launch(program: String)(using ExecutionContext): Future[Unit] =
    fs.readFile(program, BufferEncoding.utf8)
      .toFuture
      .map(text =>
        val bpText = insertBreakpoints(text)

        fph = FastParseHelpers(bpText)
        val origin = Origin(program, 0, fph)
        val lexer  = NewLexer(origin, _ => (), false)
        val tokens = lexer.bracketedTokens

        val parser = new NewParser(origin, tokens, true, _ => (), false, None):
          override def doPrintDbg(msg: => String): Unit = ()
        val ast = parser.parseAll(parser.typingUnit)

        reporter.thread("started", threadId)
        step(interpreter.debug(ast))
      )

  def continue(requestThreadId: Int): Unit =
    if requestThreadId == threadId
    then
      for label <- curLabel
      do
        reporter.output(s"Continue from $label")
        step(interpreter.resume(label))

  def step(cont: Cont[Value]): Unit =
    cont match
      case Done(value) =>
        reporter.output(s"Done: $value")
        reporter.exit(0)
        reporter.terminate()

      case Paused(label, env, rest) =>
        breakpoints.get(label) match
          case None =>
            reporter.output(s"Pause at $label ignored")
            step(interpreter.resume(label))

          case Some(bp) =>
            reporter.output(s"Pause at $label")
            curLabel = Some(label)
            curEnv = Some(env)
            reporter.stop(bp.id.get, threadId)
