package mlscript
package dsp

import mlscript.interpreter.{Cont, Done, Environment, Interpreter, Paused, Value}
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsPromisesMod as fs
import typings.vscodeDebugprotocol.anon.{BreakpointsArray, Scopes, StackFrames, Variables}
import typings.vscodeDebugprotocol.mod.DebugProtocol.{Breakpoint, Scope, Source, SourceBreakpoint, StackFrame, Variable}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import typings.std.stdStrings.start

class MLscriptRuntime(reporter: MLscriptReporter):
  private val interpreter: Interpreter             = Interpreter(reporter.output(_, "stdout"))
  private var fph: FastParseHelpers                = FastParseHelpers(IndexedSeq.empty)
  private var breakpoints: Map[String, Breakpoint] = Map.empty
  private var bpSource: Option[Source]             = None

  private var curLabel: Option[String]    = None
  private var curEnv: Option[Environment] = None

  val threadId: Int      = 1
  val frameIdOffset: Int = 1024
  val scopeIdOffset: Int = 1024

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
    val singleScope = Scope(false, "MLscript Scope", frameId + scopeIdOffset)
    Scopes(js.Array(singleScope))

  def getVariables(id: Int): Variables =
    id match
      case scopeId if scopeIdOffset <= scopeId =>
        val values =
          for
            env                         <- curEnv
            (stackName, scopeValues, _) <- env.toStackFrames.lift(scopeId - scopeIdOffset - frameIdOffset)
          yield scopeValues.map((name, value) => Variable(name, value.toTerm.toString, 0)).toArray
        Variables(values.getOrElse(Array.empty[Variable]).toJSArray)

      case _ =>
        Variables(js.Array())

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
