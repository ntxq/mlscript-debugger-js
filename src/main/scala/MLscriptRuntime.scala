package mlscript
package dsp

import mlscript.interpreter.Cont
import mlscript.interpreter.Done
import mlscript.interpreter.Environment
import mlscript.interpreter.Interpreter
import mlscript.interpreter.Paused
import mlscript.interpreter.Value
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsPromisesMod as fs
import typings.vscodeDebugprotocol.anon.BreakpointsArray
import typings.vscodeDebugprotocol.anon.Scopes
import typings.vscodeDebugprotocol.anon.StackFrames
import typings.vscodeDebugprotocol.anon.Variables
import typings.vscodeDebugprotocol.mod.DebugProtocol.Breakpoint
import typings.vscodeDebugprotocol.mod.DebugProtocol.Scope
import typings.vscodeDebugprotocol.mod.DebugProtocol.Source
import typings.vscodeDebugprotocol.mod.DebugProtocol.SourceBreakpoint
import typings.vscodeDebugprotocol.mod.DebugProtocol.StackFrame
import typings.vscodeDebugprotocol.mod.DebugProtocol.Variable

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

class MLscriptRuntime(reporter: MLscriptReporter):
  private val interpreter: Interpreter             = Interpreter(reporter.output(_, "stdout"))
  private var breakpoints: Map[String, Breakpoint] = Map.empty
  private var bpSource: Option[Source]             = None

  private var curLabel: Option[String]    = None
  private var curEnv: Option[Environment] = None

  val threadId: Int     = 1
  val stackFrameId: Int = 2
  val scopeId: Int      = 3

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
    val singleStackFrame = StackFrame(0, stackFrameId, 0, "MLscript StackFrame")

    for source <- bpSource
    do singleStackFrame.setSource(source)

    for
      label <- curLabel
      bp    <- breakpoints.get(label)
      line  <- bp.line.toOption
    do singleStackFrame.setLine(line)

    StackFrames(js.Array(singleStackFrame))

  def getScopes: Scopes =
    val singleScope = Scope(false, "MLscript Scope", scopeId)
    Scopes(js.Array(singleScope))

  def getVariables: Variables =
    val variables =
      (for env <- curEnv
      yield env.toMap.map((name, value) => Variable(name, value.toTerm.toString, 0)).toArray)
        .getOrElse(Array.empty[Variable])
    Variables(variables.toJSArray)

  def launch(program: String)(using ExecutionContext): Future[Unit] =
    fs.readFile(program, BufferEncoding.utf8)
      .toFuture
      .map(text =>
        reporter.output(s"Program:\n$text")
        reporter
          .output(s"Breakpoints:\n${breakpoints.map { case (label, bp) => s"$label: ${bp.line.get}" }.mkString("\n")}")

        val bpText = insertBreakpoints(text)
        reporter.output(s"Program with breakpoints:\n${bpText.mkString("\n")}")

        val fph    = FastParseHelpers(bpText)
        val origin = Origin(program, 0, fph)
        val lexer  = NewLexer(origin, _ => (), false)
        val tokens = lexer.bracketedTokens
        reporter.output("Tokens:\n" + NewLexer.printTokens(tokens))

        val parser = new NewParser(origin, tokens, true, _ => (), false, None):
          override def doPrintDbg(msg: => String): Unit = ()
        val ast = parser.parseAll(parser.typingUnit)
        reporter.output("AST:\n" + codegen.Helpers.inspect(ast))

        reporter.thread("started", threadId)
        step(interpreter.debug(ast))
      )

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
