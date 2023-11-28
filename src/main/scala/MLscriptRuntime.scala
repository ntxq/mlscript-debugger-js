package mlscript
package dsp

import mlscript.interpreter.Interpreter
import typings.node.bufferMod.global.BufferEncoding
import typings.node.readlinePromisesMod
import typings.node.fsPromisesMod as fs
import typings.vscodeDebugprotocol.mod.DebugProtocol.Breakpoint
import typings.vscodeDebugprotocol.mod.DebugProtocol.Source
import typings.vscodeDebugprotocol.mod.DebugProtocol.SourceBreakpoint

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class MLscriptRuntime(log: String => Unit, output: String => Unit):
  private val interpreter: Interpreter     = Interpreter(output)
  private var breakpoints: Seq[Breakpoint] = Seq.empty

  val threadId: Int = 1

  def setBreakpoints(srcBreakpoints: Seq[SourceBreakpoint], source: Source): Seq[Breakpoint] =
    breakpoints =
      for (srcBp, idx) <- srcBreakpoints.zipWithIndex
      yield
        val bp = Breakpoint(true)
        bp.setId(idx)
        bp.setLine(srcBp.line)
        bp.setSource(source)
        srcBp.column.foreach(bp.setColumn(_))
        bp
    breakpoints

  def insertBreakpoints(text: String): IndexedSeq[String] =
    text.linesIterator.zipWithIndex
      .foldLeft(IndexedSeq.empty[String]) { case (acc, (line, idx)) =>
        breakpoints.find {
          _.line.toOption match
            case None         => false
            case Some(bpLine) => bpLine == idx + 1
        } match
          case None => line +: acc
          case Some(bp) =>
            val curWhitespace = line.takeWhile(_.isWhitespace)
            val pauseLine     = curWhitespace + s"pause(\"bp${bp.id.get}\")"
            line +: pauseLine +: acc
      }
      .reverse

  def launch(program: String)(using ExecutionContext): Future[Unit] =
    fs.readFile(program, BufferEncoding.utf8)
      .toFuture
      .map(text =>
        log(s"Program:\n$text")
        log(s"Breakpoints:\n${breakpoints.map(bp => s"${bp.id.get}: ${bp.line.get}").mkString("\n")}")

        val bpText = insertBreakpoints(text)
        log(s"Program with breakpoints:\n${bpText.mkString("\n")}")
      )
