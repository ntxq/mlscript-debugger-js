package mlscript
package dsp

import scala.collection.mutable
import fs2.io.file.{Files, Path}
import mlscript.interpreter.*

class MLscriptRuntime(log: String => Unit, output: String => Unit):
  given cats.effect.unsafe.IORuntime = cats.effect.unsafe.IORuntime.global
  val interpreter: Interpreter       = Interpreter(output)

  def launch(program: String): Unit =
    Files[cats.effect.IO]
      .readUtf8Lines(Path(program))
      .compile
      .toVector
      .unsafeRunAsync {
        case Left(error) =>
          log(s"Error: $error")
        case Right(lines) =>
          log(s"Program:\n${lines.mkString("\n")}")
      }
