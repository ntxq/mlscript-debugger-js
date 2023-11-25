package mlscript
package interpreter

final case class InterpreterError(msg: String) extends Exception(msg)
