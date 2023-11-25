package mlscript
package interpreter

sealed abstract class Environment:
  def toMap: Map[String, Values]
  def lookup(key: String): Values

  def extend(name: String, value: Values): Environment =
    new Environment:
      def toMap: Map[String, Values] =
        Environment.this.toMap + (name -> value)
      def lookup(key: String): Values =
        if key == name then value else Environment.this.lookup(key)

  def extendRec(name: String, recTerm: Term): Environment =
    new Environment:
      def toMap: Map[String, Values] =
        Environment.this.toMap + (name -> Thunk(recTerm, this))
      def lookup(key: String): Values =
        if key == name then Thunk(recTerm, this) else Environment.this.lookup(key)

object Environment:
  def empty: Environment =
    new Environment:
      def toMap: Map[String, Values] =
        Map.empty

      def lookup(key: String): Values =
        throw InterpreterError(s"Unbound variable: $key")
