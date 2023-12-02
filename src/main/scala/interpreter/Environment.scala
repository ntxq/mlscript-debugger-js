package mlscript
package interpreter

sealed abstract class Environment:
  def toStackFrames: List[(String, Map[String, Values], Option[Loc])]

  def lookup(key: String): Values

  def extend(name: String, value: Values): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values], Option[Loc])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues, loc) :: rest => (stackName, stackValues + (name -> value), loc) :: rest
          case Nil                                   => throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then value else Environment.this.lookup(key)

  def extendRec(name: String, recTerm: Term): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values], Option[Loc])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues, loc) :: rest =>
            (stackName, stackValues + (name -> Thunk(recTerm, this)), loc) :: rest
          case Nil =>
            throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then Thunk(recTerm, this) else Environment.this.lookup(key)

  def extendFunDef(name: String, recTerm: Term): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values], Option[Loc])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues, loc) :: rest =>
            (stackName, stackValues + (name -> FunDef(name, recTerm, this)), loc) :: rest
          case Nil => throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then FunDef(name, recTerm, this) else Environment.this.lookup(key)

  def pushStackFrame(name: String, curEnv: Environment, curTerm: Term): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values], Option[Loc])] =
        (name, Map.empty, curTerm.toLoc) :: curEnv.toStackFrames
      def lookup(key: String): Values =
        Environment.this.lookup(key)

object Environment:
  def empty: Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values], Option[Loc])] = List(("Global", Map.empty, None))
      def lookup(key: String): Values = throw InterpreterError(s"Unbound variable: $key")
