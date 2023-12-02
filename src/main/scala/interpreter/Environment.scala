package mlscript
package interpreter

sealed abstract class Environment:
  def toStackFrames: List[(String, Map[String, Values])]

  def lookup(key: String): Values

  def extend(name: String, value: Values): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues) :: rest => (stackName, stackValues + (name -> value)) :: rest
          case Nil                              => throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then value else Environment.this.lookup(key)

  def extendRec(name: String, recTerm: Term): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues) :: rest => (stackName, stackValues + (name -> Thunk(recTerm, this))) :: rest
          case Nil                              => throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then Thunk(recTerm, this) else Environment.this.lookup(key)

  def extendFunDef(name: String, recTerm: Term): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values])] =
        Environment.this.toStackFrames match
          case (stackName, stackValues) :: rest =>
            (stackName, stackValues + (name -> FunDef(name, recTerm, this))) :: rest
          case Nil => throw InterpreterError("Cannot extend empty stack frame")
      def lookup(key: String): Values =
        if key == name then FunDef(name, recTerm, this) else Environment.this.lookup(key)

  def pushStackFrame(name: String, curEnv: Environment): Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values])] =
        (name, Map.empty) :: curEnv.toStackFrames
      def lookup(key: String): Values =
        Environment.this.lookup(key)

object Environment:
  def empty: Environment =
    new Environment:
      def toStackFrames: List[(String, Map[String, Values])] = List(("Global", Map.empty))
      def lookup(key: String): Values                        = throw InterpreterError(s"Unbound variable: $key")
