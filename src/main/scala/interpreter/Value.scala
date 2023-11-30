package mlscript
package interpreter

sealed abstract class Values:
  override def toString: String =
    this match
      case IntVal(value)  => s"<int: $value>"
      case DecVal(value)  => s"<dec: $value>"
      case StrVal(value)  => s"<str: $value>"
      case BoolVal(value) => s"<bool: $value>"
      case UnitVal()      => "<unit>"
      case Tuple(fields) =>
        s"<tuple: ${fields
            .map { case (name, fld) => s"${name.map(_.name + " = ").getOrElse("")}${fld.value}" }
            .mkString(", ")}>"
      case Record(fields) =>
        s"<record: ${fields.map { case (Var(name), fld) => s"$name = ${fld.value}" }.mkString(", ")}>"
      case Class(typeName, fields, body)       => s"<class: $typeName>"
      case Constructor(typeName, fields, body) => s"<constructor: $typeName>"
      case LamVal(lhs, rhs, _)                 => s"<lam: $lhs => $rhs>"
      case Thunk(term, _)                      => s"<thunk: $term>"

  def toTerm: Term =
    this match
      case IntVal(value)  => IntLit(value)
      case DecVal(value)  => DecLit(value)
      case StrVal(value)  => StrLit(value)
      case BoolVal(value) => Var(if value then "true" else "false")
      case UnitVal()      => UnitLit(false)
      case Tuple(fields) =>
        Tup(fields.map { case (name, fld) => (name, Fld(fld.flags, fld.value.toTerm)) })
      case Record(fields) =>
        Rcd(fields.map { case (name, fld) => (name, Fld(fld.flags, fld.value.toTerm)) })
      case Class(typeName, fields, body)       => App(Var(typeName), fields.toTerm)
      case Constructor(typeName, fields, body) => Var(typeName)
      case LamVal(lhs, rhs, env)               => Lam(lhs, rhs)
      case Thunk(term, env)                    => term

sealed abstract class Value extends Values

sealed abstract class LitVal               extends Value
final case class IntVal(value: BigInt)     extends LitVal
final case class DecVal(value: BigDecimal) extends LitVal
final case class StrVal(value: String)     extends LitVal
final case class BoolVal(value: Boolean)   extends LitVal
final case class UnitVal()                 extends LitVal

final case class Tuple(fields: List[(Option[Var], Field)])                    extends Value
final case class Record(fields: List[(Var, Field)])                           extends Value
final case class Class(typeName: String, fields: Tuple, body: TypingUnit)     extends Value
final case class Constructor(typeName: String, fields: Tup, body: TypingUnit) extends Value
final case class LamVal(lhs: Term, rhs: Term, env: Environment)               extends Value

final case class Thunk(term: Term, env: Environment) extends Values

final case class Field(flags: FldFlags, value: Value)
