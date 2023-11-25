package mlscript
package interpreter

sealed abstract class Cont[A]:
  def map[B](f: A => B): Cont[B] =
    flatMap(value => Done(f(value)))

  def flatMap[B](f: A => Cont[B]): Cont[B] =
    this match
      case Done(value)              => f(value)
      case Paused(label, env, rest) => Paused(label, env, () => rest().flatMap(f))

  def withFilter(p: A => Boolean): Cont[A] =
    this match
      case Done(value) if p(value)  => Done(value)
      case Done(_)                  => throw InterpreterError("Cont.withFilter: predicate is not satisfied")
      case Paused(label, env, rest) => Paused(label, env, () => rest().withFilter(p))

final case class Done[A](value: A)                                               extends Cont[A]
final case class Paused[A](label: String, env: Environment, rest: () => Cont[A]) extends Cont[A]

object Cont:
  def pure[A](value: A): Cont[A] =
    Done(value)

extension [A](xs: List[A])
  def traverse[B](f: A => Cont[B]): Cont[List[B]] =
    xs.foldRight[Cont[List[B]]](Done(Nil))((x, acc) =>
      for
        xVal   <- f(x)
        accVal <- acc
      yield xVal :: accVal
    )

extension (args: List[(Option[Var], Fld)])
  def traverseArgs(trace: Term => Term, f: Term => Cont[Value])(using tracer: Tracer): Cont[Tuple] =
    def loop(args: Cont[(Tuple, List[(Option[Var], Fld)])]): Cont[Tuple] =
      for
        (prev, rest) <- args
        argsVal <- rest match
          case Nil =>
            Done(prev)
          case (name, Fld(flags, arg)) :: next =>
            val redex = (cur: Term) =>
              Tup(prev.fields.map { case (name, Field(argFlag, value)) =>
                (name, Fld(argFlag, value.toTerm))
              } ++ List((name, Fld(flags, cur))) ++ next)

            for
              ()     <- Cont.pure(tracer.push(trace(redex(arg)), redex andThen trace))
              argVal <- f(arg)
              ()     <- Cont.pure(tracer.pop(argVal.toTerm))
              retVal <- loop(Done((Tuple(prev.fields ++ List((name, Field(flags, argVal)))), next)))
            yield retVal
      yield argsVal

    loop(Done((Tuple(Nil), args)))

extension (args: List[(Var, Fld)])
  def traverseRcds(trace: Term => Term, f: Term => Cont[Value])(using
      tracer: Tracer
  ): Cont[Record] =
    def loop(args: Cont[(Record, List[(Var, Fld)])]): Cont[Record] =
      for
        (prev, rest) <- args
        argsVal <- rest match
          case Nil =>
            Done(prev)
          case (name, Fld(flags, arg)) :: next =>
            val redex = (cur: Term) =>
              Rcd(prev.fields.map { case (name, Field(argFlag, value)) =>
                (name, Fld(argFlag, value.toTerm))
              } ++ List((name, Fld(flags, cur))) ++ next)

            for
              ()     <- Cont.pure(tracer.push(trace(redex(arg)), redex andThen trace))
              argVal <- f(arg)
              ()     <- Cont.pure(tracer.pop(argVal.toTerm))
              retVal <- loop(
                Done((Record(prev.fields ++ List((name, Field(flags, argVal)))), next))
              )
            yield retVal
      yield argsVal

    loop(Done((Record(Nil), args)))
