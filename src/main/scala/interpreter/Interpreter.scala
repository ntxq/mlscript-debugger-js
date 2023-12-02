package mlscript
package interpreter

import codegen.Helpers.inspect
import collection.mutable

class Interpreter(output: String => Unit):
  var suspended: mutable.Map[String, Paused[Value]] = mutable.Map.empty

  def step(cont: Cont[Value]): Cont[Value] =
    cont match
      case done: Done[Value] =>
        done
      case paused @ Paused(label, _, _) =>
        suspended += (label -> paused)
        paused

  def debug(typingUnit: TypingUnit): Cont[Value] =
    val tracer = Tracer(_.toString)
    step(resolveStmts(typingUnit.entities)(using Environment.empty, tracer))

  def resume(label: String): Cont[Value] =
    suspended.get(label) match
      case None                     => throw InterpreterError(s"Resume: $label")
      case Some(Paused(_, _, rest)) => step(rest())

  def resolveStmts(stmts: List[Statement])(using env: Environment, tracer: Tracer): Cont[Value] =
    def loop(acc: Cont[Value], env: Environment, stmts: List[Statement]): Cont[Value] =
      stmts match
        case Nil =>
          acc

        case (term: Term) :: rest =>
          for
            prevVal <- acc
            ()      <- Cont.pure(tracer.push(Blk(stmts), value => Blk(value :: rest)))
            curVal  <- evaluate(term)(using env)
            ()      <- Cont.pure(tracer.pop(curVal.toTerm))
            retVal  <- loop(Cont.pure(curVal), env, rest)
          yield retVal

        case (fun @ NuFunDef(None, Var(name), _, _, Left(body))) :: rest =>
          loop(acc, env.extendFunDef(name, body), rest)

        case (fun @ NuFunDef(Some(isRec), Var(name), _, _, Left(body))) :: rest =>
          val recEnv =
            if isRec
            then env.extendRec(name, body)
            else env

          for
            prevVal <- acc
            curVal  <- evaluate(body)(using recEnv)
            newEnv = env.extend(name, curVal)
            retVal <- loop(Cont.pure(curVal), newEnv, rest)
          yield retVal

        case (tyDef @ NuTypeDef(Cls, TypeName(typeName), _, params, _, _, _, _, _, body)) :: rest =>
          val fields = params.getOrElse(Tup(List()))
          val newEnv = env.extend(typeName, Constructor(typeName, fields, body))
          loop(acc, newEnv, rest)

        case stmt :: _ =>
          throw InterpreterError(s"Unimplemented: ${stmt}")

    loop(Done(UnitVal()), env, stmts)

  // TODO: Fix tracing
  def resolveIfBody(body: IfBody)(using env: Environment, tracer: Tracer): Cont[Option[Value]] =
    body match
      case IfBlock(Left(bbody) :: rest) =>
        for
          subVal <- resolveIfBody(bbody)
          retVal <- subVal match
            case Some(value) => Cont.pure(Some(value))
            case None        => resolveIfBody(IfBlock(rest))
        yield retVal

      case IfThen(Var("_"), thenn) =>
        for thennVal <- evaluate(thenn)
        yield Some(thennVal)

      case IfThen(App(Var("is"), Tup(List((None, Fld(_, scrut)), (None, Fld(_, pat))))), thenn) =>
        for
          scrutVal <- evaluate(scrut)
          retVal <- resolvePattern(scrutVal, pat, env) match
            case None           => Cont.pure(None)
            case Some(matchEnv) => evaluate(thenn)(using matchEnv).map(Some(_))
        yield retVal

      case IfThen(cond, thenn) =>
        for
          condVal <- evaluate(cond)
          retVal <- condVal match
            case BoolVal(true)  => evaluate(thenn).map(Some(_))
            case BoolVal(false) => Cont.pure(None)
            case _              => throw InterpreterError(s"If-Cond: $condVal")
        yield retVal

      case IfOpsApp(lhs, (Var("is"), IfThen(pat, thenn)) :: rest) =>
        for
          scrutVal <- evaluate(lhs)
          retVal <- resolvePattern(scrutVal, pat, env) match
            case None           => resolveIfBody(IfOpsApp(lhs, rest))
            case Some(matchEnv) => evaluate(thenn)(using matchEnv).map(Some(_))
        yield retVal

      case _ =>
        throw InterpreterError(s"If-body: ${inspect(body)}")

  def resolvePattern(scrut: Value, pat: Term, env: Environment): Option[Environment] =
    (scrut, pat) match
      case (Class(typeName, _, _), Var(patName)) if typeName == patName =>
        Some(env)

      case (scrutVal, Var(patVar)) =>
        Some(env.extend(patVar, scrutVal))

      case (Class(typeName, Tuple(scrutParams), _), App(Var(patName), Tup(patParams))) if typeName == patName =>
        scrutParams.zip(patParams).foldLeft[Option[Environment]](Some(env)) {
          case (None, _) =>
            None
          case (Some(accEnv), ((_, Field(_, subScrut)), (_, Fld(_, subPat)))) =>
            resolvePattern(subScrut, subPat, accEnv)
        }

      case _ =>
        None

  def resolveName(name: String)(using env: Environment, tracer: Tracer): Cont[Value] =
    env.lookup(name) match
      case value: Value =>
        Cont.pure(value)

      case Thunk(term, thunkEnv) =>
        for value <- evaluate(term)(using thunkEnv)
        yield value

      case FunDef(funName, term, funEnv) =>
        for lamVal <- evaluate(term)(using funEnv)
        yield lamVal match
          case LamVal(_, lhs, rhs, env) => LamVal(funName, lhs, rhs, env)
          case _                        => throw InterpreterError(s"FunDef: $lamVal")

  def evaluate(term: Term)(using env: Environment, tracer: Tracer): Cont[Value] =
    term match
      case IntLit(value) =>
        Cont.pure(IntVal(value))

      case StrLit(value) =>
        Cont.pure(StrVal(value))

      case UnitLit(_) =>
        Cont.pure(UnitVal())

      case Var(name) =>
        resolveName(name)

      case Lam(lhs, rhs) =>
        Cont.pure(LamVal("Anonymous function", lhs, rhs, env))

      case App(Var("pause"), Tup(List((None, Fld(_, StrLit(label)))))) =>
        Paused(label, env, () => Done(UnitVal()))

      case App(Var("cond"), Tup(preds)) =>
        for
          predsVal <- preds.traverseArgs(identity, evaluate)(using Tracer(_ => ""))
          retVal <- predsVal match
            case Tuple(List((None, Field(_, StrVal(label))), (None, Field(_, BoolVal(true))))) =>
              Paused(label, env, () => Done(UnitVal()))
            case _ =>
              Done(UnitVal())
        yield retVal

      case App(Var("log"), message) =>
        for
          case Tuple(List((None, Field(_, messageVal)))) <- evaluate(message)
          () <- Cont.pure(output(messageVal.toTerm.toString))
        yield UnitVal()

      case App(Var("+"), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var("+"), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            IntVal(lhs + rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            DecVal(lhs + rhs)
          case _ =>
            throw InterpreterError(s"App-Add: $argsVal")

      case App(Var("-"), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var("-"), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            IntVal(lhs - rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            DecVal(lhs - rhs)
          case _ =>
            throw InterpreterError(s"App-Sub: $argsVal")

      case App(Var("*"), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var("*"), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            IntVal(lhs * rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            DecVal(lhs * rhs)
          case _ =>
            throw InterpreterError(s"App-Mul: $argsVal")

      case App(Var(">"), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var(">"), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            BoolVal(lhs > rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            BoolVal(lhs > rhs)
          case _ =>
            throw InterpreterError(s"App-Gt: $argsVal")

      case App(Var("<"), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var("<"), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            BoolVal(lhs < rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            BoolVal(lhs < rhs)
          case _ =>
            throw InterpreterError(s"App-Lt: $argsVal")

      case App(Var("=="), Tup(args)) =>
        for argsVal <- args.traverseArgs(argsVal => App(Var("=="), argsVal), evaluate)
        yield argsVal match
          case Tuple(List((None, Field(_, IntVal(lhs))), ((None, Field(_, IntVal(rhs)))))) =>
            BoolVal(lhs == rhs)
          case Tuple(List((None, Field(_, DecVal(lhs))), ((None, Field(_, DecVal(rhs)))))) =>
            BoolVal(lhs == rhs)
          case Tuple(List((None, Field(_, StrVal(lhs))), ((None, Field(_, StrVal(rhs)))))) =>
            BoolVal(lhs == rhs)
          case Tuple(List((None, Field(_, BoolVal(lhs))), ((None, Field(_, BoolVal(rhs)))))) =>
            BoolVal(lhs == rhs)
          case Tuple(List((None, Field(_, UnitVal())), ((None, Field(_, UnitVal()))))) =>
            BoolVal(true)
          case _ =>
            throw InterpreterError(s"App-Eq: $argsVal")

      case App(lhs, Tup(args)) =>
        for
          ()       <- Cont.pure(tracer.push(term, lhsVal => App(lhsVal, Tup(args))))
          lhsVal   <- evaluate(lhs)
          ()       <- Cont.pure(tracer.pop(lhsVal.toTerm))
          argsVals <- args.traverseArgs(argsVal => App(lhsVal.toTerm, argsVal), evaluate)

          retVal <- lhsVal match
            case LamVal(funName, Tup(params), body, lamEnv) =>
              val argEnv = params.zip(argsVals.fields).foldLeft(lamEnv.pushStackFrame(funName, env)) {
                case (env, ((_, Fld(_, Var(name))), (_, Field(_, value)))) =>
                  env.extend(name, value)

                case (env, ((_, Fld(_, Bra(true, Rcd(param)))), (_, Field(_, Record(value))))) =>
                  param.foldLeft(env) {
                    case (env, (Var(name), Fld(_, Var(paramName)))) =>
                      val paramValue = value.find { case (Var(valName), _) =>
                        valName == paramName
                      } match
                        case None                     => throw InterpreterError(s"App-Param-Rcd: $paramName")
                        case Some(_, Field(_, value)) => value
                      env.extend(name, paramValue)

                    case (_, (Var(name), Fld(_, param))) =>
                      throw InterpreterError(s"App-Param-Rcd: ${inspect(param)} = $value")
                  }

                case (env, ((_, Fld(_, param)), (_, Field(_, value)))) =>
                  throw InterpreterError(s"App-Param: ${inspect(param)} = $value")
              }
              evaluate(body)(using argEnv)

            case Constructor(typeName, Tup(params), body) =>
              val classFld =
                params.zip(argsVals.fields).foldRight[List[(Option[Var], Field)]](Nil) {
                  case (((name, Fld(_, _)), (_, Field(_, value))), acc) =>
                    (name, Field(FldFlags(false, false, false), value)) :: acc
                }
              Cont.pure(Class(typeName, Tuple(classFld), body))

            case _ =>
              throw InterpreterError(s"App-Lhs: $lhsVal")
        yield retVal

      case Tup(fields) =>
        for tuple <- fields.traverseArgs(identity, evaluate)
        yield tuple

      case Rcd(fields) =>
        for rcdVal <- fields.traverseRcds(identity, evaluate)
        yield rcdVal

      case Sel(receiver, Var(fieldName)) =>
        for
          ()        <- Cont.pure(tracer.push(term, receiverVal => Sel(receiverVal, Var(fieldName))))
          recordVal <- evaluate(receiver)
          ()        <- Cont.pure(tracer.pop(recordVal.toTerm))
        yield recordVal match
          case Record(fields) =>
            fields.find { case (Var(name), _) => name == fieldName } match
              case None                     => throw InterpreterError(s"Sel-Field: $recordVal.$fieldName")
              case Some(_, Field(_, value)) => value

          case Class(_, Tuple(fields), _) =>
            fields.find {
              case (Some(Var(name)), _) => name == fieldName
              case (None, _)            => false
            } match
              case None =>
                recordVal match
                  case Class(_, Tuple(fields), _) => throw InterpreterError(s"Sel-Field: ${fields}")
                  case _                          => throw InterpreterError(s"Sel-Field: $recordVal.$fieldName")
              case Some(_, Field(_, value)) => value

          case _ =>
            throw InterpreterError(s"Sel-Receiver: $recordVal")

      case Let(isRec, Var(name), rhs, body) =>
        val recEnv =
          if isRec
          then env.extendRec(name, rhs)
          else env
        for
          ()      <- Cont.pure(tracer.push(term, rhsVal => Let(isRec, Var(name), rhsVal, body)))
          rhsVal  <- evaluate(rhs)(using recEnv)
          ()      <- Cont.pure(tracer.pop(rhsVal.toTerm))
          bodyVal <- evaluate(body)(using env.extend(name, rhsVal))
        yield bodyVal

      case Blk(stmts) =>
        resolveStmts(stmts)

      case Bra(_, body) =>
        evaluate(body)

      case If(body, els) =>
        for
          bodyVal <- resolveIfBody(body)
          retVal <- (bodyVal, els) match
            case (Some(value), _)      => Cont.pure(value)
            case (None, Some(elsTerm)) => evaluate(elsTerm)
            case (None, None)          => throw InterpreterError(s"If-Else: $bodyVal")
        yield retVal

      case _ =>
        throw InterpreterError(s"Unimplemented: ${inspect(term)}")
