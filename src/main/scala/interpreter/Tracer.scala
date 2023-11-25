package mlscript
package interpreter

import scala.collection.mutable

class Tracer(inspect: Term => String):
  private var outputCnt: Int = 0
  private case class Trace(level: Int, result: Term):
    override def toString: String = "\t" * level + inspect(result)

  private var curLevel: Int                      = 0
  private val traces: mutable.ListBuffer[Trace]  = mutable.ListBuffer()
  private val scope: mutable.Stack[Term => Term] = mutable.Stack()

  def push(redex: Term, f: Term => Term): Unit =
    traces += Trace(curLevel, scope.foldLeft(redex) { (redex, f) => f(redex) })
    scope.push(f)
    curLevel += 1

  def pop(result: Term): Unit =
    curLevel -= 1
    traces += Trace(curLevel, scope.foldLeft(result) { (result, f) => f(result) })
    scope.pop()

  private def removeDup: List[Trace] =
    def loop(
        acc: (List[Trace], List[Trace]),
        prev: Option[(Trace, Trace)],
        traces: (List[Trace], List[Trace])
    ): List[Trace] =
      (acc, prev, traces) match
        case ((front, back), None, (Nil, Nil)) =>
          front.reverse ++ back
        case ((front, back), Some(x, y), (Nil, Nil)) =>
          loop((x :: front, y :: back), None, (Nil, Nil))
        case ((front, back), None, (x :: xs, y :: ys)) =>
          loop((front, back), Some((x, y)), (xs, ys))
        case ((front, back), Some(x, y), (x2 :: xs, y2 :: ys)) =>
          if inspect(x.result) == inspect(x2.result) && inspect(y.result) == inspect(y2.result)
          then loop((front, back), Some((x, y)), (xs, ys))
          else loop((x :: front, y :: back), Some((x2, y2)), (xs, ys))
        case _ => throw InterpreterError("Unequal length of traces")

    val (front, back) = traces.toList.splitAt(traces.size / 2)
    loop((Nil, Nil), None, (front, back.reverse))

  def getTraces: List[String] =
    val output = traces.map(_.toString).toList.drop(outputCnt)
    outputCnt += output.size
    output
