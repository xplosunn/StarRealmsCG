import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.util.Random

object Ops {
  implicit class RangeOps[T](range: Range) {
    def toNEL() = {
      NonEmptyList(range.head, range.tail.toList)
    }
  }

  implicit class NELOps[T](list: NonEmptyList[T]) {
    def random: T = {
      list.toList(Math.abs(new Random().nextInt() % list.length)).asInstanceOf[T]
    }

    def randomWeighed(f: T => Int): T = {
      weightedPick(list, Random.nextInt(list.toList.map(f).sum), f)
    }
  }

  @tailrec
  private def weightedPick[T](list: NonEmptyList[T], weight: Int, f: T => Int): T = {
    list.tail match {
      case Nil =>
        list.head
      case tHead :: tTail =>
        val fHead = f(list.head)
        if(fHead > weight)
          list.head
        else {
          val tail = NonEmptyList(tHead, tTail)
          weightedPick(tail, weight - fHead, f)
        }
    }
  }

  def toIntRoundedUp(double: Double): Int = {
    val int = double.toInt
    if(int.toDouble == double)
      int
    else
      int + 1
  }
}