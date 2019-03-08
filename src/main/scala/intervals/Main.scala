package intervals

import scala.language.implicitConversions
import cats.Monoid
import cats.syntax.foldable._
import cats.instances.list._
import scala.math.Ordering

object Main extends App {
  import Interval._
  import NonOverlappingSortedIntervals._


  def check(originalList: List[Interval], expectedResult: List[Interval]): Unit = {
    val nonOverlappingSortedIntervals: List[NonOverlappingSortedIntervals] =
      originalList.map(interval => List(interval))

    val actualResult: NonOverlappingSortedIntervals =
      nonOverlappingSortedIntervals.combineAll

    require(expectedResult == actualResult, s"expected $expectedResult differs from $actualResult")
  }

  val t1: List[Interval] = List((1,3), (2,4), (6,8), (5,7))
  val r1: List[Interval] = List((1, 4), (5, 8))

  val t2: List[Interval] = List((2,4), (1,3), (6,8), (5,7))
  val r2: List[Interval] = List((1, 4), (5, 8))

  val t3: List[Interval] = List((1,4), (2,3), (6,8), (5,7))
  val r3: List[Interval] = List((1, 4), (5, 8))

  val t4: List[Interval] = List((1,4), (2,3), (6,8), (1,7))
  val r4: List[Interval] = List((1, 8))

  val t5: List[Interval] = List((1,2), (3,4), (6,8), (9,10))
  val r5: List[Interval] = List((1,2), (3,4), (6,8), (9,10))

  check(t1, r1)
  check(t2, r2)
  check(t3, r3)
  check(t4, r4)
  check(t5, r5)
}

case class Interval(min: Int, max: Int)
object Interval {
  implicit def intervalOrdering: Ordering[Interval] = Ordering.by(i => (i.min, i.max))
  implicit def tupleToInterval(t: (Int, Int)): Interval = {
    val (min, max) = t
    Interval(min, max)
  }
}

object NonOverlappingSortedIntervals {
  type NonOverlappingSortedIntervals = List[Interval]
  type SortedIntervals = List[Interval]

  def widestInterval(i1: Interval, i2: Interval): Interval =
    Interval(math.min(i1.min, i2.min), math.max(i1.max, i2.max))

  //O(N), sorry for recursion -  bored
  def mergeNonOverlappingSortedIntervals(
    a: NonOverlappingSortedIntervals,
    b: NonOverlappingSortedIntervals
  ): SortedIntervals = (a, b) match {
    case (Nil, _) => b
    case (_, Nil) => a
    case _ if a.head.min < b.head.min =>
      a.head :: mergeNonOverlappingSortedIntervals(a.tail, b)
    case _ if a.head.min >= b.head.min =>
      b.head :: mergeNonOverlappingSortedIntervals(a, b.tail)
  }

  //O(N)
  def compactSortedIntervals(intervals: List[Interval]): NonOverlappingSortedIntervals =
    intervals.foldLeft(List.empty[Interval]) { (acc, interval) =>
      if (acc.isEmpty) List(interval)
      else acc.init ++ (if (acc.last.max < interval.min) List(acc.last, interval)
                       else List(widestInterval(interval, acc.last)))}

  implicit val nonOverlappingIntervalsMonoid: Monoid[NonOverlappingSortedIntervals] = new Monoid[NonOverlappingSortedIntervals] {
    def empty: NonOverlappingSortedIntervals = Nil

    //O(N)
    def combine(
      a: NonOverlappingSortedIntervals,
      b: NonOverlappingSortedIntervals
    ): NonOverlappingSortedIntervals =
      compactSortedIntervals(mergeNonOverlappingSortedIntervals(a, b))
  }
}