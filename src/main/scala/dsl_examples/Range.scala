package dsl_examples

import scala.annotation.tailrec

object Range {
  type Range = Vector[Int]

  @tailrec
  def inclusiveRange(start: Int, end: Int, acc: Range = Vector()): Range =
    if (start == end)
      acc :+ start
    else
      inclusiveRange(start + 1, end, acc :+ start)

  @tailrec
  def exclusiveRange(start: Int, end: Int, acc: Range = Vector()): Range =
    if (start == end)
      acc
    else
      exclusiveRange(start + 1, end, acc :+ start)

  implicit class IntRangeOps(self: Int) {
    def to_(bound: Int): Range = inclusiveRange(self, bound)
    def until_(bound: Int): Range =  exclusiveRange(self, bound)
  }
}

object RangeTest extends App {
  import Range._
}
