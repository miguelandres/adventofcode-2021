package day01

import scala.io.Source
import scala.collection.mutable.{Seq => MutableSeq}

object SonarMeasurements extends App {

  val inputData = Source.fromFile("input/d01p01.txt").getLines.map(_.toLong).toSeq;
  def part1() = {
    Console.println(countIncreases(inputData))
  }

  def countIncreases(seq: Seq[Long]): Long = {
    var prev = seq.head
    seq.tail
      .map { curr =>
        val res = if (prev < curr) { 1L }
        else { 0L }
        prev = curr
        res
      }
      .foldLeft(0L) { case (accum, curr) => accum + curr }
  }
  part1()

  def part2() = {
    var currentSeq = inputData
    var sumSeq = Seq[Long]()
    while (currentSeq.size >= 3) {
      val sum: Long = currentSeq.slice(0, 3).foldLeft(0L) { case (accum, item) => accum + item }
      sumSeq = sumSeq.appended(sum)
      currentSeq = currentSeq.slice(1, currentSeq.size)

    }
    Console.println(countIncreases(sumSeq))

  }
  part2()

}
