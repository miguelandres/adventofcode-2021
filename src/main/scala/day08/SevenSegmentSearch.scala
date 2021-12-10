package day08

import scala.io.Source
import scala.collection.mutable.Buffer

object SevenSegmentSearch extends App {

  val input: Seq[(Seq[String], Seq[String])] =
    Source.fromFile("input/d08p01.txt").getLines().toSeq.map { line =>
      val parts = line.split(" +\\| +")
      (
        parts(0).split(" +").toSeq.map(_.sorted),
        parts(1).split(" +").toSeq.map(_.sorted)
      )
    }

  val uniqueLengths = Set(2, 3, 4, 7)

  val count = input.map { case (_, outputs) =>
    outputs.map { case str =>
      if (uniqueLengths.contains(str.length())) { 1 }
      else { 0 }
    }.sum
  }.sum
  println(count)

  def findNumbers(observations: Seq[String]): Map[String, Int] = {
    val arr = Array.fill(10)("")
    arr(1) = observations.find(_.length == 2).get
    arr(4) = observations.find(_.length == 4).get
    arr(7) = observations.find(_.length == 3).get
    arr(8) = observations.find(_.length == 7).get
    arr(9) = observations.find(str => str.length == 6 && arr(4).forall(c => str.contains(c))).get
    arr(0) = observations
      .find(str => str.length == 6 && str != arr(9) && arr(1).forall(c => str.contains(c)))
      .get
    arr(6) = observations.find(str => str.length == 6 && str != arr(9) && str != arr(0)).get
    arr(3) = observations.find(str => str.length == 5 && arr(1).forall(c => str.contains(c))).get
    arr(5) = observations
      .find(str => str.length == 5 && str != arr(3) && str.forall(c => arr(9).contains(c)))
      .get
    arr(2) = observations.find(str => str.length == 5 && str != arr(5) && str != arr(3)).get

    (0 to 9).map(i => arr(i) -> i).toMap
  }

  val sumOutputs = input.foldLeft(0L) { case (total, (observations, output)) =>
    val numbers = findNumbers(observations)
    output.foldLeft(0L) { case (accum, observedPattern) =>
      accum * 10L + numbers(observedPattern.sorted).toLong
    } + total
  }
  println(sumOutputs)
}
