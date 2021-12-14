package day14

import scala.io.Source
import scala.collection.mutable

object ExtendedPolymerization extends App {

  val input = Source
    .fromFile("input/d14p01.txt")
    .getLines()
    .filter(_.nonEmpty)
    .toSeq

  val polymerTemplate = input.head

  def generatePairs(str: String): Seq[String] =
    for (i <- 0 until str.size - 1) yield str.slice(i, i + 2)

  var counts = mutable.Map[String, Long]()
  generatePairs(polymerTemplate).foreach(pair => counts.put(pair, counts.getOrElse(pair, 0L) + 1L))

  val transformations = input.tail.map { str =>
    val parts = str.split(" -> ")
    (parts(0) -> parts(1)(0))
  }.toMap

  def applyTransformations() {
    var newCounts = mutable.Map[String, Long]()
    counts.toSeq
      .flatMap { case (pair, count) =>
        transformations.get(pair) match {
          case Some(c) =>
            Seq((pair.slice(0, 1) + c, count), ("" + c + pair(1), count))
          case None =>
            Seq((pair, count))
        }
      }
      .foreach { case (pair, count) => newCounts.put(pair, newCounts.getOrElse(pair, 0L) + count) }
    counts = newCounts
  }

  def calculateDifference(): Long = {
    val map = mutable.Map[Char, Long]()
    counts.foreach { case (pair, count) =>
      map.put(pair(0), map.getOrElse(pair(0), 0L) + count)
      map.put(pair(1), map.getOrElse(pair(1), 0L) + count)
    }
    map.put(polymerTemplate.head, map.getOrElse(polymerTemplate.head, 0L) + 1L)
    map.put(polymerTemplate.last, map.getOrElse(polymerTemplate.last, 0L) + 1L)
    val sortedCounts = map.values.toSeq.sorted
    (sortedCounts.last - sortedCounts.head) / 2L
  }

  for (i <- 0 until 10) {
    applyTransformations()
  }

  println(calculateDifference())

  for (i <- 0 until 30) {
    applyTransformations()
  }

  println(calculateDifference())
}
