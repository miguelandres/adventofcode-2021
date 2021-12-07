package day07

import scala.io.Source

import scala.collection.mutable
object CrabSubmarine extends App {
  var positions: Array[Long] =
    Source.fromFile("input/d07p01.txt").getLines().toSeq.head.split(",").map(_.toLong)
  val (min, max) = (positions.min, positions.max)

  var bestPosition = min
  var bestFuel = Long.MaxValue
  for (i <- min to max) {
    val sum = positions.map { x =>
      val distance = Math.abs(x - i)
      val fuel = distance * (distance + 1) / 2
      fuel
    }.sum
    if (sum < bestFuel) {
      bestFuel = sum
      bestPosition = i
    }
  }
  println(bestFuel)
}
