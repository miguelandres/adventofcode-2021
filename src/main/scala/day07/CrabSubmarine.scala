package day07

import scala.io.Source

import scala.collection.mutable
object CrabSubmarine extends App {
  var positions: Array[Long] =
    Source.fromFile("input/d07p01.txt").getLines().toSeq.head.split(",").map(_.toLong)
  val (min, max) = (positions.min, positions.max)

  var bestFuel = Long.MaxValue
  var bestDistance = Long.MaxValue
  for (i <- min to max) {
    val distances = positions.map(x => Math.abs(x - i))
    val distanceSum = distances.sum

    if (distanceSum < bestDistance) {
      bestDistance = distanceSum
    }

    val fuel = distances.map(distance => (distance * (distance + 1) / 2))
    val fuelSum = fuel.sum
    if (fuelSum < bestFuel) {
      bestFuel = fuelSum
    }
  }
  println(bestDistance)
  println(bestFuel)
}
