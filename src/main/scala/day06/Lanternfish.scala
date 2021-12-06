package day06

import scala.io.Source

import scala.collection.mutable
object Lanternfish extends App {
  var fish = Source.fromFile("input/d06p01.txt").getLines().toSeq.head.split(",").map(_.toInt)

  var fishMap = {
    var map = mutable.Map.from((0 to 8).map(x => (x, 0L)))
    fish.foreach(timer => map(timer) = map(timer) + 1L)
    map.toMap
  }

  for (i <- 1 to 256) {
    val numberOfResetFish = fishMap(0)
    fishMap = fishMap.map {
      case (0, count)     => (8, count)
      case (7, count)     => (6, count + numberOfResetFish)
      case (timer, count) => (timer - 1, count)
    }
  }

  println(fishMap.values.sum)
}
