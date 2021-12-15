package day15

import scala.io.Source
import scala.collection.mutable

object Chiton extends App {

  val input: Array[Array[Int]] = Source
    .fromFile("input/d15p01.txt")
    .getLines()
    .map(_.map(c => (c - '0').toInt).toArray)
    .toArray

  val (sizeX, sizeY) = (input.length, input(0).length)

  val movementDirection = Seq((0, 1), (0, -1), (1, 0), (-1, 0))

  def inRange(length: Int)(coord: Int): Boolean = coord >= 0 && coord < length
  def calculateCost(x: Int, y: Int): Int = {
    val originalCost = input(x % sizeX)(y % sizeY)
    (originalCost - 1 + x / sizeX + y / sizeY) % 9 + 1
  }

  def process(multiplier: Int) {
    val minCost: Array[Array[Int]] =
      Array.fill(sizeX * multiplier)(Array.fill(sizeY * multiplier)(Int.MaxValue))
    val visited: Array[Array[Boolean]] =
      Array.fill(sizeX * multiplier)(Array.fill(sizeY * multiplier)(false))
    val queue = mutable.Map[(Int, Int), Int]()
    minCost(0)(0) = 0
    queue.put((0, 0), 0)

    while (queue.nonEmpty) {
      var ((x, y), cost) = queue.toSeq.sortBy(_._2).head
      minCost(x)(y) = cost
      queue.remove((x, y))

      val neighbors = movementDirection.collect {
        case (dX, dY)
            if inRange(sizeX * multiplier)(x + dX) && inRange(sizeY * multiplier)(y + dY) =>
          val newX = x + dX;
          val newY = y + dY;
          val newCost = calculateCost(newX, newY) + cost
          if (
            !visited(newX)(newY) && queue
              .getOrElse((newX, newY), Int.MaxValue) > newCost && minCost(newX)(newY) > newCost
          ) {
            queue((newX, newY)) = newCost
          }
      }
      visited(x)(y) = true
    }

    println(minCost(sizeX * multiplier - 1)(sizeY * multiplier - 1))
  }
  process(1)
  process(5)
}
