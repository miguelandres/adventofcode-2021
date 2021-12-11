package day11

import scala.io.Source
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
object DumboOctopus extends App {

  val input: Array[Array[Option[Int]]] = Source
    .fromFile("input/d11p01.txt")
    .getLines()
    .map(str => str.map(c => Option((c - '0').toInt)).toArray)
    .toArray

  val neighbors: Seq[(Int, Int)] = (-1 to 1).flatMap { x =>
    (-1 to 1).flatMap(y => if (x == 0 && y == 0) None else Some((x, y)))
  }.toSeq

  {
    var countFlashes = 0
    var i = 1
    breakable {

      while (true) {
        val flashes = increaseAll()
        while (flashes.nonEmpty) {
          countFlashes = countFlashes + 1
          val (x, y) = flashes.remove(0)
          neighbors.map { case (dX, dY) => (x + dX, y + dY) }.foreach { case (nX, nY) =>
            if (increase(nX, nY)) {
              flashes.append((nX, nY))
            }
          }
        }
        if (i == 100) println(s"At 100 steps there were $countFlashes flashes")
        if (resetAllFlashed() == 100) break
        i = i + 1
      }
    }
    println(s"Stopped after step $i")
  }

  def increase(x: Int, y: Int): Boolean = {
    var flashed = false
    if ((0 until 10).contains(x) && (0 until 10).contains(y)) {
      input(x)(y) = input(x)(y) match {
        case None => None
        case Some(9) =>
          flashed = true
          None
        case Some(x) => Some(x + 1)
      }
    }
    flashed
  }

  def increaseAll(): Buffer[(Int, Int)] = {
    val flashes = Buffer[(Int, Int)]()
    for (i <- 0 until 10; j <- 0 until 10) {
      if (increase(i, j)) {
        flashes.append((i, j))
      }
    }
    flashes
  }
  def resetAllFlashed(): Int = {
    var count = 0
    for (i <- 0 until 10; j <- 0 until 10) {
      input(i)(j) = input(i)(j) match {
        case None =>
          count = count + 1
          Some(0)
        case Some(x) => Some(x)
      }
    }
    count
  }
}
