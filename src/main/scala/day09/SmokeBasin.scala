package day09

import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.Buffer

object SmokeBasin extends App {
  val input = Source.fromFile("input/d09p01.txt").getLines().toSeq

  val neighbors = Seq((-1, 0), (1, 0), (0, 1), (0, -1))
  val rangeX = 0 until input.size
  val rangeY = 0 until input.head.size
  var riskLevels = 0
  var queue: Buffer[((Int, Int), (Int, Int))] = Buffer()
  var ownerMatrix: Array[Array[Option[(Int, Int)]]] =
    Array.fill(rangeX.size)(Array.fill(rangeY.size)(None))
  var sizeMap: mutable.Map[(Int, Int), Int] = mutable.Map()
  for (i <- rangeX; j <- rangeY) {
    if (isLowPoint(i, j)) {
      mark((i, j), (i, j))
      riskLevels = riskLevels + input(i)(j) - '0' + 1
    }
  }

  println(riskLevels)

  while (queue.nonEmpty) {
    val (point, owner) = queue.remove(0)
    mark(point, owner)
  }

  val areasSortedBySize: Seq[Int] =
    sizeMap.map { case ((x, y), size) => size }.toSeq.sorted.reverse

  println(areasSortedBySize)
  println(areasSortedBySize(0) * areasSortedBySize(1) * areasSortedBySize(2))

  def isLowPoint(x: Int, y: Int): Boolean = {
    val height: Int = (input(x)(y) - '0').toInt
    neighbors.map { case (deltaX, deltaY) => (x + deltaX, y + deltaY) }.forall {
      case (newX, newY) if rangeX.contains(newX) && rangeY.contains(newY) =>
        height < (input(newX)(newY) - '0').toInt
      case _ => true
    }
  }

  def mark(point: (Int, Int), owner: (Int, Int)): Unit =
    point match {
      case (x, y)
          if rangeX.contains(x) && rangeY
            .contains(y) && ownerMatrix(x)(y).isEmpty && input(x)(y) != '9' =>
        ownerMatrix(x)(y) = Some(owner)
        sizeMap.put(owner, sizeMap.getOrElse(owner, 0) + 1)
        neighbors.foreach { case (i, j) => queue.append(((x + i, y + j), owner)) }
      case _ => ()
    }

}
