package day05

import scala.io.Source
import scala.collection.mutable
import Math.min
import Math.max

object HydrothermalVenture extends App {

  val lines: Seq[Line] =
    Source
      .fromFile("input/d05p01.txt")
      .getLines
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(str => Line(str))
      .toSeq

  val allPoints = lines.flatMap(_.allPoints).toSeq
  val (maxX, maxY) = lines.foldLeft((0, 0)) { case ((maxX, maxY), line) =>
    (max(maxX, max(line.p1._1, line.p2._1)), max(maxY, max(line.p1._2, line.p2._2)))
  }
  val pointMap = mutable.TreeMap[(Int, Int), Int]()
  for (point <- allPoints) {
    pointMap.put(
      point,
      pointMap.getOrElse(point, 0) + 1
    )
  }

  // for (j <- 0 to maxY) {
  //   println(
  //     (0 to maxX)
  //       .map(i =>
  //         pointMap.get((i, j)) match {
  //           case None    => "."
  //           case Some(x) => x.toString()
  //         }
  //       )
  //       .mkString("")
  //   )
  // }

  //println(lines)
  //println(allPoints)
  //println(pointMap)
  val dangerousPointCount = pointMap.values.count(_ > 1)
  println(dangerousPointCount)

  object Line {
    def apply(str: String): Line = {
      val coordinates = str.split(" *-> *").flatMap(_.split(",").map(_.toInt))
      new Line((coordinates(0), coordinates(1)), (coordinates(2), coordinates(3)))
    }
  }
  class Line(val p1: (Int, Int), val p2: (Int, Int)) {
    def isVertical: Boolean = p1._1 == p2._1
    def isHorizontal: Boolean = p1._2 == p2._2
    def allPoints: Seq[(Int, Int)] =
      if (isHorizontal) {
        (min(p1._1, p2._1) to max(p1._1, p2._1))
          .map(x => (x, p1._2))
          .toSeq
      } else if (isVertical) {
        (min(p1._2, p2._2) to max(p1._2, p2._2))
          .map(y => (p1._1, y))
          .toSeq
      } else {
        var (r1, r2) = (
          (min(p1._1, p2._1) to max(p1._1, p2._1)).toSeq,
          (min(p1._2, p2._2) to max(p1._2, p2._2)).toSeq
        )
        var (coordsx, coordsy) = if (p1._1 < p2._1 && p1._2 < p2._2) {
          (r1, r2)
        } else if (p1._1 > p2._1 && p1._2 < p2._2) {
          (r1.reverse, r2)
        } else if (p1._1 > p2._1 && p1._2 > p2._2) {
          (r1.reverse, r2.reverse)
        } else {
          (r1, r2.reverse)
        }
        (0 until r1.length).map(i => (coordsx(i), coordsy(i)))

      }
    override def toString: String =
      s"[${if (isHorizontal) "horizontal " else if (isVertical) "vertical " else ""}$p1 -> $p2]"
  }
}
