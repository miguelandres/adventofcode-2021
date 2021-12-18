package day17

import scala.io.Source
import scala.collection.mutable
import scala.util.control.Breaks._

object TrickShot extends App {

  case class Target(min: Int, max: Int) {
    def inRange(pos: Int): Boolean =
      pos >= min && pos <= max
  }

  case class Box(x: Target, y: Target) {
    def inRange(point: Point): Boolean =
      x.inRange(point.x) && y.inRange(point.y)

    def pastTheBox(point: Point): Boolean =
      point.x > x.max || point.y < y.min
  }

  case class Point(x: Int, y: Int)

  val box =
    Source
      .fromFile("input/d17p01.txt")
      .getLines()
      .toSeq
      .head
      .split(": ")(1)
      .split(", ")
      .map(_.split("=")(1).split("\\.\\.") match {
        case parts => Target(parts(0).toInt, parts(1).toInt)
      }) match {
      case parts => Box(parts(0), parts(1))
    }

  def positionX(xSpeed: Int, step: Int): Int =
    if (step <= xSpeed) {
      xSpeed * step - (step * (step - 1) / 2)
    } else {
      xSpeed * (xSpeed + 1) / 2
    }

  def positionY(ySpeed: Int, step: Int): Int =
    ySpeed * step - ((step * (step - 1)) / 2)

  def findPoints(xSpeed: Int, ySpeed: Int): Option[Seq[Point]] = {
    var step = 1
    var buf = mutable.Buffer[Point]()
    breakable {
      while (true) {
        val p = Point(positionX(xSpeed, step), positionY(ySpeed, step))
        if (box.pastTheBox(p))
          break
        buf.append(p)
        step = step + 1

      }
    }
    if (buf.exists(point => box.inRange(point))) { Some(buf.toSeq) }
    else { None }
  }

  val successfulTrajectories = (for (
    x <- 0 to 250;
    y <- -250 to 250
  ) yield (x, y))
    .flatMap { case (x, y) =>
      findPoints(x, y) match {
        case Some(points) => Some(Point(x, y), points)
        case None         => None
      }
    }

  println(successfulTrajectories.map { case (_, points) => points.map(_.y).max }.max);
  println(successfulTrajectories.map { case (velocity, _) => velocity }.distinct.size);
}
