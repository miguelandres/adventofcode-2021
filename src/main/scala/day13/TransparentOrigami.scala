package day13

import scala.io.Source

object TransparentOrigami extends App {

  val input = Source
    .fromFile("input/d13p01.txt")
    .getLines()
    .toSeq

  object Dot {
    def apply(str: String): Dot = {
      val parts = str.split(",")
      Dot(parts(0).toLong, parts(1).toLong)
    }
  }
  case class Dot(x: Long, y: Long)
  var (dots, instructions) = input.indexWhere(_.trim.isEmpty()) match {
    case index =>
      (
        input.slice(0, index).map(Dot(_)),
        input.slice(index + 1, input.length).map { str =>
          val parts = str.drop("fold along ".length()).split("=")
          parts(0) match {
            case "x" => Dot(parts(1).toLong, 0)
            case "y" => Dot(0, parts(1).toLong)
          }
        }
      )
  }

  def foldAlong(fold: Dot, dots: Set[Dot]): Set[Dot] = {
    val foldedCoordinates = fold match {
      case Dot(foldX, 0) =>
        dots.collect {
          case Dot(x, y) if x < foldX =>
            Dot(x, y)
          case Dot(x, y) if x > foldX =>
            Dot(foldX - (x - foldX), y)
        }
      case Dot(0, foldY) =>
        dots.collect {
          case Dot(x, y) if y < foldY =>
            Dot(x, y)
          case Dot(x, y) if y > foldY =>
            Dot(x, foldY - (y - foldY))
        }
    }
    val (minX, minY) =
      (Math.min(0L, foldedCoordinates.map(_.x).min), Math.min(0L, foldedCoordinates.map(_.y).min))
    foldedCoordinates.map { case Dot(x, y) => Dot(x - minX, y - minY) }
  }

  val endDots =
    instructions.foldLeft(dots.toSet) { case (currentDots, fold) =>
      val newdots = foldAlong(fold, currentDots)
      println(newdots.size)
      newdots
    }

  val (maxX, maxY) = (endDots.map(_.x).max, endDots.map(_.y).max)

  for (y <- 0L to maxY) {
    for (x <- 0L to maxX) {
      print(if (endDots.contains(Dot(x, y))) { '#' }
      else { '.' })
    }
    println()
  }

}
