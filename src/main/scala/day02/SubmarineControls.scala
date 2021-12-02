package day02

import scala.io.Source

object SubmarineControls extends App {
  val inputData = Source
    .fromFile("input/d02p01.txt")
    .getLines
    .map { line =>
      val parts = line.split(" ")
      parts(0) match {
        case "forward" => (parts(1).toInt, 0)
        case "down"    => (0, parts(1).toInt)
        case "up"      => (0, -parts(1).toInt)
      }
    }
    .toSeq;

  val sums = inputData.foldLeft((0, 0)) {
    case ((accumHorizontal, accumDepth), (currHor, currDepth)) =>
      (accumHorizontal + currHor, accumDepth + currDepth)
  }
  Console.println(sums._1 * sums._2)
}
