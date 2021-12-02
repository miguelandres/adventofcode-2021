package day02

import scala.io.Source

object SubmarineControls extends App {
  val inputData = Source
    .fromFile("input/d02p01.txt")
    .getLines
    .map { line =>
      val parts = line.split(" ")
      parts(0) match {
        case "forward" => (parts(1).toLong, 0L)
        case "down"    => (0L, parts(1).toLong)
        case "up"      => (0L, -parts(1).toLong)
      }
    }
    .toSeq;

  val sums = inputData.foldLeft((0L, 0L)) {
      case ((accumHorizontal, accumDepth), (currHor, currDepth)) =>
        (accumHorizontal + currHor, accumDepth + currDepth)
    }
  Console.println(sums._1 * sums._2)

  val simulationResult = inputData.foldLeft((0L, 0L, 0L)) {
      case ((pos, aim, depth), (0, aimChange))  =>
        (pos, aim + aimChange, depth)
      case ((pos, aim, depth), (forward, 0))  =>
        (pos + forward, aim, depth + forward * aim)
    }

    Console.println(simulationResult._1 * simulationResult._3)
}
