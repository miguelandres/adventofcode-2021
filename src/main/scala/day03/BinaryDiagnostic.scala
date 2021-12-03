package day03

import scala.io.Source

object BinaryDiagnostic extends App {
  val inputData = Source
    .fromFile("input/d03p01.txt")
    .getLines
    .toSeq

  def generateCounts(list: Seq[String]): Array[(Int, Int)] =
    list.foldLeft((0 until list.head.length()).toSeq.map(_ => (0, 0)).toArray) {
      case (arrCounts, binaryString) =>
        for (i <- 0 until binaryString.length()) {
          if (binaryString(i) == '0') {
            arrCounts(i) = (arrCounts(i)._1 + 1, arrCounts(i)._2)
          } else { arrCounts(i) = (arrCounts(i)._1, arrCounts(i)._2 + 1) }
        }
        arrCounts
    }
  val counts = generateCounts(inputData)

  val gamma = counts
    .map { case (zeroes, ones) =>
      if (zeroes > ones) { '0' }
      else { '1' }
    }
    .mkString("")

  val epsilon = counts
    .map { case (zeroes, ones) =>
      if (zeroes < ones) { '0' }
      else { '1' }
    }
    .mkString("")

  Console.println(Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2))

  def bitStatusMatches(status: Boolean, index: Int)(str: String): Boolean =
    (str(index) == '1') == status

  def statusToKeep(counts: (Int, Int)): Boolean =
    counts match {
      case (zeroes, ones) => zeroes <= ones
    }

  def findRating(invert: Boolean): Int = {
    var candidates = inputData
    var index = 0
    while (candidates.size > 1) {
      candidates = invert match {
        case false =>
          candidates.filter(
            bitStatusMatches(statusToKeep(generateCounts(candidates)(index)), index)
          )
        case true =>
          candidates.filterNot(
            bitStatusMatches(statusToKeep(generateCounts(candidates)(index)), index)
          )
      }
      index = 1 + index
    }
    Integer.parseInt(candidates.head, 2)
  }

  val oxygen = findRating(false)
  val co2 = findRating(true)
  Console.println(s"$oxygen*$co2= ${oxygen * co2}")
}
