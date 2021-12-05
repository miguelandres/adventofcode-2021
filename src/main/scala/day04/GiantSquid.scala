package day04

import scala.io.Source
import scala.collection.mutable
import util.control.Breaks._

object GiantSquid extends App {
  val file: Seq[String] =
    Source.fromFile("input/d04p01.txt").getLines().map(_.trim()).filter(_.nonEmpty).toSeq
  val numbersPlayed: Array[Int] = file.head.split(",").map(_.toInt)

  val boards: Seq[Board] = getBoards(file.tail.iterator)
  breakable {
    for (number <- numbersPlayed; board <- boards) {
      board.play(number) match {
        case None => ()
        case Some(value) =>
          println(s"score: $value")

      }
    }
  }

  def getBoards(input: Iterator[String]): Seq[Board] = {
    var seq = Seq[Board]()
    while (input.hasNext) {
      seq = seq ++ Seq(Board(input))
    }
    seq
  }

  object Board {
    def apply(input: Iterator[String]): Board = {
      var rows = mutable.Seq[mutable.Seq[(Int, Boolean)]]()
      var columns: mutable.Seq[mutable.Seq[(Int, Boolean)]] =
        mutable.Seq.from(((0 to 5).map(_ => mutable.Seq[(Int, Boolean)]())))
      var numbers = mutable.Map[Int, (Int, Int)]()
      for (i <- 0 to 4) {
        val currentRow =
          mutable.Seq.from(input.next().trim().split(" +").map(str => (str.trim.toInt, false)))
        rows = rows ++ Seq(currentRow)
        for (j <- 0 to 4) {
          columns(j) = columns(j) ++ Seq(currentRow(j))
          numbers(currentRow(j)._1) = (i, j)

        }
      }
      println("read board")
      new Board(rows, columns, numbers)
    }
  }

  class Board(
      var row: mutable.Seq[mutable.Seq[(Int, Boolean)]],
      var column: mutable.Seq[mutable.Seq[(Int, Boolean)]],
      var numbers: mutable.Map[Int, (Int, Int)],
      var done: Boolean = false
  ) {
    def play(number: Int): Option[Int] = {
      if (done) return None
      numbers.get(number) match {
        case None => None
        case Some((x, y)) =>
          row(x)(y) = (number, true)
          column(y)(x) = (number, true)
          if (row(x).forall(_._2) || column(y).forall(_._2)) {
            done = true
            val sumUnmarked = row
              .map(_.map {
                case (number, false) => number
                case _               => 0
              }.sum)
              .sum
            Some(number * sumUnmarked)
          } else {
            None
          }
      }
    }

  }
}
