package day10

import scala.io.Source
import scala.collection.mutable.Buffer

object SyntaxScoring extends App {

  val corruptScores = Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)
  val autocorrectScores = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)

  val input = Source.fromFile("input/d10p01.txt").getLines().toSeq

  val runningScores = input
    .map { str =>
      val stack = Buffer[Char]()
      str.find {
        case '(' =>
          stack.prepend(')')
          false
        case '[' =>
          stack.prepend(']')
          false
        case '{' =>
          stack.prepend('}')
          false
        case '<' =>
          stack.prepend('>')
          false
        case x =>
          stack.remove(0) != x
      } match {
        case None =>
          (0L, stack.foldLeft(0L) { case (score, c) => score * 5 + autocorrectScores(c) })
        case Some(x) => (corruptScores(x), 0L)
      }

    }

  val corruptScoreSum = runningScores.map(_._1).sum
  val computedAutoCorrectScores = runningScores.map(_._2).filter(_ > 0L).sorted
  val medianAutoCorrectScore = computedAutoCorrectScores(computedAutoCorrectScores.size / 2)
  println(corruptScoreSum)
  println(medianAutoCorrectScore)

}
