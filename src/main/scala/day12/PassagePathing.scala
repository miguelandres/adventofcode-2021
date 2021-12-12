package day12

import scala.collection.mutable.{HashMap, MultiMap, Set}
import scala.io.Source

object PassagePathing extends App {
  var edgeMap = new HashMap[String, Set[String]] with MultiMap[String, String]

  val edges = Source
    .fromFile("input/d12p01.txt")
    .getLines()
    .toSeq
    .flatMap { str =>
      val parts = str.split("-")
      Seq(parts(0) -> parts(1), parts(1) -> parts(0))
    }
    .foreach { case (v1, v2) => edgeMap.addBinding(v1, v2) }

  case class Path(
      path: Seq[String] = Seq("start"),
      blockedNeighbors: Set[String] = Set("start"),
      repeatVisitedSmallCave: Boolean = false
  ) {
    def isDone = path.last == "end"
    def last = path.last
  }

  var paths = Seq(Path())

  while (paths.exists(!_.isDone)) {
    paths = paths.flatMap { path =>
      if (path.isDone) { Seq(path) }
      else {
        (edgeMap(path.last).clone()).collect {
          case next
              if next != "start" && path.blockedNeighbors
                .contains(next) && !path.repeatVisitedSmallCave =>
            path.copy(path = path.path :+ next, repeatVisitedSmallCave = true)
          case next
              if next.forall(_.isLower) && !path.blockedNeighbors
                .contains(next) =>
            path.copy(path = path.path :+ next, blockedNeighbors = path.blockedNeighbors + next)
          case next if next.exists(!_.isLower) =>
            path.copy(path = path.path :+ next)
        }
      }
    }
  }

  println(paths.size)
}
