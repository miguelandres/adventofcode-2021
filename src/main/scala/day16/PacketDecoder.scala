package day16

import scala.io.Source
import scala.collection.mutable
import scala.util.control.Breaks._

object PacketDecoder extends App {

  val map = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )
  val input = Source
    .fromFile("input/d16p01.txt")
    .getLines()
    .toSeq
    .head
    .map(c => map(c))
    .mkString

  val (bytesUsed, pkg) = parsePackage(input)

  println(pkg.versionSum)
  println(pkg.calculate)

  case class Package(
      version: Int,
      typeId: Int,
      literal: Option[Long] = None,
      subpackages: Seq[Package] = Seq()
  ) {
    def versionSum: Int =
      this.version + subpackages.map(_.versionSum).sum

    def calculate: Long =
      typeId match {

        case 0 => subpackages.map(_.calculate).sum
        case 1 => subpackages.map(_.calculate).foldLeft(1L) { case (accum, next) => accum * next }
        case 2 => subpackages.map(_.calculate).min
        case 3 => subpackages.map(_.calculate).max
        case 4 => literal.get
        case 5 =>
          if (subpackages(0).calculate > subpackages(1).calculate) { 1 }
          else { 0 }
        case 6 =>
          if (subpackages(0).calculate < subpackages(1).calculate) { 1 }
          else { 0 }
        case 7 =>
          if (subpackages(0).calculate == subpackages(1).calculate) { 1 }
          else { 0 }

      }
  }

  def parseLiteral(str: String): (Int, Long) = {
    var literalBitString = ""
    var currentSlice = str;
    var usedBits = 0
    breakable {
      while (true) {
        usedBits = usedBits + 5
        literalBitString = literalBitString + currentSlice.slice(1, 5)
        if (currentSlice(0) == '0')
          break;
        currentSlice = currentSlice.slice(5, currentSlice.length())
      }
    }
    (usedBits, java.lang.Long.parseLong(literalBitString, 2))
  }

  def parsePackagesInFullString(str: String): Seq[Package] = {
    var currentSlice = str
    var result = mutable.Buffer[Package]()

    while (currentSlice.nonEmpty) {
      val (bitsUsed, pkg) = parsePackage(currentSlice)
      result.append(pkg)
      currentSlice = currentSlice.slice(bitsUsed, currentSlice.length)
    }
    result.toSeq
  }
  def parsePackages(str: String, numberOfPackagesToParse: Int): (Int, Seq[Package]) = {
    var currentSlice: String = s"$str"
    var result = mutable.Buffer[Package]()
    var totalBitsUsed = 0
    var i = 0
    while (i < numberOfPackagesToParse) {
      val (bitsUsed, pkg) = parsePackage(currentSlice)
      totalBitsUsed = totalBitsUsed + bitsUsed
      result.append(pkg)
      currentSlice = currentSlice.slice(bitsUsed, currentSlice.length)
      i = i + 1
    }
    (totalBitsUsed, result.toSeq)
  }

  def parsePackage(str: String): (Int, Package) = {
    val version = Integer.parseInt(str.slice(0, 3), 2);
    val typeId = Integer.parseInt(str.slice(3, 6), 2);
    typeId match {
      case 4 =>
        val (bitsUsed, literal) = parseLiteral(str.slice(6, str.length()))
        (bitsUsed + 6, Package(version, typeId, literal = Some(literal)))
      case _ =>
        val contents = str.slice(6, str.length())
        contents(0) match {
          case '0' =>
            // total length in bits is the next 15 characters
            val lengthInBits = Integer.parseInt(contents.slice(1, 16), 2)
            (
              lengthInBits + 16 + 6,
              Package(
                version,
                typeId,
                subpackages = parsePackagesInFullString(contents.slice(16, 16 + lengthInBits))
              )
            )
          case '1' =>
            // Number of subpackages in the next 11 characters
            val numberOfPackagesToParse = Integer.parseInt(contents.slice(1, 12), 2)
            val (bitsUsed, subpackages) =
              parsePackages(contents.slice(12, contents.length()), numberOfPackagesToParse)
            (12 + 6 + bitsUsed, Package(version, typeId, subpackages = subpackages))
        }
    }
  }
}
