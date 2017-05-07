import com.mdpeg.{transposeAnyShape, trimEnd}

import scala.annotation.tailrec
import scala.compat.Platform.EOL
import scala.collection.immutable.::

val widths = "----------------      ------------------------------------------------"

val rows: List[List[String]] =
  List(
    List(
//    "----------------       ------------------------------------------------"
      ".It 1  first la1234567 8is a long established fact that 1",
      ".It 2  second la123456 78is a long established fact that 2")
  )

def listSplit(pos: List[Int], str: String): List[String] = {
  val (rest, result) = pos.foldRight(z = (str, List[String]())) {
    case (curr, (s, res)) =>
      val (rest, split) = s.splitAt(curr)
      (rest, split :: res)
  }
  rest :: result
}

val indexes = " -".r.findAllMatchIn(widths).map(_.start+2).toList
val cells = rows.map(_.map(s => listSplit(indexes, s)))
//val cellsN: List[List[List[String]]] = rows.
//  map(_.map(_.zip(widths).toList)).
//  map(tuples =>
//    tuples.
//      foldLeft(List.empty[List[String]]) {
//        case (acc, currentLine: List[(Char, Char)]) =>
//          currentLine.foldLeft(List.empty[String]) {
//            case (acc1, currentLine) =>
//              (acc1, cl) match {
//                case (x :: xs, (_, '-')::(c1, ' ')::_) => (x + c1) :: xs
//                case (x :: xs, (_, '-')::(c1, '-')::_) => (x + c1) :: xs
//                case (x :: xs, (c, '-')::(c1, '-')::_) => (x + c+ c1) :: xs
//                case (Nil, (c, '-')::_) => c.toString :: Nil
//                case _ => Nil
//              }
//          }.filter(_ != "").reverse :: acc
//      })
//
//val cells: Vector[List[String]] =
//  transposeAnyShape(
//    cellsN.
//    map(
//      transposeAnyShape(_).
//      map(strings => trimEnd(strings.reverse.reduce((s1, s2) => trimEnd(s1) + EOL + s2))))).
//  toVector



