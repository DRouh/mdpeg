package com

package object mdpeg {
  type MultilineTableColumn = Vector[MultilineTableCell]
  type MultilineTableRow = Vector[MultilineTableCell]

  /**
    * Transposes a list. Doesn't fail on lists of different sizes (unlike built-in function)
    * @param xs list to transpose
    * @tparam A element type
    * @return transposed list
    */
  def transposeAnyShape[A](xs: List[List[A]]): List[List[A]] =
    xs.filter(_.nonEmpty) match {
      case Nil => Nil
      case ys: List[List[A]] => (ys map (_ head)) :: transposeAnyShape(ys map (_ tail))
  }

  /**
    * Trims the end of the string
    * @param s string to trim
    * @return end-trimmed string
    */
  def trimEnd(s: String): String = s.
    reverseIterator.
    dropWhile(_ == ' ').
    toSeq.
    reverseIterator.
    mkString
}
