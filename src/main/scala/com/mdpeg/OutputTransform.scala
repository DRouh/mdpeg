package com.mdpeg

object OutputTransform {
  type ErrorMessage = String
  type OutputContent = String

  def toHtml(astTree: Vector[Block]): Either[ErrorMessage, OutputContent] = ???
}
