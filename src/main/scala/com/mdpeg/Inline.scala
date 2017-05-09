package com.mdpeg

sealed trait Inline
case class Strong(inline: String) extends Inline //ToDo should be Inline instead of String
case class Italics(inline: String) extends Inline //ToDo should be Inline instead of String