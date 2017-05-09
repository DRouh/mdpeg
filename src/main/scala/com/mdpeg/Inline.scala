package com.mdpeg

sealed trait Inline
case class Strong(inline: Seq[Inline]) extends Inline
case class Text(inline: String) extends Inline
case object Space extends Inline