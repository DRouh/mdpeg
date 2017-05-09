package com.mdpeg

sealed trait Inline
case class Strong(inline: Inline) extends Inline