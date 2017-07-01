package org.mdpeg

//reference target
sealed trait Target
final case class Src(uri:String, title: Option[String]) extends Target
final case class Ref(label:Seq[Inline], ref: String) extends Target
final case object ShortcutRef extends Target