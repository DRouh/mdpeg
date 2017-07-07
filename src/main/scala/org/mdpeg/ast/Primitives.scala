package org.mdpeg.ast

final case class TexContent(u: String) extends AnyVal
final case class RawMarkdownContent(u: String) extends AnyVal
final case class CodeContent(u: String) extends AnyVal