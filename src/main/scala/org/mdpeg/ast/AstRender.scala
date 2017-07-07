package org.mdpeg.ast

trait AstRender[T] {
  def render(ast: Ast): T
}

object AstRender {
  implicit object SimpleHTMLRender extends AstRender[String] {
    def render(ast: Ast): String = ???
  }
}