package org.mdpeg.ast

final case class Ast(u: Vector[Vector[Block]]) extends AnyVal {
  def map[B](f: Vector[Block] => B): Vector[B] = u.map(f)

  def flatten: Vector[Block] = u.flatten
}