package org.mdpeg.util

private[mdpeg] class Pipe[A](a: A) {
  def |>[B](f: A => B) = f(a)
}

private[mdpeg] object Pipe {
  def apply[A](v: A) = new Pipe(v)
}