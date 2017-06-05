package com.mdpeg

class Pipe[A](a: A) {
  def |>[B](f: A => B) = f(a)
}

object Pipe {
  def apply[A](v: A) = new Pipe(v)
}