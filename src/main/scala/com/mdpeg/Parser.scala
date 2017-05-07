package com.mdpeg

import org.parboiled2.{ErrorFormatter, ParseError}

import scala.io.Source
import scala.util.{Failure, Success}

object Parser extends App {
  val fileText = Source.fromInputStream(getClass.getResourceAsStream("multiline table_complex.md")).mkString
  val parser = new BlockParser(fileText)
  parser.InputLine.run() match {
    case Success(node) => println(node)
    case Failure(e: ParseError) =>
      println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
    case Failure(e) =>
      throw e
  }
}