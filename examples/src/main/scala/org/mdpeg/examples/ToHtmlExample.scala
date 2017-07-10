package org.mdpeg.examples
import java.io.PrintWriter

import org.mdpeg.MarkdownParser

object ToHtmlExample extends App {
  val inputFilePath = "insert you file path here"      // for example, input.md
  val outputHtmlFilePath = "insert you file path here" // for example, output.html
  val fileText = scala.io.Source.fromFile(inputFilePath).mkString

  //run markdown parser to get an AST
  MarkdownParser.parse(fileText) match {
    case Left(errors) => println(errors) // in this case something went wrong, meaning that parser failed to parse given document
    case Right(ast) =>
      //transform the AST to an HTML string
      MarkdownParser.transformToHtml(ast) match {
        case Left(error) => println(error) // error case
        case Right(html) =>
          //write the HTML to the output file
          new PrintWriter(outputHtmlFilePath) { write(html); close() }
      }
  }
}