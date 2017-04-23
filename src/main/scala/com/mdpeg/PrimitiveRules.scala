package com.mdpeg

import org.parboiled2._

trait PrimitiveRules extends Parser {
  import CharPredicate._
  def anyLine = rule { !nl ~ !EOI ~ inline.+ ~ (nl | "") }
  def endLine = rule { sp.? ~ nl ~ !blankLine ~ !EOI }

  def nonIndentSpace: Rule1[String] = {
    // only to facilitate type inference,
    // i.e to support optional(A,B) where B returned when A is None
    @inline
    def h(x: AnyRef): String = x match {
      case x: Option[String @unchecked] => x.getOrElse("")
      case _ => ""
    }

    rule {
      capture("   " | "  " | " ").? ~> (h(_))
    }
  }

  def inline          = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()'") }
  def blankLine       = rule { sp.* ~ nl }
  def punctuationChar = rule { anyOf(":;,.?!-") }
  def nl              = rule { "\r\n" | "\r" | "\n" }
  def spOs            = rule { sp.* }
  def sp              = rule { " " | "\t" }
}
