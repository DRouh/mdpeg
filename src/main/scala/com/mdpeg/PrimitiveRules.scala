package com.mdpeg

import org.parboiled2._

trait PrimitiveRules extends Parser {
  import CharPredicate._
  def indentedLine : Rule0 = rule { indent ~ anyLine }
  def anyLine : Rule0 = rule { !nl ~ !EOI ~ inline.+ ~ (nl | "") }
  def endLine : Rule0 = rule { sp.? ~ nl ~ !blankLine ~ !EOI }

  def indent : Rule0 = rule { "\t" | "    " }

  def nonIndentSpace: Rule0 = {
    // only to facilitate type inference,
    // i.e to support optional(A,B) where B returned when A is None
    @inline
    def h(x: AnyRef): String = x match {
      case x: Option[String @unchecked] => x.getOrElse("")
      case _ => ""
    }

    rule { "   " | "  " | " " | ""  }
  }

  def inline          : Rule0 = rule { AlphaNum | sp | punctuationChar | anyOf("_\"{}()'") }
  def blankLine       : Rule0 = rule { sp.* ~ nl }
  def punctuationChar : Rule0 = rule { anyOf(":;,.?!-") }
  def nl              : Rule0 = rule { "\r\n" | "\r" | "\n" }
  def spOs            : Rule0 = rule { sp.* }
  def sp              : Rule0 = rule { " " | "\t" }
}
