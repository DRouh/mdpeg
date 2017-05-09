package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>

  def inline : Rule0 = rule(strong)
  def strong : Rule0 = rule(strongStarred|strongUnderlined)
  def strongStarred : Rule0  = ???
  def strongUnderlined  : Rule0 = ???

  def twoStar  :Rule0 = rule("**" ~ !twoStar)
  def twoUnder :Rule0 = rule("__" ~ !twoUnder)


  def label : Rule1[String] = rule("[" ~ capture((!"]" ~ inlineChar).+) ~ "]") // [label]
  def title : Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
    "'"  ~ capture((!"'" ~ !nl   ~ anyChar).*) ~ "'"
  }
}
