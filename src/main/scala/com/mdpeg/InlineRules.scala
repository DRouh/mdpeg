package com.mdpeg

import org.parboiled2._

trait InlineRules {
  this: Parser with PrimitiveRules =>

  def label : Rule1[String] = rule("[" ~ capture((!"]" ~ inline).+) ~ "]") // [label]
  def title : Rule1[String] = rule { // 'title' or "title" for reference
    "\"" ~ capture((!"\"" ~ !nl ~ anyChar).*) ~ "\"" |
    "'"  ~ capture((!"'" ~ !nl   ~ anyChar).*) ~ "'"
  }
}
