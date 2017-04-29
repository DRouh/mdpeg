package com.mdpeg

import org.parboiled2.{CharPredicate, Rule0, Rule1}

trait ListBlockParser extends PrimitiveRules {
  import CharPredicate._

  def list: Rule1[Block] = rule { unorderedList }
  def unorderedList: Rule1[UnorderedList] = ???
  def orderedList: Rule1[OrderedList] = ???

  // aux rules
  def bullet: Rule0     = rule { nonIndentSpace ~ anyOf("+-*") ~ sp.+ }
  def enumerator: Rule0 = rule { nonIndentSpace ~ Digit.+ ~ "." ~ sp.+ }

  def bulletListTight       = rule { (capture(bulletListItem.+) ~> ((x: Any, y: Any) => toRawMd(x, y))) ~ blankLine.* ~ !bulletListSparse}
  def bulletListSparse      = rule { capture((bulletListItem ~ blankLine.*).+) ~> ((x: Any, y: Any) => toRawMd(x, y)) }
  def bulletListItem        = rule { capture(!horizontalRule ~ bullet ~ listBlock ~ listContinuationBlock.*) ~> ((x: Any) => x) }
  def listBlock: Rule0      = rule { anyLine ~ zeroOrMore(!indent.? ~ !bulletListItem ~ !blankLine ~ !indent ~ !bullet ~ indentedLine.?) }
  def listContinuationBlock = rule { blankLine.+ ~ (indent ~ listBlock).+ }

  //aux func
  def toRawMd(x: Any, y: Any) = {
    println("x:" + x)
    println("y:" + y)
    Vector(Markdown(x.asInstanceOf[Vector[String]]))
  }
}
