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
  def bulletListItem = {
    def listStart = rule(!horizontalRule ~ bullet)
    def item      = rule(capture(listBlock) ~> ((_: String) => _))
    def listRest  = rule(listContinuationBlock.*)

    def f(x:String) = {
      println(s"f(x):x${x.getClass.getSimpleName}=$x")
      x
    }
    rule(listStart ~ capture(listBlock) ~ listRest ~> (f(_)))
  }
  def listBlock: Rule0 = rule(anyLine ~ zeroOrMore(!indent.? ~ !bulletListItem ~ !blankLine ~ !indent ~ !bullet ~ indentedLine.?))
  def listContinuationBlock: Rule0 = rule(blankLine.+ ~ (indent ~ listBlock).+)

  //aux func
  def toRawMd(x: Any, y: Any) = {
    println(s"x:${x.getClass.getSimpleName} = $x")
    println(s"y:${y.getClass.getSimpleName} = $y:")
    Vector(Markdown(x.asInstanceOf[Vector[String]]))
  }
}
