package com.mdpeg

import org.parboiled2.{CharPredicate, Rule0, Rule1}

trait ListBlockParser extends PrimitiveRules {
  import CharPredicate._

  def list : Rule1[Block] = rule { unorderedList }//| orderedList }
  def unorderedList : Rule1[UnorderedList] = ???
  def orderedList : Rule1[OrderedList] = ???

  // aux rules

  def bullet : Rule0 = rule { nonIndentSpace ~ anyOf("+-*") ~ sp.+ }
  def enumerator : Rule0 = rule { nonIndentSpace ~ Digit.+ ~ "." ~ sp.+}

  //def bulletList = rule { bulletListTight }//| bulletListSparse }

  def h (s: Any) = {
    println(s.getClass.getSimpleName)
    val ss= s.asInstanceOf[Vector[Block]]
    UnorderedList(ss)
  }
  def bulletListTight = rule { (capture(bulletListItem) ~> ((x:Any,y:Any) => toRawMd(x, y))) ~ blankLine.* }
  //def bulletListSparse = rule { ??? }
  def bulletListItem = rule { !horizontalRule ~ bullet ~ listBlock ~ listContinuationBlock }

  // ToDo revisit when ordered list implemented
  //def listBlock = rule { anyLine ~ zeroOrMore(!(indent.? ~ (bulletListItem | orderedListItem))) ~ !blankLine ~ !(indent ~ (bullet | enumerator)) ~ indentedLine }
  def listBlock : Rule1[String] = rule { capture(anyLine) ~ zeroOrMore(!(indent.? ~ bulletListItem)) ~ !blankLine ~ !(indent ~ bullet) ~ indentedLine.? } // ToDo should the last indentedLine be indentedLine.?

  // push \0 need to handle raw markdown and split it into 2 chunks
  def listContinuationBlock : Rule1[Vector[String]]  = rule { (blankLine.+ ~ (indent ~ listBlock).+) ~> ((x:Any) => x.asInstanceOf[Vector[String]]) }// todo capture whole match? ; maybe use capture instead of push

//  def orderedListItem = rule { ??? }

  //aux func
  def toRawMd (x: Any, y:Any) = {
    println(x.getClass.getSimpleName)
    println(y.getClass.getSimpleName)
    Vector(Markdown(x.asInstanceOf[String]))}
}