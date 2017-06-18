package com.mdpeg

import org.parboiled2._

trait ListBlockRules {
  this: Parser with PrimitiveRules =>

  import CharPredicate._

  private def anyLineList: Rule0 = rule((!nl ~ !EOI ~ ANY).* ~ (nl | ""))

  private def anyCharList: Rule0 = rule(anyChar | backTick)

  def list: Rule1[Block] = rule {
    unorderedList | orderedList
  }

  def unorderedList: Rule1[UnorderedList] = rule {
    bulletListTight | bulletListSparse
  }

  def orderedList: Rule1[OrderedList] = rule {
    orderedListTight | orderedListSparse
  }

  // aux rules
  def bullet: Rule0 = rule {
    nonIndentSpace ~ anyOf("+-*") ~ sp.+
  }

  def enumerator: Rule0 = rule {
    nonIndentSpace ~ Digit.+ ~ "." ~ sp.+
  }

  // unordered list
  def bulletListTight: Rule1[UnorderedList] =
    rule((bulletListItem.+ ~> toUnorderedList _) ~ blankLine.* ~ !bulletListSparse)

  def bulletListSparse: Rule1[UnorderedList] = rule((bulletListItem ~ blankLine.*).+ ~> (toUnorderedList(_)))

  def bulletListItem: Rule1[Vector[String]] = {
    def listStart = rule(!horizontalRule ~ bullet)

    def listRest = rule(listContinuationBlock.*)

    def combinePreCont(pre: String, cont: String): Vector[String] = {
      (pre, cont) match {
        case ("", "") => Vector("")
        case ("", s2) => Vector(trimEndWithEnding(s2))
        case (s1, "") => Vector(trimEndWithEnding(s1))
        case (s1, s2) => Vector(trimEndWithEnding(s1 + "\0" + s2)) // this's needed to process nested lists
      }
    }

    rule(listStart ~ capture(listBlock) ~ capture(listRest) ~> (combinePreCont(_, _)))
  }

  // ordered list
  def orderedListTight: Rule1[OrderedList] =
    rule((orderedListItem.+ ~> (toOrderedList(_))) ~ blankLine.* ~ !orderedListSparse)

  def orderedListSparse: Rule1[OrderedList] =
    rule((orderedListItem ~ blankLine.*).+ ~> (toOrderedList(_)))

  def orderedListItem: Rule1[Vector[String]] = {
    def listStart = rule(enumerator)

    def listRest = rule(listContinuationBlock.*)

    def ff(x: String, y: String): Vector[String] = {
      //println(s"bulletListItem:Vector($x) ++ ($y)")
      Vector(x + y)
    }

    rule(listStart ~ capture(listBlock) ~ capture(listRest) ~> ((x: String, y: String) => ff(x, y)))
  }

  // aux list rules
  def listBlock: Rule0 = {
    rule(anyLineList ~ (!(listIndent.? ~ (bulletListItem | orderedListItem)) ~
      !blankLine ~ !(listIndent ~ (bullet | enumerator)) ~ optionallyIndentedLine).*)
  }

  // ToDo improve continuation to handle inner lists
  def listContinuationBlock: Rule0 = {
    rule((blankLine.+ | MATCH) ~ (listIndent ~ listBlock).+)
  }

  // aux rules
  private def listIndent = rule(indent | halfIndent)

  private def halfIndent = rule("  ")

  // aux funcs
  private def toUnorderedList(x: Seq[Vector[String]]) = UnorderedList(x.flatten.map(Markdown).toVector)

  private def toOrderedList(x: Seq[Vector[String]]) = OrderedList(x.flatten.map(Markdown).toVector)
}