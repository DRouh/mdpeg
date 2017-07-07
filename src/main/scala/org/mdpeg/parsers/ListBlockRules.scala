package org.mdpeg.parsers

import org.mdpeg._
import org.mdpeg.ast._
import org.parboiled2._

private[mdpeg] trait ListBlockRules {
  this: Parser with PrimitiveRules =>

  import CharPredicate._

  def list: Rule1[Block] = rule(unorderedList | orderedList)

  def unorderedList: Rule1[UnorderedList] = rule(bulletListTight | bulletListSparse)

  def orderedList: Rule1[OrderedList] = rule(orderedListTight | orderedListSparse)

  def bullet: Rule0 = rule(nonIndentSpace ~ anyOf("+-*") ~ sp.+)

  def enumerator: Rule0 = rule(nonIndentSpace ~ Digit.+ ~ "." ~ sp.+)

  def bulletListTight: Rule1[UnorderedList] = rule((bulletListItem.+ ~> toUnorderedList _) ~ blankLine.* ~ !bulletListSparse)

  def orderedListTight: Rule1[OrderedList] = rule((orderedListItem.+ ~> toOrderedList _) ~ blankLine.* ~ !orderedListSparse)

  def bulletListSparse: Rule1[UnorderedList] = rule((bulletListItem ~ blankLine.*).+ ~> (toUnorderedList(_)))

  def orderedListSparse: Rule1[OrderedList] = rule((orderedListItem ~ blankLine.*).+ ~> (toOrderedList(_)))

  def bulletListItem: Rule1[Vector[String]] = rule(!horizontalRule ~ bullet ~ capture(listBlock) ~ capture(listContinuationBlock.*) ~> (combinePreCont(_, _)))

  def orderedListItem: Rule1[Vector[String]] = rule(enumerator ~ capture(listBlock) ~ capture(listContinuationBlock.*) ~> (combinePreCont(_, _)))

  private def listBlock: Rule0 = rule {
    anyLineList ~ (!(listIndent.? ~ (bulletListItem | orderedListItem)) ~ !blankLine ~ !(listIndent ~ (bullet | enumerator)) ~ optionallyIndentedLine).*
  }

  // ToDo improve continuation to handle inner lists
  private def listContinuationBlock: Rule0 = rule((blankLine.+ | MATCH) ~ (listIndent ~ listBlock).+)

  private def anyLineList: Rule0 = rule((!nl ~ !EOI ~ ANY).* ~ (nl | ""))

  private def optionallyIndentedLine: Rule0 = rule(listIndent.? ~ anyLine)

  private def listIndent: Rule0 = rule(indent | halfIndent)

  private def halfIndent: Rule0 = rule("  ")

  private def toUnorderedList(x: Seq[Vector[String]]) = UnorderedList(x.flatten.map(c => Markdown(RawMarkdownContent(c))).toVector)

  private def toOrderedList(x: Seq[Vector[String]]) = OrderedList(x.flatten.map(c => Markdown(RawMarkdownContent(c))).toVector)

  private def combinePreCont(pre: String, cont: String): Vector[String] = (pre, cont) match {
    case ("", "") => Vector("")
    case ("", s2) => Vector(trimEndWithEnding(s2))
    case (s1, "") => Vector(trimEndWithEnding(s1))
    case (s1, s2) => Vector(trimEndWithEnding(s1 + "\u0000" + s2)) // this's needed to process nested lists
  }
}