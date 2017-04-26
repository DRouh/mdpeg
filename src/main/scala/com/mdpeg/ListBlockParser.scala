package com.mdpeg

import org.parboiled2.{CharPredicate, Rule0, Rule1}

trait ListBlockParser extends PrimitiveRules {
  import CharPredicate._

  def list : Rule1[Block] = rule { unorderedList | orderedList }
  def unorderedList : Rule1[UnorderedList] = ???
  def orderedList : Rule1[OrderedList] = ???


  //aux rules
  def bullet : Rule0 = rule { nonIndentSpace ~ anyOf("+-*") ~ sp }
}
