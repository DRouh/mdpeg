package org.mdpeg.parsers

import org.mdpeg.ast._
import org.parboiled2._

private[mdpeg] class BlockParser(val input: ParserInput) extends Parser
  with PrimitiveRules
  with BlockRules
  with ListBlockRules
  with MultilineTablesRules
  with InlineRules {
  def InputLine: Rule1[Seq[Block]] = rule(block.+ ~ EOI)

  def block: Rule1[Block] = rule { blockQuote | tex | verbatim | reference | heading | list | multiTable | horizontalRule | paragraph | plain  }
}