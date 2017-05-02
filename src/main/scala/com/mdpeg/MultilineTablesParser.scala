package com.mdpeg

import org.parboiled2._
trait MultilineTablesParser extends PrimitiveRules {
  this: Parser =>
  import CharPredicate._

/* ToDo think about
* 1. capturing relative width of columns
* 2. capturing table caption
* 3. would it be easier to process row by row instead of immediately trying to split everything into cells?
* 4. rows are separated by blank lines
*/
}
