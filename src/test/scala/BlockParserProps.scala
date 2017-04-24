import com.mdpeg.{BlockParser, Paragraph}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Properties}

object BlockParserProps extends Properties("String") {
  val inline = Gen.alphaNumStr
  property("Any alpha sequence followed by 'cr or crlf' is a paragraph of that sequence") = forAll(inline) { i =>
    (i != null && i != "") ==> {
    val term =
      s"""${i}
        |
        |""".stripMargin
    val parsed = new BlockParser(term).paragraph.run().get
    parsed == Paragraph(i)
    }
  }
}
