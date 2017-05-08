import com.mdpeg._
object ExpectedTestResults {
  val unorderedList = UnorderedList(Vector(
    Markdown("""1st block - It is a long established fact that a reader will be distracted by the readable content of a
               |  page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less
               |  normal distribution of letters, as opposed to using 'Content here, content here',
               |""".stripMargin),
    Markdown("""2nd list block - editors now use Lorem Ipsum as their default model text, and a search for
               |  'lorem ipsum' will uncover many web sites still in their infancy. Various versions
               |  injected humour and the like).
               |  There are many variations of passages of Lorem Ipsum available, but the majority have
               |""".stripMargin),
    Markdown(
      """3rd list block - If you are going to use a passage of Lorem Ipsum, you need to be
        |""".stripMargin),
    Markdown("""4th list block - sure there isn't anything embarrassing hidden in the middle
               |  of text. All the Lorem Ipsum generators on the Internet tend to r
               |""".stripMargin)))
  val orderedList = OrderedList(Vector(
    Markdown("""This is a first item of an ordered list
               |    1. and this is a first sub item of a first item of an ordered list
               |    2. and this is a second sub item of a first item of an ordered list
               |""".stripMargin),
    Markdown("""And, finally, this is a second item of an ordered list
               |""".stripMargin)))
  val complexTable = MultilineTableBlock(
    Vector(25.0f, 75.0f),
    Some(MultilineTableCaption(Markdown("This is a table caption\\label{table:table_lable_name}"))),
    Some(Vector(
      MultilineTableCell(Markdown("This header is longer than sep")),
      MultilineTableCell(Markdown("And this header is also longer than this separator")))),
    Vector(
      Vector(
        MultilineTableCell(Markdown("**Why do we use it?**")),
        MultilineTableCell(Markdown("""There-are
                                      |""".stripMargin)),
        MultilineTableCell(Markdown("**Where can I get some?**")),
        MultilineTableCell(Markdown("""dummy
                                      |""".stripMargin)),
        MultilineTableCell(Markdown("text")),
        MultilineTableCell(Markdown("printing")),
        MultilineTableCell(Markdown("**Where does it come from?**")),
        MultilineTableCell(Markdown("""leap-into
                                      |""".stripMargin)),
        MultilineTableCell(Markdown("""variations-join
                                      |
                                      |""".stripMargin)),
        MultilineTableCell(Markdown("**What is Lorem Ipsum?**")),
        MultilineTableCell(Markdown("""Lorem
                                      |""".stripMargin)),
        MultilineTableCell(Markdown("""anything
                                      |""".stripMargin))),
      Vector(
        MultilineTableCell(Markdown("""It is a long established fact that a reader will be
                                      |distracted by the readable content of a page when looking at""".stripMargin)),
        MultilineTableCell(Markdown("""It uses a dictionary of over
                                      |Lorem Ipsum which looks reasonable""".stripMargin)),
        MultilineTableCell(Markdown("The generated Lorem Ipsum is")),
        MultilineTableCell(Markdown("or non-characteristic words etc")),
        MultilineTableCell(Markdown("""It uses a dictionary of over 200
                                      |you need to be sure there""".stripMargin)),
        MultilineTableCell(Markdown("""anything embarrassing hidden
                                      |you need to be sure there isn't
                                      |within this period""".stripMargin)),
        MultilineTableCell(Markdown(""""There are many variations of passages.
                                      |*randomised words which : 1597 z*""".stripMargin)),
        MultilineTableCell(Markdown("""but the majority have suffered alteration.
                                      |*to use a passage: "" (empty string)*""".stripMargin)))))
}
