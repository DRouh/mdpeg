import org.mdpeg.{ReferenceBlock, _}

object ExpectedTestResults {
  val headingOne = HeadingBlock(1,Vector(Text("Heading"), Space, Text("One")))
  val headingTwo = HeadingBlock(2,Vector(Text("Heading"), Space, Text("Two")))

  val paragraphTwo = Paragraph(Vector(Text("But"), Space, Text("I"), Space, Text("must"), Space, Text("explain"), Space, Text("to"), Space, Text("you"), Space, Text("how"), Space, Text("all"), Space, Text("this"), Space, Text("mistaken"), Space, Text("idea"), Space, Text("of"), Space, Text("denouncing"), Space, Text("pleasure"), Space, Text("and"), Space, Text("praising"), Space, Text("pain"), Space, Text("was"), Space, Text("born"), Space, Text("and"), Space, Text("I"), Space, Text("will"), Space, Text("give"), Space, Text("you"), Space, Text("a"), Space, Text("complete"), Space, Text("account"), Space, Text("of"), Space, Text("the"), Space, Text("system,"), Space, Text("and"), Space, Text("expound"), Space, Text("the"), Space, Text("actual"), Space, Text("teachings"), Space, Text("of"), Space, Text("the"), Space, Text("great"), Space, Text("explorer"), Space, Text("of"), Space, Text("the"), Space, Text("truth,"), Space, Text("the"), Space, Text("master-builder"), Space, Text("of"), Space, Text("human"), Space, Text("happiness."), Space, Text("No"), Space, Text("one"), Space, Text("rejects,"), Space, Text("dislikes,"), Space, Text("or"), Space, Text("avoids"), Space, Text("pleasure"), Space, Text("itself,"), Space, Text("because"), Space, Text("it"), Space, Text("is"), Space, Text("pleasure,"), Space, Text("but"), Space, Text("because"), Space, Text("those"), Space, Text("who"), Space, Text("do"), Space, Text("not"), Space, Text("know"), Space, Text("how"), Space, Text("to"), Space, Text("pursue"), Space, Text("pleasure"), Space, Text("rationally"), Space, Text("encounter"), Space, Text("consequences"), Space, Text("that"), Space, Text("are"), Space, Text("extremely"), Space, Text("painful."), Space, Text("Nor"), Space, Text("again"), Space, Text("is"), Space, Text("there"), Space, Text("anyone"), Space, Text("who"), Space, Text("loves"), Space, Text("or"), Space, Text("pursues"), Space, Text("or"), Space, Text("desires"), Space, Text("to"), Space, Text("obtain"), Space, Text("pain"), Space, Text("of"), Space, Text("itself,"), Space, Text("because"), Space, Text("it"), Space, Text("is"), Space, Text("pain,"), Space, Text("but"), Space, Text("because"), Space, Text("occasionally"), Space, Text("circumstances"), Space, Text("occur"), Space, Text("in"), Space, Text("which"), Space, Text("toil"), Space, Text("and"), Space, Text("pain"), Space, Text("can"), Space, Text("procure"), Space, Text("him"), Space, Text("some"), Space, Text("great"), Space, Text("pleasure."), Space, Text("To"), Space, Text("take"), Space, Text("a"), Space, Text("trivial"), Space, Text("example,"), Space, Text("which"), Space, Text("of")))

  val paragraphOne = Paragraph(Vector(Text("It"), Space, Text("is"), Space, Text("a"), Space, Text("long"), Space, Text("established"), Space, Text("fact"), Space, Text("that"), Space, Text("a"), Space, Text("reader"), Space, Text("will"), Space, Text("be"), Space, Text("distracted"), Space, Text("by"), Space, Text("the"), Space, Text("readable"), Space, Text("content"), Space, Text("of"), Space, Text("a"), Space, Text("page"), Space, Text("when"), Space, Text("looking"), Space, Text("at"), Space, Text("its"), Space, Text("layout."), Space, Text("The"), Space, Text("point"), Space, Text("of"), Space, Text("using"), Space, Text("Lorem"), Space, Text("Ipsum"), Space, Text("is"), Space, Text("that"), Space, Text("it"), Space, Text("has"), Space, Text("a"), Space, Text("more-or-less"), Space, Text("normal"), Space, Text("distribution"), Space, Text("of"), Space, Text("letters,"), Space, Text("as"), Space, Text("opposed"), Space, Text("to"), Space, Text("using,"), Space, Text("'Content"), Space, Text("content'"), Space, Text("making"), Space, Text("it"), Space, Text("look"), Space, Text("like"), Space, Text("readable"), Space, Text("English."), Space, Text("Many"), Space, Text("desktop"), Space, Text("publishing"), Space, Text("packages"), Space, Text("and"), Space, Text("web"), Space, Text("page"), Space, Text("editors"), Space, Text("now"), Space, Text("use"), Space, Text("Lorem"), Space, Text("Ipsum"), Space, Text("as"), Space, Text("their"), Space, Text("default"), Space, Text("model"), Space, Text("text,"), Space, Text("and"), Space, Text("a"), Space, Text("search"), Space, Text("for"), Space, Text("will"), Space, Text("uncover"), Space, Text("many"), Space, Text("web"), Space, Text("sites"), Space, Text("still"), Space, Text("in"), Space, Text("their"), Space, Text("infancy."), Space, Text("Various"), Space, Text("versions"), Space, Text("have"), Space, Text("evolved"), Space, Text("over"), Space, Text("the"), Space, Text("years,"), Space, Text("sometimes"), Space, Text("by"), Space, Text("accident,"), Space, Text("sometimes"), Space, Text("on"), Space, Text("purpose"), Space, Text("(injected"), Space, Text("humour"), Space, Text("and"), Space, Text("the"), Space, Text("like).")))

  val plainText = Plain(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("plaint"), Space, TexInline(TexContent("""\frac{1+sin(x)} {y}""")), Space, Text("string"), Space, Text("in"), Space, Text("the"), Space, Text("end.")))
  val plainTextCompound = Paragraph(Vector(Text("This"), Space, Text("is"), Space, Text("a"), Space, Text("plaint"), Space, TexInline(TexContent("""\frac{1+sin(x)} {y}""")), Space, Text("string"), Space, Text("in"), Space, Text("the"), Space, Text("end.")))

  val blockQuote = BlockQuote(Vector(
    Markdown(RawMarkdownContent("This is quote")),
    Markdown(RawMarkdownContent("and should span several")),
    Markdown(RawMarkdownContent("yet another line for the block"))))

  val unorderedList = UnorderedList(Vector(Markdown(RawMarkdownContent(
    """1st block - It is a long established fact that a reader will be distracted by the readable content of a
      |  page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less
      |  normal distribution of letters, as opposed to using 'Content here, content here',""".stripMargin)), Markdown(RawMarkdownContent(
    """2nd list block - editors now use Lorem Ipsum as their default model text, and a search for
      |  'lorem ipsum' will uncover many web sites still in their infancy. Various versions
      |  injected humour and the like).
      |  There are many variations of passages of Lorem Ipsum available, but the majority have""".stripMargin)), Markdown(RawMarkdownContent(
    """3rd list block - If you are going to use a passage of Lorem Ipsum, you need to be""".stripMargin)), Markdown(RawMarkdownContent(
    """4th list block - sure there isn't anything embarrassing hidden in the middle
      |  of text. All the Lorem Ipsum generators on the Internet tend to r""".stripMargin))))
  val nul = "\0"
  val orderedList = OrderedList(Vector(
    Markdown(RawMarkdownContent(s"""This is a first item of an ordered list
               |${nul}    1. and this is a first sub item of a first item of an ordered list
               |    2. and this is a second sub item of a first item of an ordered list""".stripMargin)),
    Markdown(RawMarkdownContent("And, finally, this is a second item of an ordered list"))))
  val complexTable = MultilineTableBlock(Vector(25.0f, 75.0f),
    Some(MultilineTableCaption(Vector(Markdown(RawMarkdownContent("This is a table caption"))),Some("table:table_lable_name"))),
    Some(Vector(
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("This header is longer than sep")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("And this header is also longer than this separator")))))),
    Vector(Vector(
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("**Why do we use it?**")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""There-are
                                           |""".stripMargin)))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("**Where can I get some?**")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""dummy
                                           |""".stripMargin)))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("text")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("printing")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("**Where does it come from?**")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""leap-into
                                           |""".stripMargin)))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""variations-join
                                           |
                                           |""".stripMargin)))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("**What is Lorem Ipsum?**")))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""Lorem
                                           |""".stripMargin)))),
      MultilineTableCell(Vector(Markdown(RawMarkdownContent("""anything
                                           |""".stripMargin))))),
      Vector(
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("""It is a long established fact that a reader will be
                                             |distracted by the readable content of a page when looking at""".stripMargin)))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("""It uses a dictionary of over
                                             |Lorem Ipsum which looks reasonable""".stripMargin)))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("The generated Lorem Ipsum is")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("or non-characteristic words etc")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("""It uses a dictionary of over 200
                                             |you need to be sure there""".stripMargin)))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("""anything embarrassing hidden
                                             |you need to be sure there isn't
                                             |within this period""".stripMargin)))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("")))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent(""""There are many variations of passages.
                                              |*randomised words which : 1597 z*""".stripMargin)))),
        MultilineTableCell(Vector(Markdown(RawMarkdownContent("""but the majority have suffered alteration.
                                             |*to use a passage: "" (empty string)*""".stripMargin)))))))

  val referenceType1 = ReferenceBlock(Vector(Text("arbitrary"), Space, Text("case-insensitive"), Space, Text("reference"), Space, Text("text")), Src("https://www.mozilla.org", Some("this is title")))
  val referenceType2 = ReferenceBlock(Vector(Text("arbitrary"), Space, Text("case-insensitive"), Space, Text("123")), Src("https://www.mozilla.org", None))
  val texBlock1 = TexBlock(TexContent(
    """\frac{1+sin(x)}{y}
      |$$ \begin{array}{l}
      |x = k \cdot a \cdot \left(a + b\right) \\
      |y = k \cdot b \cdot \left(a + b\right) \\
      |z = k \cdot a \cdot b,
      |\end{array} $$""".stripMargin))
}
