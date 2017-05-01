object TestData {
  val headingOne: String = "Heading One"
  val headingTwo: String = "Heading Two"
  val paragraphOne: String =
    """It is a long established fact that a reader will be distracted by the readable content
    |of a page when looking at its layout. The point of using Lorem Ipsum is that it has a
    |more-or-less normal distribution of letters, as opposed to using, 'Content content'
    |making it look like readable English. Many desktop publishing packages and web page editors
    |now use Lorem Ipsum as their default model text, and a search for will uncover
    |many web sites still in their infancy. Various versions have evolved over the years,
    |sometimes by accident, sometimes on purpose (injected humour and the like).""".stripMargin
  val paragraphTwo: String =
    """But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born
    |and I will give you a complete account of the system, and expound the actual teachings of the great
    |explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids
    |pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure
    |rationally encounter consequences that are extremely painful. Nor again is there anyone who loves
    |or pursues or desires to obtain pain of itself, because it is pain, but because occasionally
    |circumstances occur in which toil and pain can procure him some great pleasure.
    |To take a trivial example, which of""".stripMargin

  val blockQuoteLineOne: String = "This is quote"
  val blockQuoteLineTwo: String = "and should span several"
  val blockQuoteLineThree: String = "yet another line for the block"
  val blockQuote: String = s"""$blockQuoteLineOne $blockQuoteLineTwo $blockQuoteLineThree""".stripMargin
  val plainText: String = "This is a plaint string in the end."
  val codeBlock: String =
    """javascript
      |var s = "JavaScript syntax highlighting";
      |alert(s);
      |""".stripMargin
  val codeBlock2: String =
    """$#@#%$# DJI therefore frequently
      |DJI_EWEQ="-Qdxv3eqewq32 -rfre43rt:+terwfq43E#"
      |""".stripMargin
  val codeBlock3: String =
    """lorem.lobster {
      |  doloremque = "47.8"
      |  nostrum = "l2norm"
      |  rerum-facilis = "hello.xlsx"
      |  rerum-facilis.dolor = 0.5748
      |   rerum-facilis {
      |    rerum-facilis.dolor = 312
      |  }
      |}
      |""".stripMargin
  val codeBlock4: String =
    s"""lorem.lobster {
       |
       |  JIOJ-url = "lala://localhost:7891/lamda"
       |  # JIOJ-url = "lala:///langust/cashier/lambda"
       |
       |  # There are many variations
       |  going {
       |    # use a passage
       |    passage = "dolor.sit.amet.vitae:qui"
       |
       |    # passage of Lorem Ipsum
       |    generators = ""
       |
       |    # accusantium doloremque laudantium, totam rem aperiam,
       |    # doloremque = ""
       |
       |    # Sed ut perspiciatis unde omnis iste natus error sit voluptatem
       |    # nostrum = ""
       |
       |    # Quis autem vel eum iure reprehenderit
       |    mistaken  = "*"
       |
       |    # Vero eos et accusamus
       |    # for example: hdfs://commodi:consequatur/voluptatem/magnam.dolorem
       |    denouncing-pleasure = "who:///because/again.resultant"
       |
       |    rerum-facilis  = 0 s
       |
       |    # Ut enim ad minima veniam
       |    # commodi-consequatur = "http://example.com"
       |  }
       |
       |  # At vero eos et accusamus et iusto
       |  dignissimos  {
       |    # Itaque earum rerum hic tenetur
       |    dignissimos-maxime  = 5 impedit
       |    maxime-dignissimos  = 5 impedit
       |    maxime-dignissimos -molestias  = 30 impedit
       |    dignissimos-maxime  = 30 impedit
       |    quod -dignissimos -family-score = 30 impedit
       |    maxime-dignissimos -score = 30 impedit
       |
       |    dignissimos-maxime  = 10 frequently
       |    maxime-dignissimos  = 10 indignation
       |    maxime-input-dignissimos -lag = 30 frequently
       |
       |    # On the other hand, we denounce with righteous indignatio
       |    # and dislike men who are so beguiled and d
       |    # easy to distinguish. In a free hour, when o
       |    wise-man-therefore = 10 frequently
       |
       |    tenetur-jobs-earum  = 1 weakness
       |    tenetur-earum-step = 30 weakness
       |    earum-tenetu-max  = 30 weakness
       |  }
       |
       |  # wise man therefore always holds in
       |  therefore {
       |    every  = “"      # s qui blanditiis praesentium voluptatum deleniti atque corrupt
       |    bound   = 20 s   # hpain, but because occasionally circumstances occur in which toil and
       |    Rackham = ""     # Geasure itself, because it is pleasure, but because those who do
       |    These   = 8649   # ctetur, adipisci velit, sed quia non numquam eius
       |    prevents  = 1    # ia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.
       |  }
       |}
       |# DJI therefore frequently
       |DJI_EWEQ="-Qdxv3eqewq32 -rfre43rt:+terwfq43E#"
       |""".stripMargin

  val firstItemInList: String = "First item"
  val secondItemInList: String = "Second item"
  val sparseOrderedList: String =
    s"""1. $firstItemInList
       |
       |2. $secondItemInList
       |
       """.stripMargin
  val tightOrderedList: String =
    s"""1. $firstItemInList
       |2. $secondItemInList""".stripMargin
  val sparseUnorderedList: String =
    s"""- $firstItemInList
       |
       |- $secondItemInList
       |
       """.stripMargin
  val tightUnorderedList: String =
    s"""- $firstItemInList
       |- $secondItemInList""".stripMargin
  val compoundMD: String =
    s"""# $headingOne
       |
       |## $headingTwo
       |
       |$paragraphOne
       |
       |$paragraphTwo
       |
       |****
       |
       |-----
       |
       |> $blockQuoteLineOne
       |> $blockQuoteLineTwo
       |> $blockQuoteLineThree
       |
       |_ _ _ _
       |
       |```$codeBlock4```
       |$plainText"""
      .stripMargin
}
