object TestData {

  val headingOne = "Heading One"
  val headingTwo = "Heading Two"
  val paragraphOne =
    """It is a long established fact that a reader will be distracted by the readable content
    |of a page when looking at its layout. The point of using Lorem Ipsum is that it has a
    |more-or-less normal distribution of letters, as opposed to using, 'Content content'
    |making it look like readable English. Many desktop publishing packages and web page editors
    |now use Lorem Ipsum as their default model text, and a search for will uncover
    |many web sites still in their infancy. Various versions have evolved over the years,
    |sometimes by accident, sometimes on purpose (injected humour and the like).""".stripMargin
  val paragraphTwo =
    """But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born
    |and I will give you a complete account of the system, and expound the actual teachings of the great
    |explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids
    |pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure
    |rationally encounter consequences that are extremely painful. Nor again is there anyone who loves
    |or pursues or desires to obtain pain of itself, because it is pain, but because occasionally
    |circumstances occur in which toil and pain can procure him some great pleasure.
    |To take a trivial example, which of""".stripMargin

  val blockQuoteLineOne = "This is quote"
  val blockQuoteLineTwo = "and should span several"
  val blockQuoteLineThree = "yet another line for the block"
  val blockQuote =
    s"""${blockQuoteLineOne}
    |${blockQuoteLineTwo}
    |${blockQuoteLineThree}
    |""".stripMargin
  val plainText = "This is a plaint string in the end."
  val compoundMD =
    s"""# ${headingOne}
       |
       |## ${headingTwo}
       |
       |${paragraphOne}
       |
       |${paragraphTwo}
       |
       |****
       |
       |-----
       |
       |> ${blockQuoteLineOne}
       |> ${blockQuoteLineTwo}
       |> ${blockQuoteLineThree}
       |
       |_ _ _ _
       |
       |${plainText}"""
      .stripMargin
}
