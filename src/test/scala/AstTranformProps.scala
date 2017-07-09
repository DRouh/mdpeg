package org.mdpeg

import org.mdpeg.ast._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.ScalacheckShapeless._

object AstTranformProps extends Properties("Ast Tranform properties") {
  val texInline: Gen[TexInline] = Arbitrary.arbitrary[TexInline]
  val text: Gen[Text] = Arbitrary.arbitrary[Text]
  val code: Gen[Code] = Arbitrary.arbitrary[Code]
  val space: Gen[Space.type] = Arbitrary.arbitrary[Space.type]
  val lineBreak: Gen[LineBreak.type] = Arbitrary.arbitrary[LineBreak.type]

  def genBoundedList[T](maxSize: Int, g: Gen[T]): Gen[List[T]] =
    Gen.choose(0, maxSize) flatMap { sz => Gen.listOfN(sz, g) }

  val strong: Gen[Strong] = for {
    sps <- Gen.listOf(space)
    ts <- Gen.listOf(text)
  } yield Strong(sps ++ ts)

  val italics: Gen[Italics] = for {
    v <- Gen.choose(0, 50)
    sps <- genBoundedList(v / 2, space)
    ts <- genBoundedList(v, text)
  } yield Italics(sps ++ ts)

  val src: Gen[Src] = for {
    u <- Gen.alphaNumStr
    t <- Gen.option(Gen.alphaNumStr)
  } yield Src(u, t)

  val ref: Gen[Ref] = for {
    v <- Gen.choose(0, 50)
    sps <- genBoundedList(v / 2, space)
    ts <- genBoundedList(v, text)
    r <- Gen.alphaNumStr
  } yield Ref(sps ++ ts, r)

  val shortcutRef: Gen[ShortcutRef.type] = Arbitrary.arbitrary[ShortcutRef.type]

  val target: Gen[Target] = Gen.oneOf(src, ref, shortcutRef)

  val image: Gen[Image] =
    for {
      ss <- Gen.choose(0, 50)
      sps <- genBoundedList(ss / 2, space)
      ts <- genBoundedList(ss, text)
      t <- target
      w <- Gen.option(Gen.choose(0, 100))
    } yield Image(sps ++ ts, t, w)

  val link: Gen[Link] = for {
    v <- Gen.choose(0, 50)
    sps <- genBoundedList(v / 2, space)
    ts <- genBoundedList(v, text)
    t <- target
  } yield Link(sps ++ ts, t)

  val inlineContent: Gen[List[Inline]] = for {
    v <- Gen.choose(0, 50)
    tis <- genBoundedList(v, texInline)
    t <- genBoundedList(v, text)
    sps <- genBoundedList(v / 4, space)
    codes <- genBoundedList(v / 6, code)
    lbs <- genBoundedList(v / 8, lineBreak)
    strongs <- genBoundedList(v / 2, strong)
    italicss <- genBoundedList(v / 3, italics)
    images <- genBoundedList(v / 6, image)
    links <- genBoundedList(v, link)
  } yield (t ++ sps ++ tis ++ lbs ++ codes ++ strongs ++ italicss ++ images ++ links)

 // blocks
  val plains: Gen[List[Plain]] = for {ic <- inlineContent; p <- genBoundedList(100, Plain(ic))} yield p
  val paragraphs: Gen[List[Paragraph]] = for {ic <- inlineContent; p <- genBoundedList(100, Paragraph(ic))} yield p
  val headers: Gen[List[HeadingBlock]] = for {
    l <- Gen.choose(1, 100)
    ic <- inlineContent
    p <- genBoundedList(100, HeadingBlock(l, ic))
  } yield p
  val verbatims: Gen[List[Verbatim]] = for {v <- Arbitrary.arbitrary[Verbatim]; vs <- Gen.listOf(v) } yield vs
  val texBlocks: Gen[List[TexBlock]] = for {tb <- Arbitrary.arbitrary[TexBlock]; tbs <- Gen.listOf(tb) } yield tbs
  val refBlocks: Gen[List[ReferenceBlock]] = for {
    ic <- inlineContent
    t <- target
    rb <- Gen.listOf(ReferenceBlock(ic, t))
  } yield rb
  val markdowns: Gen[List[Markdown]] = for {md <- Arbitrary.arbitrary[Markdown]; mds <- Gen.listOf(md) } yield mds
  val blockQuotes: Gen[List[BlockQuote]] = for {
    mds <- markdowns
    bqs <- Gen.listOf(BlockQuote(mds.toVector))
  } yield bqs

  val uls: Gen[List[UnorderedList]] = for {
    v <- Gen.choose(25, 75)
    ps <-  Gen.resize(v / 3, plains)
    mds <- Gen.resize(v / 2, markdowns)
    uls <- genBoundedList(v, UnorderedList((ps ++ mds).toVector))
  } yield uls

  val ols: Gen[List[OrderedList]] = for {
    v <- Gen.choose(25, 75)
    ps <-  Gen.resize(v / 3, plains)
    mds <- Gen.resize(v / 2, markdowns)
    ols <- genBoundedList(v, OrderedList((ps ++ mds).toVector))
  } yield ols

  // todo add the rest of the block types
  val rawAst: Gen[List[Block]] = for {
    v <- Gen.choose(0, 25)
    ps <- plains
    pars <- paragraphs
    hs <- headers
    vbs <- verbatims
    tbs <- texBlocks
    rfbs <- refBlocks
    mds <- markdowns
    bqs <- blockQuotes
    ulss <- Gen.resize(v,uls)
    olss <- Gen.resize(v,ols)
  } yield ps ++ pars ++ hs ++ vbs ++ tbs ++ rfbs ++ mds ++ bqs ++ ulss ++ olss
  property("For any list of Plain texts tree can be transformed") = forAll(plains) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Paragarphs tree can be transformed") = forAll(paragraphs) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Headers tree can be transformed") = forAll(headers) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Verbatims tree can be transformed") = forAll(verbatims) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of TexBlocks tree can be transformed") = forAll(texBlocks) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of ReferenceBlock tree can be transformed") = forAll(refBlocks) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Markdown tree can be transformed") = forAll(markdowns) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of BlockQuotes tree can be transformed") = forAll(blockQuotes) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Unordered list tree can be transformed") = forAll(uls) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of Ordered list tree can be transformed") = forAll(uls) { i => ASTTransform.transformTree(i).isRight }
  property("For any list of any blocks tree can be transformed") = forAll(rawAst) { i => ASTTransform.transformTree(i).isRight }
}
