package org.mdpeg

import org.mdpeg.ast._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.ScalacheckShapeless._

object AstTranformSpec extends Properties("AstTranform") {
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


  val plains: Gen[List[Plain]] = for {ic <- inlineContent; p <- genBoundedList(100, Plain(ic))} yield p
  val paragraphs: Gen[List[Paragraph]] = for {ic <- inlineContent; p <- genBoundedList(100, Paragraph(ic))} yield p

  property("For any Plain text tree can be transformed") = forAll(plains) { i => ASTTransform.transformTree(i).isRight }
  property("For any Paragarph tree can be transformed") = forAll(paragraphs) { i => ASTTransform.transformTree(i).isRight }
}
