package evolution

import java.util.UUID
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{ Gen, Arbitrary }

case class TestCell(id: UUID) extends Cell

object AreaSuiteAux {
  val genCell: Gen[TestCell] =
    for (id <- Arbitrary.arbitrary[UUID]) yield TestCell(id)

  val genEqualCells: Gen[(TestCell, TestCell)] =
    for (c1 <- genCell) yield {
      val c2 = c1.copy()
      (c1, c2)
    }

  def isPositionValid(p: (Int, Int), a: Area) =
    p._1 >= 0 && p._1 < a.size._1 &&
      p._2 >= 0 && p._2 < a.size._2 &&
      a.get(p).isDefined

  def numToPos(n: Int, a: Area) = (n % a.size._1, n / a.size._1)

  trait AreaFiller {
    def apply(a: Area): Area = {
      foldLeft(a) { (a, nc) =>
        val (n, c) = nc
        a.updated(numToPos(n, a), c)
      }
    }
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B
  }

  implicit class CellList(val cs: List[Cell]) extends AreaFiller {
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B =
      (cs./:((0, z)) { (acc, c) =>
        val (n, z) = acc
        (n + 1, op(z, (n, c)))
      })._2
  }

  implicit class OptionCellList(val cs: List[Option[Cell]]) extends AreaFiller {
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B =
      (cs./:((0, z)) { (acc, c) =>
        c match {
          case None    => (acc._1 + 1, acc._2)
          case Some(c) => (acc._1 + 1, op(acc._2, (acc._1, c)))
        }
      })._2
  }

  def fillArea(a: Area, af: AreaFiller): Area = af(a)

  val genOptionCell: Gen[Option[TestCell]] =
    Gen.oneOf(genCell.map(Some(_)), Gen.const(None))

  def genAreaWithSize(size: (Int, Int)): Gen[Area] = {
    val count = size._1 * size._2
    for {
      cs <- Gen.listOfN(count, genOptionCell)
    } yield fillArea(Area(size), cs)
  }

  val genArea: Gen[Area] = for {
    width <- Gen.posNum[Int]
    height <- Gen.posNum[Int]
    area <- genAreaWithSize((width, height))
  } yield area

  implicit val arbArea: Arbitrary[Area] = Arbitrary(genArea)
}

class AreaSuiteAux extends FunSuite {
  import AreaSuiteAux._

  test("the genEqualCells") {
    val Some((c1, c2)) = genEqualCells.sample
    assert(c1 === c2)
    assert(!(c1 eq c2))
  }

  test("the fillArea method #1") {
    val s = (2, 2)
    val Some(cs) = Gen.listOfN(s._1 * s._2, genCell).sample
    val a = fillArea(Area(s), cs)
    assert(a(0 -> 0) === cs.head)
    assert(a(1 -> 0) === cs.tail.head)
    assert(a(0 -> 1) === cs.tail.tail.head)
    assert(a(1 -> 1) === cs.tail.tail.tail.head)
  }

  test("the fillArea method #2") {
    val s = (2, 2)
    val Some(cs) = Gen.listOfN(s._1 * s._2, genOptionCell).sample
    val a = fillArea(Area(s), cs)
    assert(a.get(0 -> 0) === cs.head)
    assert(a.get(1 -> 0) === cs.tail.head)
    assert(a.get(0 -> 1) === cs.tail.tail.head)
    assert(a.get(1 -> 1) === cs.tail.tail.tail.head)
  }
}

class AreaSuite extends FunSuite with PropertyChecks {
  import AreaSuiteAux._

  test("create empty Area") {
    val a = Area(2, 3)
    assert(a.size === 2 -> 3)
    for (x <- 0 until a.size._1; y <- 0 until a.size._2) {
      assert(a.get((x, y)) === None)
    }
  }

  test("the adding element") {
    val a = Area(3, 3)
    val Some(c) = genCell.sample
    val na = a + ((1, 2) -> c)
    assert(na(1 -> 2) === c)
  }

  test("the removing element") {
    val a = Area(3, 3) + ((1, 2) -> genCell.sample.get)
    val na = a - (1 -> 2)
    assert(na.get(1 -> 2) === None)
  }

  test("the == operator should return true") {
    val Some((c1, c2)) = genEqualCells.sample
    val a1: Any = Area(3, 3) + ((1, 2) -> c1)
    val a2: Any = Area(3, 3) + ((1, 2) -> c2)
    val isEqual = a1 == a2
    assert(isEqual === true)
  }

  test("the == operator should return false") {
    val Some(c1) = genCell.sample
    val Some(c2) = genCell.sample
    val a1: Any = Area(3, 3) + ((1, 2) -> c1)
    val a2: Any = Area(3, 3) + ((1, 2) -> c2)
    val isEqual = a1 == a2
    assert(isEqual == false)
  }

  test("the foldLeft method") {
    val s = (3, 2)
    val Some(cs) = Gen.listOfN(s._1 * s._2, genCell).sample
    val a = fillArea(Area(s), cs)

    val got = a.foldLeft(List.empty[Cell]) { (l, c) => c._2 :: l }.reverse
    assert(got === cs)
  }

  test("the foldRight method") {
    val s = (3, 2)
    val Some(cs) = Gen.listOfN(s._1 * s._2, genCell).sample
    val a = fillArea(Area(s), cs)

    val got = a.foldRight(List.empty[Cell]) { (c, l) => c._2 :: l }
    assert(got === cs)
  }

  test("the foreach method") {
    forAll(Gen.listOf(genOptionCell)) { cs: List[Option[Cell]] =>
      whenever(!cs.isEmpty) {
        val height: Int = scala.math.sqrt(cs.size.toDouble).toInt
        val width = (cs.size / height) + 1
        val a = fillArea(Area(width, height), cs)
        val cells = cs.withFilter(!_.isEmpty).map(_.get)
        var got = List.empty[Cell]
        a.foreach((p, c) => got = c :: got)
        got = got.reverse
        assert(got === cells)
      }
    }
  }

  test("the ++ method") {
    forAll { (a1: Area, a2: Area) =>
      val a = a1 ++ a2
      val expectedSize = (a1.size._1 max a2.size._1, a1.size._2 max a2.size._2)
      assert(a.size === expectedSize)

      a foreach { (p, c) =>
        if (isPositionValid(p, a2)) {
          assert(a(p) === a2(p))
          ()
        } else {
          assert(isPositionValid(p, a1))
          assert(a(p) === a1(p))
          ()
        }
      }
    }
  }
}
