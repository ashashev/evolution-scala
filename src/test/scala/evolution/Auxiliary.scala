package evolution

import java.util.UUID
import org.scalatest.FunSuite
import org.scalacheck.{ Gen, Arbitrary }
import World.Position

object Auxiliary {
  case class TestCell(id: UUID) extends Cell

  val genCell: Gen[TestCell] =
    for (id <- Arbitrary.arbitrary[UUID]) yield TestCell(id)

  val genEqualCells: Gen[(TestCell, TestCell)] =
    for (c1 <- genCell) yield {
      val c2 = c1.copy()
      (c1, c2)
    }

  def isPositionValid(p: Position, a: Area) =
    a.includes(p) && a.get(p).isDefined

  def numToPos(n: Int, a: Area) = (n % a.size._1, n / a.size._1)

  trait AreaFiller {
    def apply(a: Area): Area = {
      foldLeft(a) { (a, nc) =>
        val (n, c) = nc
        a.updated(numToPos(n, a), c)
      }
    }
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B
    def size: Int
  }

  implicit class CellList(val cs: List[Cell]) extends AreaFiller {
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B =
      (cs./:((0, z)) { (acc, c) =>
        val (n, z) = acc
        (n + 1, op(z, (n, c)))
      })._2

    def size: Int = cs.size
  }

  implicit class OptionCellList(val cs: List[Option[Cell]]) extends AreaFiller {
    def foldLeft[B](z: B)(op: (B, (Int, Cell)) => B): B =
      (cs./:((0, z)) { (acc, c) =>
        c match {
          case None    => (acc._1 + 1, acc._2)
          case Some(c) => (acc._1 + 1, op(acc._2, (acc._1, c)))
        }
      })._2

    def size: Int = cs.size
  }

  def fillArea(a: Area, af: AreaFiller): Area = af(a)

  def makeArea(af: AreaFiller): Area = {
    val height: Int = scala.math.sqrt(af.size.toDouble).toInt
    val width = (af.size / height) + 1
    fillArea(Area(width, height), af)
  }

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

  def genPosition(a: Area): Gen[Position] = for {
    x <- Gen.choose[Int](0, a.size._1 - 1)
    y <- Gen.choose[Int](0, a.size._2 - 1)
  } yield (x, y)

  implicit val arbArea: Arbitrary[Area] = Arbitrary(genArea)
}

class AuxiliarySuite extends FunSuite {
  import Auxiliary._

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
