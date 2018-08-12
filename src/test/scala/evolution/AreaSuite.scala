package evolution

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

import Auxiliary._

class AreaSuite extends FunSuite with PropertyChecks {
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
        val a = makeArea(cs)
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
