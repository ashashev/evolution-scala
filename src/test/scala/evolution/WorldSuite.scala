package evolution

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

import Auxiliary._
import World.Position

class WorldSuite extends FunSuite with PropertyChecks {
  def toString(a: Area): String = s"size: ${a.size}\n" +
    (0 until a.size._2).map(y => (for {
      x <- (0 until a.size._1)
      c = if (a.get((x, y)).isEmpty) '_' else 'X'
    } yield c).mkString).mkString("\n")

  test("the findFreeNear method") {
    forAll(Gen.posNum[Int], Gen.posNum[Int]) { (w, h) =>
      whenever(w > 0 && h > 0) {
        val bs = Gen.listOfN(w * h, genOptionCell).sample.get.toList
        val area = makeArea(bs)
        val pos: Position = genPosition(area).sample.get

        val l = World.findFreeNear(area, pos)
        l foreach { p =>
          assert(area.get(p).isEmpty)
        }
      }
    }
  }
}