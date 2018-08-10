package evolution

import java.util.UUID

trait Cell {
  def id: UUID
}

object World {

  case class CellReference(val position: (Int, Int), val id: UUID)

  def apply(size: (Int, Int), cs: ((Int, Int), Creature)*) = {
    def place(area: Area, rs: List[CellReference],
              cs: Seq[((Int, Int), Creature)]): World = cs match {
      case Seq() => new World(area, rs, 0)
      case Seq((pos, c), t @ _*) =>
        val na = area.updated(pos, c)
        val nrs = CellReference(pos, c.id) :: rs
        place(na, nrs, t)
    }
    place(Area(size), Nil, cs)
  }
}

class World(
  private val area: Area,
  private val rs:   List[World.CellReference],
  val turnNumber:   Int) {

  val size = area.size

  def turn(): World = {
    new World(area, rs, turnNumber + 1)
  }

  def sunFactor(pos: (Int, Int)): Float = {
    val mid = size._2.toFloat / 2f
    if (pos._2 < mid) pos._2 / mid
    else (pos._2 - size._2).abs / mid
  } ensuring (f => (f >= 0.0f) && (f <= 1.0f))

  val foreach = area.foreach(_)
}
