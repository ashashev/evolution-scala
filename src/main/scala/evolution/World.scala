package evolution

import java.util.UUID

trait Cell {
  def id: UUID
}

import World.{ Position, CellReference }
import Creature.Energy

object World {
  type Position = (Int, Int)
  private val rand = util.Random

  case class CellReference(val p: Position, val id: UUID)

  type StepInfo = (Area, List[CellReference])

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

  def findNear(a: Area, p: Position): Seq[Position] = for {
      dx <- (-1 to 1)
      dy <- (-1 to 1)
      x = (p._1 + dx + a.size._1) % a.size._1
      y = (p._2 + dy + a.size._2) % a.size._2
      np = (x, y)
      if (np != p) && a.includes(np)
    } yield np

  def findFreeNear(a: Area, p: Position): Seq[Position] =
    findNear(a, p).filter(p => a.get(p).isEmpty)

  def selectCell(ps: Seq[Position]): Option[Position] =
    if (ps.isEmpty) None
    else Some(ps(rand.nextInt(ps.size)))
}

class World(
  private val area: Area,
  private val rs:   List[CellReference],
  val turnNumber:   Int) {

  import World._

  val size = area.size

  def creatureTurn(a: Area, rs: List[CellReference]): StepInfo = {
    val (r, t) = (rs.head, rs.tail)

    a(r.p) match {
      case c: Creature if c.alive =>
        assert(c.id == r.id)

        val (na, np) = c.turn(this, a, r.p)(a)
        (na, CellReference(np, na(np).id) :: t)
      case _ => (a, rs)
    }
  }

  def dailySpendEnergy(a: Area, rs: List[CellReference]): StepInfo = {
    val r = rs.head

    a(r.p) match {
      case c: Creature =>
        assert(c.id == r.id)
        val spent: Energy =
          if (c.alive) (c.necessity * (1f + (1f - sunFactor(r.p)))).round
          else 1
        (a.updated(r.p, c.copy(energy = c.energy - spent)), rs)
      case _ => (a, rs)
    }
  }

  def procreate(a: Area, rs: List[CellReference]): StepInfo = {
    val (r, t) = (rs.head, rs.tail)

    a(r.p) match {
      case c: Creature if Creature.canProcreate(c) =>
        assert(c.id == r.id)
        selectCell(findFreeNear(a, r.p)) match {
          case Some(cp) =>
            val (parent, child) = Creature.procreate(c)
            //println(s"procreate: Position: ${cp}, id: ${child.id}")
            (a.updated(r.p, parent).updated(cp, child),
              r :: CellReference(cp, child.id) :: t)
          case None => (a, rs)
        }
      case _ => (a, rs)
    }
  }

  def timeToDie(a: Area, rs: List[CellReference]): StepInfo = {
    val r = rs.head

    a(r.p) match {
      case c: Creature if Creature.mustDie(c) =>
        assert(c.id == r.id)
        (a.updated(r.p, Creature.die(c)), rs)
      case _ => (a, rs)
    }
  }

  def endTurn(a: Area, rs: List[CellReference]): StepInfo = {
    val (r, t) = (rs.head, rs.tail)

    a(r.p) match {
      case c: Creature if Creature.mustRemoved(c) =>
        assert(c.id == r.id)
        //println(s"removed: Position: ${r.p}, id: ${c.id}")
        (a - r.p, t)
      case _ => (a, rs)
    }
  }

  private val turnHandler = ((creatureTurn _).tupled).
    andThen((dailySpendEnergy _).tupled).
    andThen((procreate _).tupled).
    andThen((timeToDie _).tupled).
    andThen((endTurn _).tupled)

  def turn(): World = {
    def isValid(a: Area, r: CellReference): Boolean = a.get(r.p) match {
      case Some(c) => c.id == r.id
      case _ => false
    }
    def impl(a: Area, moved: List[CellReference],
             ns: List[CellReference]): (Area, List[CellReference]) = ns match {
      case Nil    => (a, moved.reverse)
      case h :: t if isValid(a, h) =>
        val (na, nh) = turnHandler((a, List(h)))
        impl(na, nh ++ moved, t)
      case h :: t if !isValid(a, h) =>
        impl(a, moved, t)
    }

    val (narea, nrs) = impl(area, List.empty[CellReference], rs)
    new World(narea, nrs, turnNumber + 1)
  }

  def sunFactor(p: Position): Float = {
    val mid = size._2.toFloat / 2f
    if (p._2 < mid) p._2 / mid
    else (p._2 - size._2).abs / mid
  } ensuring (f => (f >= 0.0f) && (f <= 1.0f))

  val foreach = area.foreach(_)
}
