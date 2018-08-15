package evolution
import World.Position

sealed trait Ability {
  def suggest(w: World, a: Area, p: Position): Option[Suggestion]
}

object Ability {
  def apply() = Photosynthesis
}

object Suggestion {
  type Action = Area => (Area, Position)

  def apply(
    cost:   Creature.Energy,
    get:    Creature.Energy,
    action: Suggestion.Action) =
    new Suggestion(cost, get, action)
}

class Suggestion(
  val cost:   Creature.Energy,
  val get:    Creature.Energy,
  val action: Suggestion.Action)

object Empty extends Ability {
  private def action(p: Position)(a: Area) = (a, p)
  def suggest(w: World, a: Area, p: Position): Option[Suggestion] =
    Some(Suggestion(0, 0, action(p)(_)))
}

object Photosynthesis extends Ability {
  private val getMax: Creature.Energy = 10
  private def action(p: Position)(got: Creature.Energy)(a: Area) = {
    a(p) match {
      case c: Creature =>
        val nc = c.copy(
          energy = c.energy + got,
          sources = c.sources + EnergySources.Sun)
        (a.updated(p, nc), p)
    }
  }

  def suggest(w: World, a: Area, p: Position): Option[Suggestion] = {
    val sf = w.sunFactor(p)
    val get: Creature.Energy = if (sf > 0) (getMax * sf).round else 0

    Some(Suggestion(0, get, action(p)(get)(_)))
  }
}

object Scavenger extends Ability {
  private val cost = 1
  private def died(c: Option[Cell]): Option[Creature] = c match {
    case Some(c: Creature) if !c.alive => Some(c)
    case _                             => None
  }

  private def action(p: Position)(tp: Position, got: Creature.Energy)(a: Area) = {
    a(p) match {
      case c: Creature =>
        val nc = c.copy(
          energy = c.energy + got - cost,
          sources = c.sources + EnergySources.Carrion)
        ((a - p) + (tp -> nc), tp)
    }
  }

  def suggest(w: World, area: Area, pos: Position): Option[Suggestion] = {
    val ts = (for {
      p <- World.findNear(area, pos)
      c <- died(area.get(p))
    } yield (p, c)).sortBy(_._2.energy)(math.Ordering[Int].reverse)

    ts match {
      case Seq((tp, t), _*) =>
        Some(Suggestion(cost, t.energy, action(pos)(tp, t.energy)(_)))
      case _ => None
    }
  }
}

object Carnivore extends Ability {
  private val rand = util.Random
  private val cost = 10
  private def alive(c: Option[Cell]): Option[Creature] = c match {
    case Some(c: Creature) if c.alive => Some(c)
    case _                            => None
  }

  private def action(p: Position)(tp: Position, got: Creature.Energy)(a: Area) = {
    a(p) match {
      case c: Creature =>
        val (nc, np) = if (rand.nextInt(3) == 2) (
          c.copy(
            energy = c.energy - cost,
            sources = c.sources),
          p)
        else (
          c.copy(
            energy = c.energy + got - cost,
            sources = c.sources + EnergySources.Carrion),
          tp)
        ((a - p) + (np -> nc), np)
    }
  }

  def suggest(w: World, area: Area, pos: Position): Option[Suggestion] = {
    val ts = (for {
      p <- World.findNear(area, pos)
      c <- alive(area.get(p))
    } yield (p, c)).sortBy(_._2.energy)(math.Ordering[Int].reverse)

    ts match {
      case Seq((tp, t), _*) =>
        Some(Suggestion(cost, t.energy, action(pos)(tp, t.energy * 2 / 3)(_)))
      case _ => None
    }
  }
}