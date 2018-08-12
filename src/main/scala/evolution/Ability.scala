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
