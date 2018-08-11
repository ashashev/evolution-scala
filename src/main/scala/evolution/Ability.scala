package evolution

sealed trait Ability {
  def suggest(w: World, a: Area, pos: (Int, Int)): Option[Suggestion]
}

object Ability {
  def apply() = Photosynthesis
}

object Suggestion {
  type Action = Area => (Area, (Int, Int))

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
  def suggest(w: World, a: Area, pos: (Int, Int)): Option[Suggestion] = None
}

object Photosynthesis extends Ability {
  private val getMax: Creature.Energy = 10
  private def action(pos: (Int, Int))(
    got: Creature.Energy)(a: Area): (Area, (Int, Int)) = {
    a(pos) match {
      case c: Creature =>
        val nc = c.copy(
          energy = c.energy + got,
          sources = c.sources + EnergySources.Sun)
        (a.updated(pos, nc), pos)
    }
  }

  def suggest(w: World, a: Area, pos: (Int, Int)): Option[Suggestion] = {
    val sf = w.sunFactor(pos)
    val get: Creature.Energy = if (sf > 0) (getMax * sf).round else 0

    Some(Suggestion(0, get, action(pos)(get)(_)))
  }
}
