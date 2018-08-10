package evolution

sealed trait Ability {
  def suggest(w: World, a: Area, pos: (Int, Int)): Suggestion
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
  private def action(pos: (Int, Int))(a: Area): (Area, (Int, Int)) = (a, pos)
  def suggest(w: World, a: Area, pos: (Int, Int)): Suggestion =
    Suggestion(0, 0, action(pos)(_))
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

  def suggest(w: World, a: Area, pos: (Int, Int)): Suggestion = {
    val sf = w.sunFactor(pos)
    val get: Creature.Energy = if (sf > 0) (getMax * sf).round else 0

    Suggestion(0, get, action(pos)(get)(_))
  }
}
