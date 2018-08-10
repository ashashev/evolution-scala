package evolution

sealed trait Ability {
  def suggest(
    w:      World,
    c:      Creature,
    before: List[Creature],
    after:  List[Creature]): Suggestion
}

object Ability {
  def apply() = Photosynthesis
}

object Suggestion {
  type Action = (Creature, /*before:*/ List[Creature], /*after:*/ List[Creature]) => (Creature, /*before:*/ List[Creature], /*after:*/ List[Creature])

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
  private val suggestion = Suggestion(0, 0,
    (c: Creature, before: List[Creature], after: List[Creature]) => (c, before, after))

  def suggest(
    w:      World,
    c:      Creature,
    before: List[Creature],
    after:  List[Creature]): Suggestion = suggestion
}

object Photosynthesis extends Ability {
  private val getMax: Creature.Energy = 10

  def suggest(
    w:      World,
    c:      Creature,
    before: List[Creature],
    after:  List[Creature]): Suggestion = {
    val sf = w.sunFactor(c.position)
    val get: Creature.Energy = if (sf > 0) (getMax * sf).round else 0
    val action = (c: Creature, before: List[Creature], after: List[Creature]) =>
      (c.copy(energy = c.energy + get, sources = c.sources + EnergySources.Sun), before, after)

    Suggestion(0, get, action)
  }
}
