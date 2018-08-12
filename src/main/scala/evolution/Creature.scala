package evolution

import java.util.UUID

import Creature.Energy
import World.Position

object Creature {
  type Energy = Int
  private val maxLevel: Energy = 100
  private val defLevel: Energy = (maxLevel * 0.4f).round
  private val defNecessity: Energy = (maxLevel * 0.01f).round
  private val procreationLevel: Energy = (maxLevel * 0.7f).round
  private val defAbilities =
    Vector.fill(8)(Photosynthesis) ++ Vector.fill(8)(Empty)
  private val defSolver = RandomSolver

  def apply() = new Creature(
    defLevel,
    EnergySources(),
    UUID.randomUUID(),
    true,
    defNecessity,
    defAbilities,
    defSolver)

  //TODO: Idea - the procreation can be ability.
  def canProcreate(c: Creature): Boolean =
    c.alive && (c.energy >= procreationLevel)

  def procreate(parent: Creature): (Creature, Creature) = {
    val updatedParent = parent.copy(energy = parent.energy / 2)
    val child = parent.copy(energy = defLevel, id = UUID.randomUUID())
    (updatedParent, child)
  }

  def mustDie(c: Creature): Boolean =
    c.alive && ((c.energy >= maxLevel) || (c.energy <= 0))

  def die(c: Creature): Creature =
    if (c.energy <= 0) c.copy(energy = c.necessity * 2, alive = false)
    else c.copy(energy = c.energy / 2, alive = false)

  def mustRemoved(c: Creature): Boolean = !c.alive && (c.energy <= 0)
}

case class Creature(
  val energy:    Energy,
  val sources:   EnergySources,
  val id:        UUID,
  val alive:     Boolean,
  val necessity: Energy,
  val abilities: Vector[Ability],
  val solver:    Solver) extends Cell {

  def turn(w: World, a: Area, p: Position): Suggestion.Action = {
    val suggestions = for {
      ability <- abilities
      s <- ability.suggest(w, a, p)
    } yield s
    solver(suggestions).action
  }
}

object EnergySources {
  def apply() = new EnergySources(128, 128, 128)
  val Meat = new EnergySources(1, -1, -1)
  val Sun = new EnergySources(-1, 1, -1)
  val Carrion = new EnergySources(-1, -1, 1)
}

class EnergySources(val meat: Int, val sun: Int, val carrion: Int) {
  def copy(
    newMeat:    Int = meat,
    newSun:     Int = sun,
    newCarrion: Int = carrion): EnergySources = {
    new EnergySources(newMeat, newSun, newCarrion)
  }

  def +(that: EnergySources): EnergySources = {
    def sum(l: Int, r: Int): Int = (l + r) match {
      case s if s > 255 => 255
      case s if s < 0   => 0
      case s            => s
    }
    new EnergySources(
      sum(meat, that.meat),
      sum(sun, that.sun),
      sum(carrion, that.carrion))
  }
}
