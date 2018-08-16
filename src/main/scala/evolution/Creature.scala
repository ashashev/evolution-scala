package evolution

import java.util.UUID

import Creature.Energy
import World.Position

object Creature {
  type Energy = Int
  val maxLevel: Energy = 1000
  private val defLevel: Energy = 40
  private val defNecessity: Energy = 1
  private val procreationLevel: Energy = 200
  private val defAbilities =
    Vector.fill(8)(Photosynthesis) ++ Vector.fill(8)(Empty)
  private val defSolver = RandomSolver

  private val rand = util.Random

  def apply() = new Creature(
    defLevel,
    EnergySources(),
    UUID.randomUUID(),
    true,
    defNecessity,
    defAbilities,
    defSolver)

  private def mutateAbilities(as: Vector[Ability]): Vector[Ability] = {
    if (rand.nextInt(4) != 0) as
    else as.updated(rand.nextInt(as.size), Ability.random())
  }

  //TODO: Idea - the procreation can be ability.
  def canProcreate(c: Creature): Boolean =
    c.alive && (c.energy >= procreationLevel)

  def procreate(parent: Creature): (Creature, Creature) = {
    val updatedParent = parent.copy(energy = parent.energy / 2)
    val child = parent.copy(
        energy = defLevel,
        id = UUID.randomUUID(),
        abilities = mutateAbilities(parent.abilities))
    (updatedParent, child)
  }

  def mustDie(c: Creature): Boolean =
    c.alive && ((c.energy >= maxLevel) || (c.energy <= 0))

  def die(c: Creature): Creature =
    if (c.energy <= 0) c.copy(energy = c.necessity * 200, alive = false)
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
    } yield ability.suggest(w, a, p)

    assert(suggestions.nonEmpty)
    solver(suggestions).action
  }
}

object EnergySources {
  def apply() = new EnergySources(128, 128, 128)
  def Meat(got: Energy) = new EnergySources(got, -1 * got, -1 * got)
  def Sun(got: Energy) = new EnergySources(-1 * got, got, -1 * got)
  def Carrion(got: Energy) = new EnergySources(-1 * got, -1 * got, got)
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
