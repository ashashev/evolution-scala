package evolution

import java.util.UUID

object Creature {
  type Energy = Int
  val maxLevel: Energy = 100
  val defLevel: Energy = (maxLevel * 0.5f).round

  def apply() = new Creature(defLevel, EnergySources(), UUID.randomUUID(), true)
}

case class Creature(
  val energy:  Creature.Energy = Creature.defLevel,
  val sources: EnergySources,
  val id:      UUID,
  val alive:   Boolean) extends Cell

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
