package ashashev

object Creature {
  def apply(x: Int, y: Int): Creature = new Creature((x, y))
}

class Creature(val position: (Int, Int)) {
}
