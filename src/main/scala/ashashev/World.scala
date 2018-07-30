package ashashev

object World {
  def apply(size: (Int, Int)) =
    new World(size)

  def apply(width: Int, height: Int) =
    new World((width, height))
}

class World(val size: (Int, Int)) {
  def turn(creatures: List[Creature]) = {
    creatures
  }
}
