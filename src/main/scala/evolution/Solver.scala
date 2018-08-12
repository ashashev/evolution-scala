package evolution

trait Solver {
  def apply(ss: Seq[Suggestion]): Suggestion
}

object RandomSolver extends Solver {
  private val rand = util.Random

  def apply(ss: Seq[Suggestion]): Suggestion = {
    val n = rand.nextInt(ss.size)
    ss(n)
  }
}