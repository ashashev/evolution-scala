package evolution

import java.time.Duration
import java.time.temporal.ChronoUnit

object Utils {
  private val lessSecond = Set(ChronoUnit.MILLIS, ChronoUnit.MICROS, ChronoUnit.NANOS)

  def prettyDuration(d: Duration): String = {
    import ChronoUnit._
    val units = Seq(DAYS, HOURS, MINUTES, SECONDS, MILLIS)

    def extract(u: ChronoUnit, d: Duration): (Long, Duration) = {
      val b = Duration.of(1, u)
      if (lessSecond.contains(u)) {
        val result = d.getNano().toLong / b.getNano().toLong
        val rest = d.minusNanos(result * b.getNano().toLong)
        (result, rest)
      } else {
        val result = d.getSeconds() / b.getSeconds()
        val rest = d.minusSeconds(result * b.getSeconds())
        (result, rest)
      }
    }

    def fmt(d: Duration, us: Seq[ChronoUnit], s: String): String = us match {
      case Seq() => s
      case Seq(u, t @ _*) =>
        val (du, rest) = extract(u, d)
        val part = u match {
          case MILLIS         => f"${du}%03d"
          case SECONDS        => f"${du}%02d."
          case DAYS if du > 0 => s"${du}d "
          case HOURS          => s"${du}:"
          case MINUTES        => f"${du}%02d:"
          case _              => ""
        }
        fmt(rest, t, s + part)
    }

    fmt(d, units, "")
  }
}
