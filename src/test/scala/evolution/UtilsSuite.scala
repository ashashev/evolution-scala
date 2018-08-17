package evolution

import org.scalatest.FunSuite

import java.time.Duration

class UtilsSuite extends FunSuite {
  import Utils._

  test("prettyDuration of Zero") {
    val expected = "0:00:00.000"
    val dur = Duration.ZERO
    info(s"Expects that '${dur}' is '${expected}'")
    assert(prettyDuration(dur) === expected)
  }

  test("prettyDuration of 1 day 3 hours 5 minutes and 3.034 seconds") {
    val expected = "1d 3:05:03.034"
    val dur = Duration.ofDays(1).
      plusHours(3).
      plusMinutes(5).
      plusSeconds(3).
      plusNanos(34000000)
    info(s"Expects that '${dur}' is '${expected}'")
    assert(prettyDuration(dur) === expected)
  }
}
