import org.scalatest.{FlatSpec, Matchers}

abstract class BaseSpec extends FlatSpec with Matchers {
  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }
}
