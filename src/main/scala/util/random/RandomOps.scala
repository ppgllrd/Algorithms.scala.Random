/** ****************************************************************************
  * Different distribution generators that can be mixed with
  * arbitrary pseudorandom generators
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package util.random


object RandomOps {
  // constants for generating gaussian random values using
  // Leva's algorithm
  final object LevaConstants {
    final val r = StrictMath.sqrt(2 / StrictMath.E)
    final val twiceR = 2 * r
    final val s = 0.449871
    final val t = -0.386595
    final val r1 = 0.27597
    final val r2 = 0.27846
    final val a = 0.19600
    final val b = 0.25472
  }
}

trait RandomOps {
  protected def internalNextLong(l: Long): Long
  def nextDouble(): Double

  // for implementing a gaussian generator using Box-Muller algorithm
  protected var haveNextNextGaussian: Boolean = false
  protected var nextNextGaussian: Double = 0

  /**
    * Returns a uniformly distributed pseudorandom {@code Long}
    * value in range [0,{@code n})
    *
    * @param n exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Long}
    *   from 0 (inclusive) to {@code n} (exclusive)
    */
  def uniform(n: Long): Long = {
    if (n <= 0)
      throw new IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    internalNextLong(n)
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Int}
    * value in range [0,{@code n})
    *
    * @param n exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Int}
    *   from 0 (inclusive) to {@code n} (exclusive)
    */
  def uniform(n: Int): Int = {
    if (n <= 0)
      throw new IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    internalNextLong(n).toInt
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Double}
    * value in range [0,{@code n})
    *
    * @param n exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Double}
    *   from 0 (inclusive) to {@code n} (exclusive)
    */
  def uniform(n: Double): Double = {
    if (n <= 0)
      throw new IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    nextDouble() * n
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Long}
    * value in range [{@code low},{@code high})
    *
    * @param low inclusive lower bound of range
    * @param high exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Long}
    *   from {@code low} (inclusive) to {@code high} (exclusive)
    */
  def uniform(low: Long, high: Long): Long = {
    if (high <= low)
      throw new IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + internalNextLong(high - low)
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Int}
    * value in range [{@code low},{@code high})
    *
    * @param low inclusive lower bound of range
    * @param high exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Int}
    *   from {@code low} (inclusive) to {@code high} (exclusive)
    */
  def uniform(low: Int, high: Int): Int = {
    if (high <= low)
      throw new IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + internalNextLong(high - low).toInt
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Double}
    * value in range [{@code low},{@code high})
    *
    * @param low inclusive lower bound of range
    * @param high exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Double}
    *   from {@code low} (inclusive) to {@code high} (exclusive)
    */
  def uniform(low: Double, high: Double): Double = {
    if (high <= low)
      throw new IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + nextDouble() * (high - low)
  }

  /**
    * Returns an uniformly pseudorandomly chosen element from all
    * values in sequence {@code xs}
    *
    * @param xs sequence of values to choose from
    * @return an uniformly pseudorandomly chosen element from all
    *         values in sequence {@code xs}
    */
  def uniform[A](xs : Seq[A]) : A =
    xs(internalNextLong(xs.length).toInt)

  /**
    * Result will be {@code true} with probability 0.5
    * and {@code false} with probability 0.5. In other words,
    * results are distributed according to a
    * [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli distribution]]
    * with probability of success 0.5
    *
    * @return {@code true} with probability 0.5 and
    *         {@code false} with probability 0.5
    */
  def bernoulli(): Boolean = {
    nextDouble() < 0.5
  }

  /**
    * Result will be {@code true} with probability {@code p}
    * and {@code false} with probability 1 - {@code p}. In other words,
    * results are distributed according to a
    * [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli distribution]]
    * with probability of success {@code p}
    *
    * @param p probability of success. Must be in [0.0,1.0]
    * @return {@code true} with probability {@code p} and
    *         {@code false} with probability 1 - {@code p}
    */
  def bernoulli(p: Double): Boolean = {
    if (p < 0.0 || p > 1.0)
      throw new IllegalArgumentException("bernoulli: success probability must be in [0,1]")
    nextDouble() < p
  }

  def gaussianBoxMuller(): Double = {
    // Box-Muller algorithm
    if (haveNextNextGaussian) {
      haveNextNextGaussian = false
      nextNextGaussian
    }
    else {
      while (true) {
        val v1 = 2 * nextDouble() - 1 // between -1 and 1

        val v2 = 2 * nextDouble() - 1
        val s = v1 * v1 + v2 * v2
        if (s < 1.0 && s != 0.0) {
          val multiplier = StrictMath.sqrt(-2 * StrictMath.log(s) / s)
          // println(v1, v2, s, multiplier)
          nextNextGaussian = v2 * multiplier
          haveNextNextGaussian = true
          return v1 * multiplier
        }
      }
      0.0 // not reached
    }
  }

  /**
    * Returns a Gaussian ("normally") distributed pseudorandom
    * {@code Double} value with mean {@code 0.0} and standard
    * deviation {@code 1.0}.
    *
    * @return a pseudorandom {@code Double} value distributed
    *     accordingly to a normal distribution with mean {@code 0.0}
    *     and standard deviation {@code 1.0}.
    */
  def gaussian(): Double = {
    /**
      * Joseph L. Leva's algorithm
      * A fast normal random number generator.
      * ACM Transactions on Mathematical Software 18(4):449-453.
      * December 1992
      *
      */
    import RandomOps.LevaConstants._
    while (true) {
      val u = nextDouble()
      val v = twiceR * (0.5 - nextDouble())
      val x = u - s
      val y = StrictMath.abs(v) - t
      val q = x * x + y * (a * y - b * x)
      if (q < r1
        || (q <= r2 && v * v <= -4 * u * u * StrictMath.log(u)))
        return v / u
    }
    0.0 // not reached
  }

  /**
    * Returns a Gaussian ("normally") distributed pseudorandom
    * {@code Double} value with mean {@code mu} and standard
    * deviation {@code sigma}.
    *
    * @return a pseudorandom {@code Double} value distributed
    *      accordingly to a normal distribution with mean {@code mu}
    *      and standard deviation {@code sigma}.
    */
  def gaussian(mu: Double, sigma: Double): Double =
    mu + sigma * gaussian()
}
