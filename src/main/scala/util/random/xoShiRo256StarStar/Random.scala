/** ****************************************************************************
  * Pseudorandom generator based on xoShiRo256**
  * See http://xoshiro.di.unimi.it/
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package util.random.xoShiRo256StarStar

object Random {

  import java.util.concurrent.atomic.AtomicLong

  private val _seedUniquifier = new AtomicLong(8682522807148012L)

  private def seedUniquifier(): Long = {
    while (true) {
      val current = _seedUniquifier.get
      val next = current * 181783497276652981L
      if (_seedUniquifier.compareAndSet(current, next))
        return next
    }
    0L // not reached
  }

  private final val DOUBLE_UNIT = 1.1102230246251565E-16 // 0x1.0p-53 = 1.0 / (1L << 53)

  private final val FLOAT_UNIT = 5.9604645E-8f // 0x1.0p-24 = 1.0 / (1L << 24)

  /**
    * @param seed seed for this pseudorandom generator
    * @return a new pseudorandom generator with provided {@code seed}.
    *       Different pseudorandom generators with same seeds will produce
    *       same pseudorandom sequences of values.
    *       Different pseudorandom generators with different seeds will very
    *       likely produce different pseudorandom sequences of values.
    */
  def apply(seed: Long): Random =
    new Random(seed)

  /**
    * @return a new random number generator using as a seed
    *         a value which will very likely be unique on each
    *         invocation of this factory method.
    */
  def apply(): Random =
    new Random()

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


/**
  * @param seed seed for this pseudorandom generator
  * @return a new pseudorandom generator with provided {@code seed}.
  *          Different pseudorandom generators with same seeds will produce
  *          same pseudorandom sequences of values.
  *          Different pseudorandom generators with different seeds will very
  *         likely produce different pseudorandom sequences of values.
  */
class Random(seed: Long) {
  // state of this prg
  private var s0: Long = 0L
  private var s1: Long = 0L
  private var s2: Long = 0L
  private var s3: Long = 0L

  private var haveNextNextGaussian: Boolean = false
  private var nextNextGaussian: Double = _

  // seeds this prg using an random.SplitMix64 generator, as recommended by http://xoshiro.di.unimi.it/
  private def setSeed(seed: Long): Unit = {
    val splitMix64Random = new util.random.splitMix64.Random(seed)
    s0 = splitMix64Random.nextLong()
    s1 = splitMix64Random.nextLong()
    s2 = splitMix64Random.nextLong()
    s3 = splitMix64Random.nextLong()

    haveNextNextGaussian = false
  }

  setSeed(seed)

  /**
    * Creates a new random number generator using as a seed
    * a value which will very likely be unique on each
    * invocation of this constructor.
    */
  def this() {
    this(Random.seedUniquifier() ^ System.nanoTime)
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Long}
    * value.
    *
    * @return a uniformly distributed pseudorandom {@code Long}
    *                                                      value
    */
  def nextLong(): Long = {
    var result = s1 + (s1 << 2)
    result = result << 7 | result >>> -7 // java.lang.Long.rotateLeft(result, 7)
    result += result << 3
    val t = s1 << 17
    s2 ^= s0
    s3 ^= s1
    s1 ^= s2
    s0 ^= s3
    s2 ^= t
    s3 = s3 << 45 | s3 >>> -45 // java.lang.Long.rotateLeft(s3, 45)
    result
  }

  // assumes n > 0
  private def internalNextLong(n: Long): Long = {
    val nMinus1 = n - 1
    while (true) {
      val long = nextLong() >>> 1
      val result = long % n
      if (long + nMinus1 - result >= 0)
        return result
    }
    0L // not reached
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Long}
    * value in range [0,{@code n})
    *
    * @param n exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Long}
    *         from 0 (inclusive) to {@code n} (exclusive)
    */
  def nextLong(n: Long): Long = {
    if (n <= 0)
      throw new IllegalArgumentException("nextLong: illegal bound " + n + " (must be positive)")
    internalNextLong(n)
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Int}
    * value in range [{@code Int.MinValue}, {@code Int.MaxValue}]
    *
    * @return a uniformly distributed pseudorandom {@code Int}
    *         value from {@code Int.MinValue} (inclusive) to
    *         {@code Int.MaxValue} (inclusive)
    */
  def nextInt(): Int =
    nextLong().toInt

  /**
    * Returns a uniformly distributed pseudorandom {@code Int}
    * value in range [0,{@code n})
    *
    * @param n exclusive upper bound of range
    * @return a uniformly distributed pseudorandom {@code Int}
    *   from 0 (inclusive) to {@code n} (exclusive)
    */
  def nextInt(n: Int): Int = {
    if (n <= 0)
      throw new IllegalArgumentException(s"nextInt: illegal upper bound $n (must be positive)")
    internalNextLong(n).toInt
  }

  /**
    * Returns a uniformly distributed pseudorandom {@code Double}
    * value in range [0.0,1.0)
    *
    * @return a uniformly distributed pseudorandom {@code Double}
    *          from 0.0 (inclusive) to 1.0 (exclusive)
    */
  def nextDouble(): Double =
    (nextLong() >>> 11) * Random.DOUBLE_UNIT

  def nextDoubleFast(): Double =
    java.lang.Double.longBitsToDouble(0x3FFL << 52 | nextLong() >>> 12) - 1.0

  /**
    * Returns a uniformly distributed pseudorandom {@code Float}
    * value in range [0.0,1.0)
    *
    * @return a uniformly distributed pseudorandom {@code Float}
    *         from 0.0 (inclusive) to 1.0 (exclusive)
    */
  def nextFloat(): Float =
    (nextLong() >>> 40) * Random.FLOAT_UNIT

  /**
    * Returns a uniformly distributed pseudorandom {@code Boolean}
    * value. {@code true} and {@code false} are produced with equal
    * probabilities
    *
    * @return a uniformly distributed pseudorandom {@code Boolean}
    *         value.
    */
  def nextBoolean(): Boolean =
    nextLong() < 0

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
    *                 {@code false} with probability 0.5
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
    *                 {@code false} with probability 1 - {@code p}
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
      * A fast normal random number generator
      *  ACM Transactions on Mathematical Software 18(4):449-453.
      * December 1992
      *
      */
    import Random.LevaConstants._
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

