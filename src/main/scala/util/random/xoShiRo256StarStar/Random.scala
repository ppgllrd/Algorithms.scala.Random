/** ****************************************************************************
  * Pseudorandom generator based on xoShiRo256**
  * See http://xoshiro.di.unimi.it/
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package util.random.xoShiRo256StarStar

import java.util.concurrent.atomic.AtomicLong

object Random {
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
}


/**
  * @param seed seed for this pseudorandom generator
  * @return a new pseudorandom generator with provided {@code seed}.
  *         Different pseudorandom generators with same seeds will produce
  *         same pseudorandom sequences of values.
  *         Different pseudorandom generators with different seeds will very
  *         likely produce different pseudorandom sequences of values.
  */
class Random(seed: Long) extends util.random.RandomOps {
  // state of this prg
  private var s0: Long = 0L
  private var s1: Long = 0L
  private var s2: Long = 0L
  private var s3: Long = 0L

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
    *         value
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
  protected final def internalNextLong(n: Long): Long = {
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
    *         from 0 (inclusive) to {@code n} (exclusive)
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
    *         from 0.0 (inclusive) to 1.0 (exclusive)
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
}