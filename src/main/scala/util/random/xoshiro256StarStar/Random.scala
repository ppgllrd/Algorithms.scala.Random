/**
 * ****************************************************************************
 * Pseudorandom generator based on xoshiro256** See http://xoshiro.di.unimi.it/
 *
 * Pepe Gallardo, 2024
 *
 * ***************************************************************************
 */

package util.random.xoshiro256StarStar

import java.util.concurrent.atomic.AtomicLong

object Random:
  private val _seedUniquifier = new AtomicLong(8682522807148012L)

  private def seedUniquifier(): Long =
    while true do
      val current = _seedUniquifier.get
      val next = current * 181783497276652981L
      if _seedUniquifier.compareAndSet(current, next) then
        return next

    0L // not reached

  private final val DOUBLE_UNIT = 1.1102230246251565e-16 // 0x1.0p-53 = 1.0 / (1L << 53)
  private final val FLOAT_UNIT = 5.9604645e-8f // 0x1.0p-24 = 1.0 / (1L << 24)

  /**
   * @param seed
   *   seed for this pseudorandom generator
   * @return
   *   a new pseudorandom generator seeded with provided seed. Different
   *   pseudorandom generators with same seeds will produce same pseudorandom
   *   sequences of values. Different pseudorandom generators with different
   *   seeds will very likely produce different pseudorandom sequences of
   *   values.
   */
  def apply(seed: Long): Random =
    new Random(seed)

  /**
   * @return
   *   a new random number generator using as a seed a value which will very
   *   likely be unique on each invocation of this factory method.
   */
  def apply(): Random =
    new Random()

/**
 * @param _seed
 *   seed for seeding this pseudorandom generator
 * @return
 *   a new pseudorandom generator with seeded with provided seed. Different
 *   pseudorandom generators with same seeds will produce same pseudorandom
 *   sequences of values. Different pseudorandom generators with different seeds
 *   will very likely produce different pseudorandom sequences of values.
 */
class Random(private var _seed: Long) extends util.random.RandomOps:
  // state of this pseudo random generator
  private var s0: Long = 0L
  private var s1: Long = 0L
  private var s2: Long = 0L
  private var s3: Long = 0L

  /**
   * Seeds this pseudo random generator using provided `seed` and a
   * random.SplitMix64 generator, as recommended by http://xoshiro.di.unimi.it
   * @param seed
   *   new seed for this pseudorandom generator
   */
  def seed_=(seed: Long): Unit =
    val splitMix64Random = util.random.splitMix64.Random(seed)
    s0 = splitMix64Random.nextLong()
    s1 = splitMix64Random.nextLong()
    s2 = splitMix64Random.nextLong()
    s3 = splitMix64Random.nextLong()
    _seed = seed
    haveNextNextGaussian = false

  /**
   * Returns the seed that was used for seeding this pseudo random generator.
   * @return
   *   the seed that was used for seeding this pseudo random generator.
   */
  def seed: Long = _seed

  // initially seeds using constructor's parameter
  seed = _seed

  /**
   * Creates a new random number generator using as a seed a value which will
   * very likely be unique on each invocation of this constructor.
   */
  def this() =
    this(Random.seedUniquifier() ^ System.nanoTime)

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range
   * [`Long.MinValue`, `Long.MaxValue`]
   *
   * @return
   *   a uniformly distributed pseudorandom `Long` value from `Long.MinValue`
   *   (inclusive) to `Long.MaxValue` (inclusive)
   */
  protected final def mixedNextLong(): Long =
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

  // assumes n > 0
  protected final def mixedNextLong(n: Long): Long =
    val nMinus1 = n - 1
    var result = 0L
    var stop = false
    while !stop do
      val long = mixedNextLong() >>> 1
      result = long % n
      if long + nMinus1 - result >= 0 then
        stop = true

    result

  /**
   * Returns a uniformly distributed pseudorandom Double value in range
   * [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Double from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  final protected def mixedNextDouble(): Double =
    (mixedNextLong() >>> 11) * Random.DOUBLE_UNIT

  final def nextDoubleFast(): Double =
    java.lang.Double.longBitsToDouble(0x3ffL << 52 | mixedNextLong() >>> 12) - 1.0

  /**
   * Returns a uniformly distributed pseudorandom Float value in range [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Float from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  final protected def mixedNextFloat(): Float =
    (nextLong() >>> 40) * Random.FLOAT_UNIT
