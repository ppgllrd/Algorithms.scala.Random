/**
 * ****************************************************************************
 * Different distribution generators that can be mixed with arbitrary
 * pseudorandom generators
 *
 * Pepe Gallardo, 2019
 *
 * ***************************************************************************
 */

package util.random

object RandomOps:
  // constants for generating gaussian random values using Leva's algorithm
  private object LevaConstants:
    private final val r = StrictMath.sqrt(2 / StrictMath.E)
    final val twiceR = 2 * r
    final val s = 0.449871
    final val t = -0.386595
    final val r1 = 0.27597
    final val r2 = 0.27846
    final val a = 0.19600
    final val b = 0.25472

trait RandomOps:
  // should be provided for any class mixing in this trait
  protected def mixedNextLong(): Long
  protected def mixedNextLong(n: Long): Long
  protected def mixedNextDouble(): Double
  protected def mixedNextFloat(): Float

  // for implementing a gaussian generator using Box-Muller algorithm
  protected var haveNextNextGaussian: Boolean = false
  private var nextNextGaussian: Double = 0

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range
   * [`Long.MinValue`, `Long.MaxValue`]
   *
   * @return
   *   a uniformly distributed pseudorandom `Long` value from `Long.MinValue`
   *   (inclusive) to `Long.MaxValue` (inclusive)
   */
  inline def nextLong(): Long =
    mixedNextLong()

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Long` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def nextLong(n: Long): Long =
    if n <= 0 then
      throw IllegalArgumentException("nextLong: illegal bound " + n + " (must be positive)")
    mixedNextLong(n)

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range
   * [`Int.MinValue`, `Int.MaxValue`]
   *
   * @return
   *   a uniformly distributed pseudorandom `Int` value from `Int.MinValue`
   *   (inclusive) to `Int.MaxValue` (inclusive)
   */
  inline def nextInt(): Int =
    mixedNextLong().toInt

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom Int from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def nextInt(n: Int): Int =
    if n <= 0 then
      throw IllegalArgumentException(s"nextInt: illegal upper bound $n (must be positive)")
    mixedNextLong(n).toInt

  /**
   * Returns a uniformly distributed pseudoRandom `Double` value in range
   * [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Double from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  inline def nextDouble(): Double =
    mixedNextDouble()

  /**
   * Returns a uniformly distributed pseudorandom Float value in range [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Float from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  def nextFloat(): Float =
    mixedNextFloat()

  /**
   * Returns a uniformly distributed pseudorandom `Boolean` value. `true` and
   * `false` are produced with equal probabilities
   *
   * @return
   *   a uniformly distributed pseudorandom `Boolean` value.
   */
  def nextBoolean(): Boolean =
    mixedNextLong() < 0

  /**
   * Returns a uniformly distributed pseudorandom `Boolean` value. `true` and
   * `false` are produced with equal probabilities
   *
   * @return
   *   a uniformly distributed pseudorandom `Boolean` value.
   */
  def boolean(): Boolean =
    mixedNextLong() < 0

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom Int from 0 (inclusive) to `n`
   *   (exclusive)
   */
  inline def int(): Int =
    mixedNextLong().toInt

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Int` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  inline def int(n: Int): Int =
    if n <= 0 then
      throw IllegalArgumentException(s"int: illegal upper bound $n (must be positive)")
    mixedNextLong(n).toInt

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Int` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def int(low: Int, high: Int): Int =
    if high <= low then
      throw IllegalArgumentException(s"int: illegal range [$low,$high)")
    low + mixedNextLong(high - low).toInt

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range
   * [`Long.MinValue`, `Long.MaxValue`]
   *
   * @return
   *   a uniformly distributed pseudorandom `Long` value from `Long.MinValue`
   *   (inclusive) to `Long.MaxValue` (inclusive)
   */
  inline def long(): Long =
    mixedNextLong()

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Long` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  inline def long(n: Long): Long =
    if n <= 0 then
      throw IllegalArgumentException("long: illegal bound " + n + " (must be positive)")
    mixedNextLong(n)

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Long` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def long(low: Long, high: Long): Long =
    if high <= low then
      throw IllegalArgumentException(s"long: illegal range [$low,$high)")
    low + mixedNextLong(high - low)

  /**
   * Returns a uniformly distributed pseudoRandom `Double` value in range
   * [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Double from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  inline def double(): Double =
    mixedNextDouble()

  /**
   * Returns a uniformly distributed pseudorandom `Double` value in range
   * [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Double` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def double(n: Double): Double =
    if n <= 0 then
      throw IllegalArgumentException(s"double: illegal upper bound $n (must be positive)")
    mixedNextDouble() * n

  /**
   * Returns a uniformly distributed pseudorandom `Double` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Double` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def double(low: Double, high: Double): Double =
    if high <= low then
      throw IllegalArgumentException(s"double: illegal range [$low,$high)")
    low + mixedNextDouble() * (high - low)

  /**
   * Returns a uniformly distributed pseudorandom Float value in range [0.0,1.0)
   *
   * @return
   *   a uniformly distributed pseudorandom Float from 0.0 (inclusive) to 1.0
   *   (exclusive)
   */
  def float(): Float =
    mixedNextFloat()

  /**
   * Returns a uniformly distributed pseudorandom `Float` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Float` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def float(n: Float): Float =
    if n <= 0 then
      throw IllegalArgumentException(s"float: illegal upper bound $n (must be positive)")
    mixedNextFloat() * n

  /**
   * Returns a uniformly distributed pseudorandom `Float` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Float` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def float(low: Float, high: Float): Double =
    if high <= low then
      throw IllegalArgumentException(s"float: illegal range [$low,$high)")
    low + mixedNextFloat() * (high - low)

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Long` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def uniform(n: Long): Long =
    if n <= 0 then
      throw IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    mixedNextLong(n)

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Int` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def uniform(n: Int): Int =
    if n <= 0 then
      throw IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    mixedNextLong(n).toInt

  /**
   * Returns a uniformly distributed pseudorandom `Double` value in range
   * [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Double` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def uniform(n: Double): Double =
    if n <= 0 then
      throw IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    mixedNextDouble() * n

  /**
   * Returns a uniformly distributed pseudorandom `Float` value in range [0,`n`)
   *
   * @param n
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Float` from 0 (inclusive) to `n`
   *   (exclusive)
   */
  def uniform(n: Float): Float =
    if n <= 0 then
      throw IllegalArgumentException(s"uniform: illegal upper bound $n (must be positive)")
    mixedNextFloat() * n

  /**
   * Returns a uniformly distributed pseudorandom `Long` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Long` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def uniform(low: Long, high: Long): Long =
    if high <= low then
      throw IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + mixedNextLong(high - low)

  /**
   * Returns a uniformly distributed pseudorandom `Int` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Int` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def uniform(low: Int, high: Int): Int =
    if high <= low then
      throw IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + mixedNextLong(high - low).toInt

  /**
   * Returns a uniformly distributed pseudorandom `Double` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Double` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def uniform(low: Double, high: Double): Double =
    if high <= low then
      throw IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + mixedNextDouble() * (high - low)

  /**
   * Returns a uniformly distributed pseudorandom `Float` value in range
   * [`low`,`high`)
   *
   * @param low
   *   inclusive lower bound of range
   * @param high
   *   exclusive upper bound of range
   * @return
   *   a uniformly distributed pseudorandom `Float` from `low` (inclusive) to
   *   `high` (exclusive)
   */
  def uniform(low: Float, high: Float): Double =
    if high <= low then
      throw IllegalArgumentException(s"uniform: illegal range [$low,$high)")
    low + mixedNextFloat() * (high - low)

  /**
   * Returns an uniformly chosen pseudorandom element from all values in
   * sequence `xs`
   *
   * @param xs
   *   sequence of values to choose from
   * @return
   *   an uniformly chosen pseudorandom element from all values in sequence `xs`
   */
  def uniform[A](xs: Seq[A]): A =
    xs(mixedNextLong(xs.length).toInt)

  /**
   * Returns an uniformly chosen pseudorandom element from all values in given
   * `Iterable`.
   *
   * @param iterable
   *   `Iterable` of elements to choose from.
   * @return
   *   A random element taken from `iterable`. Uniform distribution among all
   *   elements in `iterable`.
   */
  def uniform[A](iterable: collection.Iterable[A]): A =
    val it = iterable.iterator

    val skip = mixedNextLong(iterable.size).toInt
    for _ <- 0 until skip do it.next()

    it.next()

  /**
   * Result will either be `true` ot `false` with same probability (0.5). In
   * other words, returns `true` according to a Bernoulli distribution
   * [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli distribution]]
   * with probability of success 0.5
   *
   * @return
   *   `true` of `false` with same probability (0.5)
   */
  def bernoulli(): Boolean =
    mixedNextDouble() < 0.5

  /**
   * Result will be `true` with probability `p` and `false` with probability 1 -
   * `p`. In other words, returns `true` according to a Bernoulli distribution
   * [[https://en.wikipedia.org/wiki/Bernoulli_distribution Bernoulli distribution]]
   * with probability of success `p`
   *
   * @param p
   *   probability of success. Must be in [0.0,1.0]
   * @return
   *   `true` with probability `p` and `false` with probability 1 - `p`
   */
  def bernoulli(p: Double): Boolean =
    if (p < 0.0 || p > 1.0)
      throw IllegalArgumentException("bernoulli: success probability must be in [0,1]")
    mixedNextDouble() < p

  /**
   * Returns a random `Int` value with Geometric distribution.
   *
   * @param p
   *   success probability.
   * @return
   *   A random `Int` value. Geometric (with success probability of p)
   *   distribution.
   */
  def geometric(p: Double): Int =
    if p < 0.0 || p > 1.0 then
      throw IllegalArgumentException("geometric: success probability must be in 0.0 to 1.0 range")
    else
      // using algorithm given by Knuth
      scala.math.ceil(scala.math.log(mixedNextDouble()) / scala.math.log(1.0 - p)).toInt

  /**
   * Returns a Gaussian ("normally") distributed pseudorandom `Double` value
   * with mean 0.0 and standard deviation 1.0. Uses Box-Muller's algorithm.
   *
   * @return
   *   a pseudorandom `Double` value distributed accordingly to a normal
   *   distribution with mean 0.0 and standard deviation 1.0
   */
  def gaussianBoxMuller(): Double =
    if haveNextNextGaussian then
      haveNextNextGaussian = false
      nextNextGaussian
    else
      var stop = false
      var result = 0.0
      while !stop do
        val v1 = 2 * mixedNextDouble() - 1 // between -1 and 1
        val v2 = 2 * mixedNextDouble() - 1
        val s = v1 * v1 + v2 * v2
        if s < 1.0 && s != 0.0 then
          val multiplier = StrictMath.sqrt(-2 * StrictMath.log(s) / s)
          nextNextGaussian = v2 * multiplier
          haveNextNextGaussian = true
          result = v1 * multiplier
          stop = true

      result

  /**
   * Returns a Gaussian ("normally") distributed pseudorandom `Double` value
   * with mean `mu` and standard deviation `sigma`. Uses Box-Muller's algorithm.
   *
   * @return
   *   a pseudorandom `Double` value distributed accordingly to a normal
   *   distribution with mean `mu` and standard deviation `sigma`.
   */
  def gaussianBoxMuller(mu: Double, sigma: Double): Double =
    mu + sigma * gaussianBoxMuller()

  /**
   * Returns a Gaussian ("normally") distributed pseudorandom `Double` value
   * with mean 0.0 and standard deviation 1.0. Uses Joseph L. Leva's algorithm
   * and is about 20% faster than Box-Muller's.
   *
   * @return
   *   a pseudorandom `Double` value distributed accordingly to a normal
   *   distribution with mean 0.0 and standard deviation 1.0
   */
  def gaussian(): Double =
    /**
     * Joseph L. Leva's algorithm A fast normal random number generator. ACM
     * Transactions on Mathematical Software 18(4):449-453. December 1992
     */
    import RandomOps.LevaConstants.*
    var stop = false
    var result = 0.0
    while !stop do
      val u = nextDouble()
      val v = twiceR * (0.5 - mixedNextDouble())
      val x = u - s
      val y = StrictMath.abs(v) - t
      val q = x * x + y * (a * y - b * x)
      if q < r1 || (q <= r2 && v * v <= -4 * u * u * StrictMath.log(u)) then
        result = v / u
        stop = true

    result

  /**
   * Returns a Gaussian ("normally") distributed pseudorandom `Double` value
   * with mean `mu` and standard deviation `sigma`. Uses Joseph L. Leva's
   * algorithm.
   *
   * @param mu
   *   mean of distribution
   * @param sigma
   *   standard deviation of distribution
   * @return
   *   a pseudorandom `Double` value distributed accordingly to a normal
   *   distribution with mean `mu` and standard deviation `sigma`.
   */
  def gaussian(mu: Double, sigma: Double): Double =
    mu + sigma * gaussian()

  /**
   * Returns a `Double` value with Lognormal distribution (mean 0.0 and standard
   * deviation 1.0).
   *
   * @param mu
   *   mean of distribution
   * @param sigma
   *   standard deviation of distribution
   * @return
   *   Random `Double` value. Lognormal distribution (default mean 0.0 and
   *   standard deviation 1.0).
   */
  def lognormal(mu: Double = 0, sigma: Double = 1): Double =
    scala.math.exp(gaussian(mu, sigma))

  /**
   * Returns a `Double` value with Exponential distribution (`lambda` is rate
   * parameter).
   *
   * @param lambda
   *   rate parameter.
   * @return
   *   Random `Double` value. Exponential distribution (`lambda` is rate
   *   parameter).
   */
  def exp(lambda: Double): Double =
    if lambda <= 0.0 then throw IllegalArgumentException("exp: lambda must be greater than 0")
    else -scala.math.log(1.0 - mixedNextDouble()) / lambda

  /**
   * Returns a `Double` value with Weibull distribution (`alpha` is scale and
   * `beta` is shape).
   *
   * @param alpha
   *   scale.
   * @param beta
   *   shape.
   * @return
   *   Random `Double` value. Weibull distribution (`alpha` is scale and `beta`
   *   is shape).
   */
  def weibull(alpha: Double, beta: Double): Double =
    alpha * scala.math.pow(-scala.math.log(1.0 - mixedNextDouble()), 1.0 / beta)

  /**
   * Returns a `Double` value with Poisson distribution (`lambda` is rate
   * parameter).
   *
   * @param lambda
   *   rate parameter.
   * @return
   *   Random `Double` value. Poisson distribution (`lambda` is rate parameter).
   */
  def poisson(lambda: Double): Int =
    if lambda <= 0.0 then
      throw IllegalArgumentException("poisson: lambda must be greater than 0")
    if lambda.isInfinite then throw IllegalArgumentException("poisson: lambda must be finite")
    // using algorithm given by Knuth
    // see http://en.wikipedia.org/wiki/Poisson_distribution
    var k: Int = 0
    var p = 1.0
    val L = scala.math.exp(-lambda)
    var end = false
    while !end do
      k += 1
      p *= mixedNextDouble()
      end = p < L

    k - 1

  /**
   * Returns a `Double` value with Cauchy distribution.
   *
   * @return
   *   Random `Double` value. Cauchy distribution.
   */
  def cauchy(): Double =
    scala.math.tan(scala.math.Pi * (mixedNextDouble() - 0.5))

  /**
   * Returns a `Double` value with Pareto distribution.
   *
   * @param alpha
   *   shape parameter
   * @return
   *   Random `Double` value from Pareto distribution (`alpha` is shape
   *   parameter).
   */
  def pareto(alpha: Double): Double =
    if alpha <= 0.0 then throw IllegalArgumentException("pareto: alpha must be greater than 0")
    else scala.math.pow(1 - mixedNextDouble(), -1.0 / alpha) - 1.0

  /**
   * Constructs an impure function returning random `Int`s from 0 to
   * `frequencies.length` - 1.
   *
   * @param frequencies
   *   array defining frequencies for each possible random outcome.
   * @return
   *   An impure function returning random `Int`s from 0 (inclusive) until
   *   `frequencies.length` (exclusive). Probability of value i is proportional
   *   to `frequencies(i)`.
   */
  def discrete(frequencies: Array[Int]): () => Int =
    discrete(frequencies.toIndexedSeq)

  /**
   * Constructs an impure function returning random `Int`s from 0 to
   * `frequencies.length` - 1.
   *
   * @param frequencies
   *   sequence defining frequencies for each possible random outcome.
   * @return
   *   An impure function returning random `Int`s from 0 (inclusive) until
   *   `frequencies.length` (exclusive). Probability of value i is proportional
   *   to `frequencies(i)`.
   */
  def discrete(frequencies: Seq[Int]): () => Int =
    val cumulativeFrequencies = new Array[Long](frequencies.length)
    var observations: Long = 0L
    for i <- cumulativeFrequencies.indices do
      val freq = frequencies(i)
      if freq < 0 then
        throw IllegalArgumentException(s"All frequencies must be positive values but found $freq.")
      observations += freq
      cumulativeFrequencies(i) = observations

    if observations == 0 then
      throw IllegalArgumentException("At least one frequency must be greater than 0")
    if observations >= Integer.MAX_VALUE then
      throw IllegalArgumentException("Sum of frequencies is too large")

    def searcher(): Int =
      val target = mixedNextLong(observations)
      var left = 0
      var right = cumulativeFrequencies.length - 1
      var index = -1
      while left <= right do
        val center = left + (right - left) / 2
        if cumulativeFrequencies(center) <= target then left = center + 1
        else
          index = center
          right = center - 1
      index

    searcher

  /**
   * Constructs an impure function returning random `Int`s from 0 to
   * `frequencies.length` - 1.
   *
   * @param probabilities
   *   array defining probabilities for each possible random outcome.
   * @return
   *   An impure function returning random `Int`s from 0 (inclusive) until
   *   `probabilities.length` (exclusive). Probability of value i is
   *   `probabilities(i)`.
   */
  def discreteProbabilities(probabilities: Array[Double]): () => Int =
    discreteProbabilities(probabilities.toIndexedSeq)

  /**
   * Constructs an impure function returning random `Int`s from 0 to
   * `frequencies.length` - 1.
   *
   * @param probabilities
   *   sequence defining probabilities for each possible random outcome.
   * @return
   *   An impure function returning random `Int`s from 0 (inclusive) until
   *   `probabilities.length` (exclusive). Probability of value i is
   *   `probabilities(i)`.
   */
  def discreteProbabilities(probabilities: Seq[Double]): () => Int =
    val cumulativeDistribution = new Array[Double](probabilities.length)
    var totalProb: Double = 0
    for i <- cumulativeDistribution.indices do
      val prob = probabilities(i)
      if prob < 0 then
        throw IllegalArgumentException(
          s"All probabilities must be positive values but found $prob."
        )
      totalProb += prob
      cumulativeDistribution(i) = totalProb

    val epsilon = 1e-10
    if totalProb > 1.0 + epsilon || totalProb < 1.0 - epsilon then
      throw IllegalArgumentException(
        s"Sum of all probabilities must be close to 1.0 but it is $totalProb"
      )

    def searcher(): Int =
      var index = -1
      while index < 0 do
        val target = mixedNextDouble()
        var left = 0
        var right = cumulativeDistribution.length - 1
        while left <= right do
          val center = left + (right - left) / 2
          if cumulativeDistribution(center) <= target then left = center + 1
          else
            index = center
            right = center - 1
      index

    searcher

  /**
   * Randomly shuffles contents of Sequence seq.
   *
   * @param seq
   *   sequence of elements to shuffle.
   */
  def shuffleInPlace[A](seq: scala.collection.mutable.Seq[A]): Unit =
    val l = seq.length
    for i <- 0 until l - 1 do
      val j = i + mixedNextLong(l - i).toInt
      val tmp = seq(i)
      seq(i) = seq(j)
      seq(j) = tmp
