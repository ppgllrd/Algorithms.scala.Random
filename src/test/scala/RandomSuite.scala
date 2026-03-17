// RandomSuite.scala
// Test suite for the xoshiro256** pseudorandom number generator and related utilities.
//
// This file contains:
//  1) Exhaustive functional tests
//  2) Extra **CI-friendly** statistical tests (fast, robust thresholds)
//  3) **Nightly** statistical tests (heavier, more sensitive)
//
// How to run with munit tags:
//   - Run default (CI-friendly only): sbt "testOnly -- -l nightly"
//   - Run Nightly only:               sbt "testOnly -- -n nightly"
//   - Run everything:                 sbt test
//
// Threshold notes:
//   * For chi-squared with df = k-1, the critical value at p=0.001 for df=9 is ~27.88.
//     We use 30.0 to add CI robustness against random fluctuations.
//   * For high-df chi-squared, we often use an approximation: df + z*sqrt(2*df)
//     with z in [5,7] for CI to avoid flaky failures.

import munit.FunSuite
import util.random.xoshiro256StarStar.Random

class RandomSuite extends FunSuite:

  // ────────────────────────────────────────────────────────────────────────────
  // Helpers (statistics)
  // ────────────────────────────────────────────────────────────────────────────

  /** Chi-squared statistic for discrete uniformity over k categories. */
  private def chiSquaredUniform(counts: Array[Int], n: Int): Double =
    val k        = counts.length
    val expected = n.toDouble / k
    var i = 0
    var sum = 0.0
    while i < k do
      val diff = counts(i) - expected
      sum += (diff * diff) / expected
      i += 1
    sum

  /** Kolmogorov–Smirnov D statistic for Uniform(0,1). */
  private def ksUniform01(xs: Array[Double]): Double =
    val n = xs.length
    scala.util.Sorting.quickSort(xs)
    var d  = 0.0
    var i  = 1
    while i <= n do
      val xi = xs(i - 1)
      val cdf = xi // Uniform(0,1)
      val dPlus  = i.toDouble / n - cdf
      val dMinus = cdf - (i - 1).toDouble / n
      val local  = math.max(dPlus, dMinus)
      if local > d then d = local
      i += 1
    d

  /** Approximate chi-squared threshold: df + z * sqrt(2*df). */
  private def chi2ThresholdApprox(df: Int, z: Double): Double =
    df + z * math.sqrt(2.0 * df)

  object Nightly extends munit.Tag("nightly")

  // ────────────────────────────────────────────────────────────────────────────
  // 1) Reproducibility
  // ────────────────────────────────────────────────────────────────────────────

  // What: Same seed must produce identical sequences.
  // Why: Reproducibility is essential; PRNGs are deterministic given the seed.
  test("same seed produces identical Long sequence") {
    val rnd1 = Random(42L)
    val rnd2 = Random(42L)
    val seq1 = List.fill(20)(rnd1.nextLong())
    val seq2 = List.fill(20)(rnd2.nextLong())
    assertEquals(seq1, seq2)
  }

  // What: Different seeds should produce different sequences (with overwhelming probability).
  // Why: Confirms the seed actually affects the internal state.
  test("different seeds produce different sequences") {
    val rnd1 = Random(1L)
    val rnd2 = Random(2L)
    val seq1 = List.fill(20)(rnd1.nextLong())
    val seq2 = List.fill(20)(rnd2.nextLong())
    assertNotEquals(seq1, seq2)
  }

  // What: Reseeding to the original value must reproduce the same sequence.
  // Why: Guarantees that `seed` setter resets the generator correctly.
  test("reseed with same value reproduces sequence") {
    val rnd   = Random(123L)
    val first = List.fill(10)(rnd.nextLong())
    rnd.seed = 123L
    val second = List.fill(10)(rnd.nextLong())
    assertEquals(first, second)
  }

  // What: `seed` getter/setter preserves the configured seed value.
  // Why: Validates the public API contract for state inspection.
  test("seed accessor returns value used for seeding") {
    val rnd = Random(999L)
    assertEquals(rnd.seed, 999L)
    rnd.seed = 777L
    assertEquals(rnd.seed, 777L)
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 2) nextLong / nextInt ranges & errors
  // ────────────────────────────────────────────────────────────────────────────

  // What: nextLong() should yield both negative and positive values.
  // Why: Sanity check that the full signed range is reachable in practice.
  test("nextLong() covers full Long range (signs)") {
    val rnd     = Random(0L)
    val samples = 10_000
    val values  = List.fill(samples)(rnd.nextLong())
    assert(values.exists(_ < 0), "expected some negative values")
    assert(values.exists(_ > 0), "expected some positive values")
  }

  // What: nextLong(n) returns values in [0, n).
  // Why: Bound handling must be correct and exclusive of n.
  test("nextLong(n) stays within [0, n)") {
    val rnd = Random(0L)
    val n   = 100L
    for _ <- 1 to 1_000 do
      val v = rnd.nextLong(n)
      assert(v >= 0L && v < n, s"nextLong($n) out of range: $v")
  }

  // What: nextLong(n) rejects non-positive bounds.
  // Why: API safety guardrails.
  test("nextLong(n) throws on non-positive bound") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.nextLong(0L) }
    intercept[IllegalArgumentException] { rnd.nextLong(-1L) }
  }

  // What: nextInt() should yield both negative and positive values.
  // Why: Sanity check on the signed Int range.
  test("nextInt() covers full Int range (signs)") {
    val rnd     = Random(0L)
    val samples = 10_000
    val values  = List.fill(samples)(rnd.nextInt())
    assert(values.exists(_ < 0), "expected some negative values")
    assert(values.exists(_ > 0), "expected some positive values")
  }

  // What: nextInt(n) returns values in [0, n).
  // Why: Boundaries must be correct.
  test("nextInt(n) stays within [0, n)") {
    val rnd = Random(0L)
    val n   = 10
    for _ <- 1 to 1_000 do
      val v = rnd.nextInt(n)
      assert(v >= 0 && v < n, s"nextInt($n) out of range: $v")
  }

  // What: nextInt(n) rejects non-positive bounds.
  // Why: API safety guardrails.
  test("nextInt(n) throws on non-positive bound") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.nextInt(0) }
    intercept[IllegalArgumentException] { rnd.nextInt(-5) }
  }

  // What: int(low, high) returns values in [low, high).
  // Why: Interval handling must be correct and half-open.
  test("int(low, high) stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.int(-10, 10)
      assert(v >= -10 && v < 10, s"int(-10, 10) out of range: $v")
  }

  // What: int(low, high) rejects high <= low.
  // Why: Guard against invalid intervals that would be empty or reversed.
  test("int(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.int(5, 5) }
    intercept[IllegalArgumentException] { rnd.int(10, 5) }
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 3) nextDouble / nextFloat ranges & errors
  // ────────────────────────────────────────────────────────────────────────────

  // What: nextDouble() is in [0,1).
  // Why: Core invariant of uniform floating generator.
  test("nextDouble() stays within [0.0, 1.0)") {
    val rnd = Random(0L)
    for _ <- 1 to 10_000 do
      val v = rnd.nextDouble()
      assert(v >= 0.0 && v < 1.0, s"nextDouble() out of range: $v")
  }

  // What: double(low,high) returns values in [low,high).
  // Why: Interval handling must be correct and half-open.
  test("double(low, high) stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.double(-5.0, 5.0)
      assert(v >= -5.0 && v < 5.0, s"double(-5.0, 5.0) out of range: $v")
  }

  // What: double(low,high) rejects high <= low.
  // Why: Guard against invalid intervals.
  test("double(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.double(1.0, 1.0) }
    intercept[IllegalArgumentException] { rnd.double(2.0, 1.0) }
  }

  // What: nextFloat() is in [0,1).
  // Why: Core invariant of uniform float generator.
  test("nextFloat() stays within [0.0f, 1.0f)") {
    val rnd = Random(0L)
    for _ <- 1 to 10_000 do
      val v = rnd.nextFloat()
      assert(v >= 0.0f && v < 1.0f, s"nextFloat() out of range: $v")
  }

  // What: float(low,high) returns Float in [low,high).
  // Why: Type correctness and interval correctness.
  test("float(low, high) returns Float and stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v: Float = rnd.float(2.0f, 8.0f)
      assert(v >= 2.0f && v < 8.0f, s"float(2f, 8f) out of range: $v")
  }

  // What: float(low,high) rejects high <= low.
  // Why: Guard against invalid intervals.
  test("float(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.float(3.0f, 3.0f) }
    intercept[IllegalArgumentException] { rnd.float(5.0f, 2.0f) }
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 4) Booleans & Uniform helpers
  // ────────────────────────────────────────────────────────────────────────────

  // What: nextBoolean() should eventually produce both true and false.
  // Why: Sanity check for bit extraction from PRNG.
  test("nextBoolean() produces both true and false") {
    val rnd     = Random(0L)
    val samples = 1_000
    val values  = List.fill(samples)(rnd.nextBoolean())
    assert(values.contains(true),  "expected true values")
    assert(values.contains(false), "expected false values")
  }

  // What: uniform(Int) is in [0,n).
  // Why: Bound correctness for convenience wrapper.
  test("uniform(n: Int) stays within [0, n)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.uniform(50)
      assert(v >= 0 && v < 50, s"uniform(50) out of range: $v")
  }

  // What: uniform(Long) is in [0,n).
  // Why: Bound correctness for long overload.
  test("uniform(n: Long) stays within [0, n)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.uniform(200L)
      assert(v >= 0L && v < 200L, s"uniform(200L) out of range: $v")
  }

  // What: uniform(Float,Float) returns Float in range.
  // Why: Type and bound correctness.
  test("uniform(low: Float, high: Float) returns Float and stays in range") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v: Float = rnd.uniform(1.0f, 9.0f)
      assert(v >= 1.0f && v < 9.0f, s"uniform(1f, 9f) out of range: $v")
  }

  // What: uniform(Seq) always returns one of the elements.
  // Why: Validates index mapping and bounds.
  test("uniform(Seq) picks a valid element") {
    val rnd  = Random(0L)
    val list = List("a", "b", "c", "d", "e")
    for _ <- 1 to 100 do
      val v = rnd.uniform(list)
      assert(list.contains(v), s"uniform(list) returned unexpected value: $v")
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 5) Bernoulli
  // ────────────────────────────────────────────────────────────────────────────

  // What: bernoulli() should be close to p=0.5 in frequency.
  // Why: Quick sanity check of unbiased boolean generation.
  test("bernoulli() is close to 0.5 frequency") {
    val rnd       = Random(0L)
    val n         = 1_000_000
    val trueCount = (1 to n).count(_ => rnd.bernoulli())
    val freq      = trueCount.toDouble / n
    assert(math.abs(freq - 0.5) < 0.005, s"bernoulli() frequency $freq far from 0.5")
  }

  // What: bernoulli(p) rejects invalid probabilities.
  // Why: Guard against inputs outside [0,1].
  test("bernoulli(p) throws on invalid probability") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.bernoulli(-0.1) }
    intercept[IllegalArgumentException] { rnd.bernoulli(1.1) }
  }

  // What: Edge case p=0 returns false always.
  // Why: Deterministic edge correctness.
  test("bernoulli(0.0) always returns false") {
    val rnd = Random(0L)
    assert((1 to 100).forall(_ => !rnd.bernoulli(0.0)))
  }

  // What: Edge case p=1 returns true always.
  // Why: Deterministic edge correctness.
  test("bernoulli(1.0) always returns true") {
    val rnd = Random(0L)
    assert((1 to 100).forall(_ => rnd.bernoulli(1.0)))
  }

  // What: bernoulli(p) matches the target probability p.
  // Why: Validates parameterization of the generator.
  test("bernoulli(p) frequency is close to p") {
    val rnd       = Random(0L)
    val n         = 1_000_000
    val p         = 0.3
    val trueCount = (1 to n).count(_ => rnd.bernoulli(p))
    val freq      = trueCount.toDouble / n
    assert(math.abs(freq - p) < 0.005, s"bernoulli($p) frequency $freq far from $p")
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 6) Gaussian (normal)
  // ────────────────────────────────────────────────────────────────────────────

  // What: gaussian() should have mean ≈ 0 and stddev ≈ 1.
  // Why: Validates shape and scaling of the standard normal generator.
  test("gaussian() has mean ~0 and stddev ~1") {
    val rnd     = Random(0L)
    val n       = 1_000_000
    val samples = Array.fill(n)(rnd.gaussian())
    val mean    = samples.sum / n
    val variance = samples.map(x => (x - mean) * (x - mean)).sum / n
    val stddev  = math.sqrt(variance)
    assert(math.abs(mean) < 0.005, s"gaussian() mean $mean not close to 0")
    assert(math.abs(stddev - 1.0) < 0.005, s"gaussian() stddev $stddev not close to 1")
  }

  // What: gaussian(mu,sigma) should have mean ≈ mu and stddev ≈ sigma.
  // Why: Validates shifting and scaling logic.
  test("gaussian(mu, sigma) has mean ~mu and stddev ~sigma") {
    val rnd     = Random(0L)
    val n       = 1_000_000
    val mu      = 5.0
    val sigma   = 2.0
    val samples = Array.fill(n)(rnd.gaussian(mu, sigma))
    val mean    = samples.sum / n
    val variance = samples.map(x => (x - mean) * (x - mean)).sum / n
    val stddev  = math.sqrt(variance)
    assert(math.abs(mean - mu) < 0.005, s"gaussian($mu,$sigma) mean $mean not close to $mu")
    assert(math.abs(stddev - sigma) < 0.005, s"gaussian($mu,$sigma) stddev $stddev not close to $sigma")
  }

  // What: gaussian via Box–Muller should also have mean ≈ 0 and stddev ≈ 1.
  // Why: Cross-check alternative algorithm implementation.
  test("gaussianBoxMuller() has mean ~0 and stddev ~1") {
    val rnd     = Random(0L)
    val n       = 1_000_000
    val samples = Array.fill(n)(rnd.gaussianBoxMuller())
    val mean    = samples.sum / n
    val variance = samples.map(x => (x - mean) * (x - mean)).sum / n
    val stddev  = math.sqrt(variance)
    assert(math.abs(mean) < 0.005, s"gaussianBoxMuller() mean $mean not close to 0")
    assert(math.abs(stddev - 1.0) < 0.005, s"gaussianBoxMuller() stddev $stddev not close to 1")
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 7) Exponential / Poisson / Pareto / Geometric
  // ────────────────────────────────────────────────────────────────────────────

  // What: Exponential(lambda) should have mean 1/lambda.
  // Why: Validates parameterization and transform correctness.
  test("exp(lambda) has mean close to 1/lambda") {
    val rnd    = Random(0L)
    val lambda = 2.0
    val n      = 1_000_000
    val mean   = Array.fill(n)(rnd.exp(lambda)).sum / n
    assert(math.abs(mean - 1.0 / lambda) < 0.005,
      s"exp($lambda) mean $mean not close to ${1.0 / lambda}")
  }

  // What: exp(lambda) rejects non-positive lambdas.
  // Why: Domain of the distribution.
  test("exp(lambda) throws on non-positive lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.exp(0.0) }
    intercept[IllegalArgumentException] { rnd.exp(-1.0) }
  }

  // What: exp(lambda) must be strictly positive.
  // Why: Support of the exponential distribution.
  test("exp(lambda) always produces positive values") {
    val rnd    = Random(0L)
    val lambda = 1.0
    for _ <- 1 to 10_000 do
      assert(rnd.exp(lambda) > 0, "exp() produced non-positive value")
  }

  // What: Poisson(lambda) should have mean ≈ lambda.
  // Why: Validates parameterization and algorithm.
  test("poisson(lambda) has mean close to lambda") {
    val rnd    = Random(0L)
    val lambda = 5.0
    val n      = 1_000_000
    val mean   = Array.fill(n)(rnd.poisson(lambda).toDouble).sum / n
    assert(math.abs(mean - lambda) < 0.005,
      s"poisson($lambda) mean $mean not close to $lambda")
  }

  // What: Poisson rejects non-positive lambda.
  // Why: Domain validation.
  test("poisson(lambda) throws on non-positive lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.poisson(0.0) }
    intercept[IllegalArgumentException] { rnd.poisson(-1.0) }
  }

  // What: Poisson rejects infinite lambda.
  // Why: Avoid degenerate/undefined behavior.
  test("poisson(lambda) throws on infinite lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.poisson(Double.PositiveInfinity) }
  }

  // What: Poisson outputs are non-negative integers.
  // Why: Support of the distribution.
  test("poisson(lambda) always produces non-negative values") {
    val rnd    = Random(0L)
    val lambda = 3.0
    for _ <- 1 to 1_000 do
      assert(rnd.poisson(lambda) >= 0, "poisson() produced negative value")
  }

  // What: Pareto rejects non-positive alpha.
  // Why: Domain validation.
  test("pareto(alpha) throws on non-positive alpha") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.pareto(0.0) }
    intercept[IllegalArgumentException] { rnd.pareto(-1.0) }
  }

  // What: Pareto outputs are non-negative.
  // Why: Support of the distribution.
  test("pareto(alpha) always produces non-negative values") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      assert(rnd.pareto(2.0) >= 0, "pareto() produced negative value")
  }

  // What: Geometric(p) (trials until first success) should have mean 1/p.
  // Why: Validates parameterization and transform correctness.
  test("geometric(p) has mean close to 1/p (trials until first success)") {
    val rnd = Random(0L)
    val p   = 0.25
    val n   = 1_000_000
    val mean = Array.fill(n)(rnd.geometric(p).toDouble).sum / n
    assert(math.abs(mean - 1.0 / p) < 0.005,
      s"geometric($p) mean $mean not close to ${1.0 / p}")
  }

  // What: Geometric rejects invalid p.
  // Why: Domain validation.
  test("geometric(p) throws on invalid probability") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.geometric(-0.1) }
    intercept[IllegalArgumentException] { rnd.geometric(1.1) }
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 8) Chi-squared uniformity for nextInt / nextDouble (bucketed)
  // ────────────────────────────────────────────────────────────────────────────

  // What: nextInt(k) should be uniform over 0..k-1 (first-order uniformity).
  // Why: Detects gross bias in categorical outputs.
  test("nextInt(k) passes chi-squared uniformity test (k=10)") {
    val rnd    = Random(0L)
    val k      = 10
    val n      = 1_000_000
    val counts = new Array[Int](k)
    for _ <- 1 to n do counts(rnd.nextInt(k)) += 1
    val stat = chiSquaredUniform(counts, n)
    // df=9; critical p=0.001 ≈ 27.88, use 30.0 for CI robustness
    assert(stat < 30.0, s"Chi-squared statistic $stat exceeds threshold (poor uniformity)")
  }

  // What: nextDouble() in [0,1) should be uniform (via 10 equal buckets).
  // Why: Simple bucket-based chi-squared as a quick smoke-test.
  test("nextDouble() passes chi-squared uniformity test (10 buckets)") {
    val rnd    = Random(0L)
    val k      = 10
    val n      = 1_000_000
    val counts = new Array[Int](k)
    for _ <- 1 to n do
      val bucket = (rnd.nextDouble() * k).toInt.min(k - 1)
      counts(bucket) += 1
    val stat = chiSquaredUniform(counts, n)
    assert(stat < 30.0, s"Chi-squared statistic $stat exceeds threshold (poor uniformity)")
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 9) Discrete / discreteProbabilities
  // ────────────────────────────────────────────────────────────────────────────

  // What: discrete(frequencies) returns indices within 0..len-1.
  // Why: Bounds correctness of categorical generator.
  test("discrete(frequencies) produces values in range") {
    val rnd  = Random(0L)
    val freq = Array(1, 2, 3, 4)
    val gen  = rnd.discrete(freq)
    for _ <- 1 to 1_000 do
      val v = gen()
      assert(v >= 0 && v < freq.length, s"discrete() out of range: $v")
  }

  // What: discrete(frequencies) honors relative weights.
  // Why: Validates proportionate sampling (ratio 1:3).
  test("discrete(frequencies) respects proportions (1:3)") {
    val rnd  = Random(0L)
    val freq = Array(1, 3)
    val gen  = rnd.discrete(freq)
    val n    = 1_000_000
    val counts = new Array[Int](2)
    for _ <- 1 to n do counts(gen()) += 1
    val ratio = counts(1).toDouble / counts(0).toDouble
    assert(math.abs(ratio - 3.0) < 0.005, s"discrete([1,3]) ratio $ratio not close to 3.0")
  }

  // What: discrete(frequencies) rejects negative weights.
  // Why: Input validation.
  test("discrete(frequencies) throws on negative frequency") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discrete(Array(1, -1, 2)) }
  }

  // What: discrete(frequencies) rejects all-zero weights.
  // Why: Degenerate case would be undefined.
  test("discrete(frequencies) throws when all frequencies are zero") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discrete(Array(0, 0, 0)) }
  }

  // What: discreteProbabilities returns valid indices.
  // Why: Bounds correctness under probability inputs.
  test("discreteProbabilities produces values in range") {
    val rnd   = Random(0L)
    val probs = Array(0.2, 0.5, 0.3)
    val gen   = rnd.discreteProbabilities(probs)
    for _ <- 1 to 1_000 do
      val v = gen()
      assert(v >= 0 && v < probs.length, s"discreteProbabilities() out of range: $v")
  }

  // What: discreteProbabilities requires probabilities summing to 1.
  // Why: Input validation and normalization expectations.
  test("discreteProbabilities throws when sum != 1") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discreteProbabilities(Array(0.3, 0.3)) }
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 10) Shuffle
  // ────────────────────────────────────────────────────────────────────────────

  // What: shuffleInPlace preserves all elements (multiset unchanged).
  // Why: Correctness of in-place Fisher–Yates implementation.
  test("shuffleInPlace preserves all elements") {
    val rnd    = Random(0L)
    val arr    = scala.collection.mutable.ArrayBuffer.from(1 to 20)
    val sorted = arr.toList.sorted
    rnd.shuffleInPlace(arr)
    assertEquals(arr.toList.sorted, sorted)
  }

  // What: shuffleInPlace changes order with overwhelming probability.
  // Why: Sanity check that it’s not a no-op.
  test("shuffleInPlace changes order with overwhelming probability") {
    val rnd  = Random(0L)
    val arr  = scala.collection.mutable.ArrayBuffer.from(1 to 20)
    val orig = arr.toList
    rnd.shuffleInPlace(arr)
    assertNotEquals(arr.toList, orig)
  }

  // ────────────────────────────────────────────────────────────────────────────
  // 11) SplitMix64 seeder
  // ────────────────────────────────────────────────────────────────────────────

  // What: SplitMix64 should not produce all zeros.
  // Why: Sanity check of the seeding PRNG used to initialize xoshiro state.
  test("SplitMix64 produces non-zero output") {
    val rnd    = util.random.splitMix64.Random(0L)
    val values = List.fill(10)(rnd.nextLong())
    assert(values.exists(_ != 0L), "SplitMix64 produced all zeros")
  }

  // What: SplitMix64 is reproducible for identical seeds.
  // Why: Determinism guarantee for the seeder generator too.
  test("SplitMix64 same seed produces same sequence") {
    val rnd1 = util.random.splitMix64.Random(12345L)
    val rnd2 = util.random.splitMix64.Random(12345L)
    assertEquals(
      List.fill(10)(rnd1.nextLong()),
      List.fill(10)(rnd2.nextLong())
    )
  }

  // ===========================================================================
  //                       CI-FRIENDLY EXTRA TESTS
  // ===========================================================================

  // What: Lag-1 serial correlation should be ~0.
  // Why: Checks first-order independence beyond marginal uniformity.
  test("nextInt(k) lag-1 serial correlation ~ 0 (independence)") {
    val rnd = Random(0L)
    val k   = 1 << 16
    val n   = 200_000
    val xs  = Array.fill(n)(rnd.nextInt(k).toDouble)
    val mean = xs.sum / n
    var num = 0.0
    var den = 0.0
    var i   = 1
    while i < n do
      num += (xs(i) - mean) * (xs(i - 1) - mean)
      i += 1
    i = 0
    while i < n do
      val d = xs(i) - mean
      den += d * d
      i += 1
    val rho = if den == 0 then 0.0 else num / den
    assert(math.abs(rho) < 0.005, s"lag-1 correlation $rho not ~ 0")
  }

  // What: Low bits (LSBs) should have ~50% ones.
  // Why: Many PRNGs fail on low-bit bias; we test 8 LSBs with a binomial tolerance.
  test("nextInt() low bits have ~50% ones (binomial tolerance)") {
    val rnd    = Random(0L)
    val n      = 500_000
    val bitMax = 8
    val ones   = Array.fill(bitMax)(0L)
    var i = 0
    while i < n do
      val v = rnd.nextInt()
      var b = 0
      while b < bitMax do
        if ((v >>> b) & 1) == 1 then ones(b) += 1
        b += 1
      i += 1
    val p     = n / 2.0
    val sigma = math.sqrt(n * 0.25)
    var b = 0
    while b < bitMax do
      val diff = math.abs(ones(b) - p)
      assert(diff < 5 * sigma, s"bit $b biased: ones=${ones(b)}/$n")
      b += 1
    end while
  }

  // What: nextInt(bound) should not be biased for non-powers-of-two bounds.
  // Why: Detects modulo bias if `%` is used naively.
  test("nextInt(n) shows no modulo bias for non-powers-of-two bounds") {
    val rnd       = Random(0L)
    val bounds    = Array(3, 5, 7, 10, 1000, 1001, 4095)
    val nSamples  = 300_000
    bounds.foreach { bound =>
      val counts = new Array[Int](bound)
      var i = 0
      while i < nSamples do
        counts(rnd.nextInt(bound)) += 1
        i += 1
      val expected = nSamples.toDouble / bound
      var chi2 = 0.0
      var j = 0
      while j < bound do
        val diff = counts(j) - expected
        chi2 += (diff * diff) / expected
        j += 1
      val df        = bound - 1
      val threshold = chi2ThresholdApprox(df, 5.0) // relaxed CI threshold
      assert(chi2 < threshold, s"bias suspected for n=$bound: chi2=$chi2 df=$df")
    }
  }

  // What: Runs test (Wald–Wolfowitz) should not show abnormal clustering.
  // Why: Tests independence in a binary stream beyond simple frequency.
  test("nextBoolean() passes runs test (no abnormal clustering)") {
    val rnd = Random(0L)
    val n   = 200_000

    var prev = rnd.nextBoolean()
    var runs = 1
    var n1   = if prev then 1 else 0
    var n0   = if prev then 0 else 1

    var i = 1
    while (i < n) {
      val v = rnd.nextBoolean()
      if (v != prev) runs += 1
      if (v) n1 += 1 else n0 += 1
      prev = v
      i += 1
    }

    val N = n0 + n1
    assert(N == n, s"Inconsistent counts: n0=$n0 n1=$n1 N=$N n=$n")

    val mu    = (2.0 * n0 * n1) / N + 1.0
    val numer = 2.0 * n0 * n1 * (2.0 * n0 * n1 - n0 - n1)
    val denom = (N.toDouble * N) * (N - 1).toDouble
    val varR  = if (denom == 0) 0.0 else numer / denom
    val sigma = math.sqrt(math.max(varR, 0.0))
    assert(sigma > 0.0, s"runs variance is zero: n0=$n0 n1=$n1 runs=$runs")

    val z = (runs - mu) / sigma
    assert(math.abs(z) < 5.0, s"runs test failed: runs=$runs z=$z n0=$n0 n1=$n1")
  }

  // What: After many shuffles, each position should be near-uniform over elements.
  // Why: Validates Fisher–Yates implementation does not bias positions.
  test("shuffleInPlace: each element's position is ~ uniform (k=10)") {
    val rnd     = Random(0L)
    val size    = 10
    val trials  = 50_000
    val pos     = Array.fill(size, size)(0)
    val base    = (0 until size).toArray
    var t = 0
    while t < trials do
      val arr = scala.collection.mutable.ArrayBuffer.from(base)
      rnd.shuffleInPlace(arr)
      var i = 0
      while i < size do
        pos(i)(arr(i)) += 1
        i += 1
      t += 1
    val expected = trials.toDouble / size
    var row = 0
    while row < size do
      var chi2 = 0.0
      var col = 0
      while col < size do
        val diff = pos(row)(col) - expected
        chi2 += (diff * diff) / expected
        col += 1
      assert(chi2 < 30.0, s"position $row not uniform: chi2=$chi2")
      row += 1
    end while
  }

  // What: KS test against Uniform(0,1) should pass with relaxed α≈0.001.
  // Why: Detects subtle distributional deviations without binning.
  test("nextDouble() passes Kolmogorov–Smirnov test (Uniform[0,1))") {
    val rnd = Random(0L)
    val n   = 50_000
    val xs  = Array.fill(n)(rnd.nextDouble())
    val d   = ksUniform01(xs)
    val threshold = 1.95 / math.sqrt(n)
    assert(d < threshold, s"KS D=$d >= $threshold")
  }

  // ===========================================================================
  //                           NIGHTLY HEAVY TESTS
  // ===========================================================================

  // What: 2D serial test on consecutive pairs (X_i, X_{i+1}) over a 20x20 grid.
  // Why: Detects serial dependence that 1D tests miss.
  test("nextDouble() serial 2D chi-square (20x20) [nightly]".tag(Nightly)) {
    val rnd = Random(123L)
    val k   = 20
    val n   = 2_000_000
    val cells = k * k
    val counts = Array.fill(cells)(0)
    var i   = 0
    var prev = rnd.nextDouble()
    while i < n do
      val x  = prev
      val y  = rnd.nextDouble()
      val ix = math.min((x * k).toInt, k - 1)
      val iy = math.min((y * k).toInt, k - 1)
      counts(ix * k + iy) += 1
      prev = y
      i += 1
    val expected = n.toDouble / cells
    var chi2 = 0.0
    var c = 0
    while c < cells do
      val diff = counts(c) - expected
      chi2 += (diff * diff) / expected
      c += 1
    val df        = cells - 1
    val threshold = chi2ThresholdApprox(df, 6.0)
    assert(chi2 < threshold, s"2D serial chi2=$chi2, df=$df")
  }

  // What: Tail frequencies |Z|>3 and |Z|>4 should match Normal(0,1) within tolerance.
  // Why: Ensures Gaussian generator has correct tail behavior (not too light/heavy).
  test("gaussian() tail frequencies reasonable (|Z|>3,>4) [nightly]".tag(Nightly)) {
    val rnd = Random(0L)
    val n   = 3_000_000
    var gt3 = 0L
    var gt4 = 0L
    var i   = 0
    while i < n do
      val z = rnd.gaussian()
      val a = math.abs(z)
      if a > 3.0 then gt3 += 1
      if a > 4.0 then gt4 += 1
      i += 1
    val p3 = 0.002699796
    val p4 = 6.334248e-5
    def within(count: Long, p: Double): Boolean =
      val exp   = n * p
      val sigma = math.sqrt(n * p * (1 - p))
      math.abs(count - exp) < 6 * sigma
    assert(within(gt3, p3), s"|Z|>3 frequency off: $gt3/$n")
    assert(within(gt4, p4), s"|Z|>4 frequency off: $gt4/$n")
  }

  // What: Bit autocorrelation at multiple lags should be ~0 for low bits.
  // Why: Catches subtle serial structure in LSBs.
  test("bit autocorrelation across multiple lags [nightly]".tag(Nightly)) {
    val rnd  = Random(0L)
    val n    = 2_000_000
    val bits = 0 to 7
    val xs   = Array.fill(n)(rnd.nextInt())
    val lags = Array(1, 2, 3, 5, 7)

    def corrBit(bit: Int, lag: Int): Double =
      val mean = 0.5
      var num  = 0.0
      var den  = 0.0
      var i    = lag
      while i < n do
        val a = ((xs(i)     >>> bit) & 1).toDouble
        val b = ((xs(i-lag) >>> bit) & 1).toDouble
        num += (a - mean) * (b - mean)
        i += 1
      val m = n - lag
      var j = 0
      while j < m do
        val a = ((xs(j) >>> bit) & 1).toDouble - mean
        den += a * a
        j += 1
      if den == 0 then 0.0 else num / den

    for bit <- bits do
      for lag <- lags do
        val r = corrBit(bit, lag)
        assert(math.abs(r) < 0.005, s"bit $bit lag $lag autocorr $r not ~ 0")
  }
