import munit.FunSuite
import util.random.xoshiro256StarStar.Random

/** Test suite for the xoshiro256** pseudorandom number generator.
  *
  * Tests cover:
  *   - Deterministic reproducibility (same seed → same sequence)
  *   - Range/boundary correctness for all numeric types
  *   - Exception handling for invalid arguments
  *   - Statistical distribution quality via chi-squared and moment tests
  *   - Shuffle correctness (Fisher-Yates property)
  *   - Distribution generators (Gaussian, Bernoulli, Poisson, Exponential, etc.)
  */
class RandomSuite extends FunSuite:

  // ─── Reproducibility ───────────────────────────────────────────────────────

  test("same seed produces identical Long sequence") {
    val rnd1 = Random(42L)
    val rnd2 = Random(42L)
    val seq1 = List.fill(20)(rnd1.nextLong())
    val seq2 = List.fill(20)(rnd2.nextLong())
    assertEquals(seq1, seq2)
  }

  test("different seeds produce different sequences") {
    val rnd1 = Random(1L)
    val rnd2 = Random(2L)
    val seq1 = List.fill(20)(rnd1.nextLong())
    val seq2 = List.fill(20)(rnd2.nextLong())
    assertNotEquals(seq1, seq2)
  }

  test("reseed with same value reproduces sequence") {
    val rnd = Random(123L)
    val first = List.fill(10)(rnd.nextLong())
    rnd.seed = 123L
    val second = List.fill(10)(rnd.nextLong())
    assertEquals(first, second)
  }

  test("seed accessor returns value used for seeding") {
    val rnd = Random(999L)
    assertEquals(rnd.seed, 999L)
    rnd.seed = 777L
    assertEquals(rnd.seed, 777L)
  }

  // ─── nextLong ──────────────────────────────────────────────────────────────

  test("nextLong() covers full Long range") {
    val rnd     = Random(0L)
    val samples = 10_000
    val values  = List.fill(samples)(rnd.nextLong())
    assert(values.exists(_ < 0), "expected some negative values")
    assert(values.exists(_ > 0), "expected some positive values")
  }

  test("nextLong(n) stays within [0, n)") {
    val rnd = Random(0L)
    val n   = 100L
    for _ <- 1 to 1_000 do
      val v = rnd.nextLong(n)
      assert(v >= 0L && v < n, s"nextLong($n) out of range: $v")
  }

  test("nextLong(n) throws on non-positive bound") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.nextLong(0L) }
    intercept[IllegalArgumentException] { rnd.nextLong(-1L) }
  }

  // ─── nextInt ───────────────────────────────────────────────────────────────

  test("nextInt() covers full Int range") {
    val rnd     = Random(0L)
    val samples = 10_000
    val values  = List.fill(samples)(rnd.nextInt())
    assert(values.exists(_ < 0), "expected some negative values")
    assert(values.exists(_ > 0), "expected some positive values")
  }

  test("nextInt(n) stays within [0, n)") {
    val rnd = Random(0L)
    val n   = 10
    for _ <- 1 to 1_000 do
      val v = rnd.nextInt(n)
      assert(v >= 0 && v < n, s"nextInt($n) out of range: $v")
  }

  test("nextInt(n) throws on non-positive bound") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.nextInt(0) }
    intercept[IllegalArgumentException] { rnd.nextInt(-5) }
  }

  test("int(low, high) stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.int(-10, 10)
      assert(v >= -10 && v < 10, s"int(-10, 10) out of range: $v")
  }

  test("int(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.int(5, 5) }
    intercept[IllegalArgumentException] { rnd.int(10, 5) }
  }

  // ─── nextDouble ────────────────────────────────────────────────────────────

  test("nextDouble() stays within [0.0, 1.0)") {
    val rnd = Random(0L)
    for _ <- 1 to 10_000 do
      val v = rnd.nextDouble()
      assert(v >= 0.0 && v < 1.0, s"nextDouble() out of range: $v")
  }

  test("double(low, high) stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.double(-5.0, 5.0)
      assert(v >= -5.0 && v < 5.0, s"double(-5.0, 5.0) out of range: $v")
  }

  test("double(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.double(1.0, 1.0) }
    intercept[IllegalArgumentException] { rnd.double(2.0, 1.0) }
  }

  // ─── nextFloat ─────────────────────────────────────────────────────────────

  test("nextFloat() stays within [0.0f, 1.0f)") {
    val rnd = Random(0L)
    for _ <- 1 to 10_000 do
      val v = rnd.nextFloat()
      assert(v >= 0.0f && v < 1.0f, s"nextFloat() out of range: $v")
  }

  test("float(low, high) returns Float and stays within [low, high)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v: Float = rnd.float(2.0f, 8.0f) // must compile as Float
      assert(v >= 2.0f && v < 8.0f, s"float(2f, 8f) out of range: $v")
  }

  test("float(low, high) throws when high <= low") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.float(3.0f, 3.0f) }
    intercept[IllegalArgumentException] { rnd.float(5.0f, 2.0f) }
  }

  // ─── nextBoolean ───────────────────────────────────────────────────────────

  test("nextBoolean() produces both true and false") {
    val rnd     = Random(0L)
    val samples = 1_000
    val values  = List.fill(samples)(rnd.nextBoolean())
    assert(values.contains(true), "expected true values")
    assert(values.contains(false), "expected false values")
  }

  // ─── uniform overloads ─────────────────────────────────────────────────────

  test("uniform(n: Int) stays within [0, n)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.uniform(50)
      assert(v >= 0 && v < 50, s"uniform(50) out of range: $v")
  }

  test("uniform(n: Long) stays within [0, n)") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v = rnd.uniform(200L)
      assert(v >= 0L && v < 200L, s"uniform(200L) out of range: $v")
  }

  test("uniform(low: Float, high: Float) returns Float and stays in range") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      val v: Float = rnd.uniform(1.0f, 9.0f) // must compile as Float
      assert(v >= 1.0f && v < 9.0f, s"uniform(1f, 9f) out of range: $v")
  }

  test("uniform(Seq) picks a valid element") {
    val rnd  = Random(0L)
    val list = List("a", "b", "c", "d", "e")
    for _ <- 1 to 100 do
      val v = rnd.uniform(list)
      assert(list.contains(v), s"uniform(list) returned unexpected value: $v")
  }

  // ─── Bernoulli ─────────────────────────────────────────────────────────────

  test("bernoulli() is close to 0.5 frequency") {
    val rnd       = Random(0L)
    val n         = 100_000
    val trueCount = (1 to n).count(_ => rnd.bernoulli())
    val freq      = trueCount.toDouble / n
    assert(math.abs(freq - 0.5) < 0.01, s"bernoulli() frequency $freq far from 0.5")
  }

  test("bernoulli(p) throws on invalid probability") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.bernoulli(-0.1) }
    intercept[IllegalArgumentException] { rnd.bernoulli(1.1) }
  }

  test("bernoulli(0.0) always returns false") {
    val rnd = Random(0L)
    assert((1 to 100).forall(_ => !rnd.bernoulli(0.0)))
  }

  test("bernoulli(1.0) always returns true") {
    val rnd = Random(0L)
    assert((1 to 100).forall(_ => rnd.bernoulli(1.0)))
  }

  test("bernoulli(p) frequency is close to p") {
    val rnd       = Random(0L)
    val n         = 200_000
    val p         = 0.3
    val trueCount = (1 to n).count(_ => rnd.bernoulli(p))
    val freq      = trueCount.toDouble / n
    assert(math.abs(freq - p) < 0.005, s"bernoulli($p) frequency $freq far from $p")
  }

  // ─── Gaussian ──────────────────────────────────────────────────────────────

  test("gaussian() has mean close to 0 and stddev close to 1") {
    val rnd     = Random(0L)
    val n       = 200_000
    val samples = Array.fill(n)(rnd.gaussian())
    val mean    = samples.sum / n
    val variance =
      samples.map(x => (x - mean) * (x - mean)).sum / n
    assert(math.abs(mean) < 0.01, s"gaussian() mean $mean not close to 0")
    assert(math.abs(math.sqrt(variance) - 1.0) < 0.01,
      s"gaussian() stddev ${math.sqrt(variance)} not close to 1")
  }

  test("gaussian(mu, sigma) has mean close to mu and stddev close to sigma") {
    val rnd     = Random(0L)
    val n       = 200_000
    val mu      = 5.0
    val sigma   = 2.0
    val samples = Array.fill(n)(rnd.gaussian(mu, sigma))
    val mean    = samples.sum / n
    val variance =
      samples.map(x => (x - mean) * (x - mean)).sum / n
    assert(math.abs(mean - mu) < 0.02, s"gaussian($mu,$sigma) mean $mean not close to $mu")
    assert(math.abs(math.sqrt(variance) - sigma) < 0.02,
      s"gaussian($mu,$sigma) stddev ${math.sqrt(variance)} not close to $sigma")
  }

  test("gaussianBoxMuller() has mean close to 0 and stddev close to 1") {
    val rnd     = Random(0L)
    val n       = 200_000
    val samples = Array.fill(n)(rnd.gaussianBoxMuller())
    val mean    = samples.sum / n
    val variance =
      samples.map(x => (x - mean) * (x - mean)).sum / n
    assert(math.abs(mean) < 0.01, s"gaussianBoxMuller() mean $mean not close to 0")
    assert(math.abs(math.sqrt(variance) - 1.0) < 0.01,
      s"gaussianBoxMuller() stddev ${math.sqrt(variance)} not close to 1")
  }

  // ─── Exponential ───────────────────────────────────────────────────────────

  test("exp(lambda) has mean close to 1/lambda") {
    val rnd    = Random(0L)
    val lambda = 2.0
    val n      = 200_000
    val mean   = Array.fill(n)(rnd.exp(lambda)).sum / n
    assert(math.abs(mean - 1.0 / lambda) < 0.01,
      s"exp($lambda) mean $mean not close to ${1.0 / lambda}")
  }

  test("exp(lambda) throws on non-positive lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.exp(0.0) }
    intercept[IllegalArgumentException] { rnd.exp(-1.0) }
  }

  test("exp(lambda) always produces positive values") {
    val rnd    = Random(0L)
    val lambda = 1.0
    for _ <- 1 to 10_000 do
      assert(rnd.exp(lambda) > 0, "exp() produced non-positive value")
  }

  // ─── Poisson ───────────────────────────────────────────────────────────────

  test("poisson(lambda) has mean close to lambda") {
    val rnd    = Random(0L)
    val lambda = 5.0
    val n      = 200_000
    val mean   = Array.fill(n)(rnd.poisson(lambda).toDouble).sum / n
    assert(math.abs(mean - lambda) < 0.05,
      s"poisson($lambda) mean $mean not close to $lambda")
  }

  test("poisson(lambda) throws on non-positive lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.poisson(0.0) }
    intercept[IllegalArgumentException] { rnd.poisson(-1.0) }
  }

  test("poisson(lambda) throws on infinite lambda") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.poisson(Double.PositiveInfinity) }
  }

  test("poisson(lambda) always produces non-negative values") {
    val rnd    = Random(0L)
    val lambda = 3.0
    for _ <- 1 to 1_000 do
      assert(rnd.poisson(lambda) >= 0, "poisson() produced negative value")
  }

  // ─── Pareto ────────────────────────────────────────────────────────────────

  test("pareto(alpha) throws on non-positive alpha") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.pareto(0.0) }
    intercept[IllegalArgumentException] { rnd.pareto(-1.0) }
  }

  test("pareto(alpha) always produces non-negative values") {
    val rnd = Random(0L)
    for _ <- 1 to 1_000 do
      assert(rnd.pareto(2.0) >= 0, "pareto() produced negative value")
  }

  // ─── Geometric ─────────────────────────────────────────────────────────────

  test("geometric(p) has mean close to 1/p") {
    val rnd  = Random(0L)
    val p    = 0.25
    val n    = 200_000
    val mean = Array.fill(n)(rnd.geometric(p).toDouble).sum / n
    // geometric() uses the "trials until first success" convention: mean = 1/p
    assert(math.abs(mean - 1.0 / p) < 0.1,
      s"geometric($p) mean $mean not close to ${1.0 / p}")
  }

  test("geometric(p) throws on invalid probability") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.geometric(-0.1) }
    intercept[IllegalArgumentException] { rnd.geometric(1.1) }
  }

  // ─── Chi-squared uniformity test for nextInt ───────────────────────────────

  /** Chi-squared test for discrete uniform distribution.
    *
    * Returns the chi-squared statistic. For a uniform distribution over `k`
    * categories with `n` samples, the expected value of the statistic under H0
    * is `k − 1`.
    */
  private def chiSquaredUniform(counts: Array[Int], n: Int): Double =
    val k        = counts.length
    val expected = n.toDouble / k
    counts.map(c => (c - expected) * (c - expected) / expected).sum

  test("nextInt(n) passes chi-squared uniformity test") {
    val rnd    = Random(0L)
    val k      = 10
    val n      = 100_000
    val counts = new Array[Int](k)
    for _ <- 1 to n do counts(rnd.nextInt(k)) += 1
    val stat = chiSquaredUniform(counts, n)
    // Critical value for chi-squared with df=9 at p=0.001 is ~27.88.
    // A well-behaved PRNG should comfortably fall below this.
    assert(stat < 30.0, s"Chi-squared statistic $stat exceeds critical value (poor uniformity)")
  }

  test("nextDouble() passes chi-squared uniformity test (bucket analysis)") {
    val rnd    = Random(0L)
    val k      = 10
    val n      = 100_000
    val counts = new Array[Int](k)
    for _ <- 1 to n do
      val bucket = (rnd.nextDouble() * k).toInt.min(k - 1)
      counts(bucket) += 1
    val stat = chiSquaredUniform(counts, n)
    assert(stat < 30.0, s"Chi-squared statistic $stat exceeds critical value (poor uniformity)")
  }

  // ─── discrete / discreteProbabilities ─────────────────────────────────────

  test("discrete(frequencies) produces values in range") {
    val rnd  = Random(0L)
    val freq = Array(1, 2, 3, 4)
    val gen  = rnd.discrete(freq)
    for _ <- 1 to 1_000 do
      val v = gen()
      assert(v >= 0 && v < freq.length, s"discrete() out of range: $v")
  }

  test("discrete(frequencies) respects proportions") {
    val rnd    = Random(0L)
    val freq   = Array(1, 3) // ratio 1:3
    val gen    = rnd.discrete(freq)
    val n      = 100_000
    val counts = new Array[Int](2)
    for _ <- 1 to n do counts(gen()) += 1
    val ratio = counts(1).toDouble / counts(0).toDouble
    assert(math.abs(ratio - 3.0) < 0.1,
      s"discrete([1,3]) ratio $ratio not close to 3.0")
  }

  test("discrete(frequencies) throws on negative frequency") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discrete(Array(1, -1, 2)) }
  }

  test("discrete(frequencies) throws when all frequencies are zero") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discrete(Array(0, 0, 0)) }
  }

  test("discreteProbabilities produces values in range") {
    val rnd   = Random(0L)
    val probs = Array(0.2, 0.5, 0.3)
    val gen   = rnd.discreteProbabilities(probs)
    for _ <- 1 to 1_000 do
      val v = gen()
      assert(v >= 0 && v < probs.length, s"discreteProbabilities() out of range: $v")
  }

  test("discreteProbabilities throws when sum ≠ 1") {
    val rnd = Random(0L)
    intercept[IllegalArgumentException] { rnd.discreteProbabilities(Array(0.3, 0.3)) }
  }

  // ─── shuffleInPlace ────────────────────────────────────────────────────────

  test("shuffleInPlace preserves all elements") {
    val rnd    = Random(0L)
    val arr    = scala.collection.mutable.ArrayBuffer.from(1 to 20)
    val sorted = arr.toList.sorted
    rnd.shuffleInPlace(arr)
    assertEquals(arr.toList.sorted, sorted)
  }

  test("shuffleInPlace changes order with overwhelming probability") {
    val rnd = Random(0L)
    val arr = scala.collection.mutable.ArrayBuffer.from(1 to 20)
    val orig = arr.toList
    rnd.shuffleInPlace(arr)
    assertNotEquals(arr.toList, orig)
  }

  // ─── SplitMix64 seeder ─────────────────────────────────────────────────────

  test("SplitMix64 produces non-zero output") {
    val rnd = util.random.splitMix64.Random(0L)
    val values = List.fill(10)(rnd.nextLong())
    assert(values.exists(_ != 0L), "SplitMix64 produced all zeros")
  }

  test("SplitMix64 same seed produces same sequence") {
    val rnd1 = util.random.splitMix64.Random(12345L)
    val rnd2 = util.random.splitMix64.Random(12345L)
    assertEquals(
      List.fill(10)(rnd1.nextLong()),
      List.fill(10)(rnd2.nextLong())
    )
  }
