import util.random.xoshiro256StarStar.Random

@main def TestFrequencies(): Unit =
  val n = 6
  val frequencies = new Array[Int](n)

  val total = 100000000
  val rnd = Random(0)
  for _ <- 0 until total do
    frequencies(rnd.nextInt(n)) += 1

  val relativeFrequencies = frequencies.map(_.toDouble / total)
  println(relativeFrequencies.mkString(" "))


@main def TestGauss(): Unit =
  val rnd = Random()
  val samples = 1000000000

  var sum = 0.0
  var sumSquare = 0.0
  timed(() =>
  for _ <- 0 until samples do
    val n = rnd.gaussian(5, 1) // (0, 1)
    sum += n
    sumSquare += n * n
  )
  val mu = sum / samples
  val sigma = math.sqrt((sumSquare - samples*mu*mu) / samples)
  println(s"$mu, $sigma")


def timed(p: () => Unit): Unit =
  val start = System.currentTimeMillis()
  p()
  val end = System.currentTimeMillis()
  println(s"Elapsed time: ${end - start} ms")

val runs = 1000000000

@main def TestXoShiRo256StarStar(): Unit =
  val rnd = util.random.xoshiro256StarStar.Random(0)
  timed(() =>
    for _ <- 0 until runs do
      val x = rnd.nextInt()
  )

@main def TestScalaUtil(): Unit =
  val rnd = scala.util.Random(0)

  timed(() =>
    for _ <- 0 until runs do
      val x = rnd.nextInt()
  )

@main def TestFloat(): Unit =
  val rnd = Random(0)
  for _ <- 1 to 10 do
    println(rnd.nextFloat())

@main def TestLong(): Unit =
  val rnd = Random(1)
  for _ <- 1 to 10 do
    println(rnd.nextLong())