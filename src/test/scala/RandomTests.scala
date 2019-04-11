import util.random.xoShiRo256StarStar.Random

object TestFreqs extends App {
  val n = 6
  val counters = new Array[Int](n)

  val total = 100000000
  val rnd = Random(0)
  for(i <- 0 until total)
    counters(rnd.nextInt(n)) += 1

  val freqs = counters.map(_.toDouble / total)

  println(freqs.mkString(" "))
}

object Test extends App {
  val rnd = Random(0)

  for(i <- 0 until 500000000) {
    val x = rnd.nextInt()
  }

}


object TestGauss extends App {
  val rnd = Random()
  var sz = 1000000000

  var s = 0.0
  var sSqr = 0.0
  for(i <- 0 until sz) {
    val n = rnd.gaussian(5, 1) // (0, 1)
    // println(n)
    s += n
    sSqr += n*n
  }

  val mu = s / sz
  val sigma = math.sqrt((sSqr - sz*mu*mu)/ sz)
  println(mu, sigma)

}