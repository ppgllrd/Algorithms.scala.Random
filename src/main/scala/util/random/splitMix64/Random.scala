/** ****************************************************************************
  * Simple SplitMix64 generator used for seeding the random generator
  *
  * Pepe Gallardo, 2024
  *
  * ****************************************************************************/

package util.random.splitMix64

/** A simple SplitMix64 pseudorandom number generator, used internally to
  * initialize the state of the xoshiro256** generator from a single 64-bit
  * seed.
  *
  * @param seed
  *   the 64-bit seed for this generator
  */
class Random(private var seed: Long):
  /** Returns the next pseudorandom `Long` value from this SplitMix64 generator.
    *
    * @return
    *   a pseudorandom `Long` value
    */
  def nextLong(): Long =
    seed += 0x9e3779b97f4a7c15L
    var z = seed
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)