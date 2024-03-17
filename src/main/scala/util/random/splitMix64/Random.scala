/** ****************************************************************************
  * Simple SplitMix64 generator used for seeding the random generator
  *
  * Pepe Gallardo, 2024
  *
  * ****************************************************************************/

package util.random.splitMix64

class Random(private var seed: Long):
  def nextLong(): Long = 
    seed += 0x9e3779b97f4a7c15L
    var z = seed
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    z ^ (z >>> 31)