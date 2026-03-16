# Algorithms.scala.Random

Scala implementation of [xoshiro256**](http://xoshiro.di.unimi.it/xoshiro256starstar.c) pseudo-random number generator.

More than 4 times faster than `scala.util.Random`.

## Features

- **xoshiro256\*\*** – a state-of-the-art 256-bit generator with excellent statistical properties.
- **SplitMix64** – used internally to expand a single 64-bit seed into the full 256-bit generator state, as recommended by the original authors.
- Rich API: uniform integers, longs, doubles, floats, booleans; Bernoulli, Gaussian (Leva and Box-Muller), exponential, Poisson, geometric, Pareto, Weibull, Cauchy, lognormal distributions; discrete / weighted distributions; in-place shuffle.
- Reproducible sequences: constructing two generators with the same seed produces identical streams.

## Building and Testing

```
sbt compile
sbt test
```

Tests use [MUnit](https://scalameta.org/munit/) and cover reproducibility, range correctness, error handling, and statistical distribution quality (chi-squared uniformity tests, moment tests for mean and variance).

Pepe Gallardo, 2024
