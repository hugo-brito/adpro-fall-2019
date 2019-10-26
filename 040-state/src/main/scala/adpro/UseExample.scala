package adpro

import adpro.RNG.SimpleRNG
import State._

object Example {

val r0 = random_int // generator of random integers
val (i1,r1) = random_int.run (SimpleRNG(42))
val (i2,r2) = random_int.run (r1)
val (i3,r3) = random_int.run (r2)

}
