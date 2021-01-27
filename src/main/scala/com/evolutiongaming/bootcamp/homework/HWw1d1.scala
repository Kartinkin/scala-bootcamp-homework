package com.evolutiongaming.bootcamp.homework

object HWw1d1 {

  def gcd(a: Int, b: Int): Int = {

    def euclid(a: Int, b: Int): Int = if (b == 0) a else euclid(b, a % b)

    val aa = a.abs
    val ab = b.abs
    if (aa > ab) euclid(aa, ab) else euclid(ab, aa)
  }

  def lcm(a: Int, b: Int): Int =
    if (a == 0 || b == 0) 0
    else (a / gcd(a, b) * b).abs

}
