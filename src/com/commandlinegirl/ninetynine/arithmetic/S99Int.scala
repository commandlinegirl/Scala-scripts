package com.commandlinegirl.ninetynine.arithmetic

/**
 * Created on 05/10/15.
 */

class S99Int(val start: Int) {
  import S99Int._

  def - (other: S99Int): S99Int =
    this - other

  def toInt: Int =
    start

  /* 31. Determine whether a given integer number is prime.*/
  def isPrime(start: Int): Boolean = {
    val r = 2 to math.sqrt(start).toInt
    println(r)
    !(r exists (start % _ == 0))
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}
