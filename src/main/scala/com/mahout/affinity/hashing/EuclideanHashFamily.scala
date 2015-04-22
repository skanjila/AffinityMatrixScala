package com.mahout.affinity.hashing

import java.util.Arrays

@SerialVersionUID(3406464542795652263L)
class EuclidianHashFamily(private var w: Int, private val dimensions: Int) extends HashFamily {

  override def createHashFunction(): HashFunction = new EuclideanHash(dimensions, w)

  override def combine(hashes: Array[Int]): java.lang.Integer = Arrays.hashCode(hashes)

  override def createDistanceMeasure(): DistanceMeasure = new EuclideanDistance()
}
