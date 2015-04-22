package com.mahout.affinity.hashing

import java.util.Random
import com.mahout.affinity.DataVector

@SerialVersionUID(-3784656820380622717L)
class EuclideanHash(dimensions: Int, private var w: Int) extends HashFunction {

  private var randomProjection: DataVector = new DataVector(dimensions)

  val rand = new Random()
  private var offset: Int = rand.nextInt(w)

  

  for (d <- 0 until dimensions) {
    val `val` = rand.nextGaussian()
    randomProjection.set(d, `val`)
  }

  def hash(vector: DataVector): Int = {
    val hashValue = (vector.dot(randomProjection) + offset) / java.lang.Double.valueOf(w)
    Math.round(hashValue).toInt
  }
}