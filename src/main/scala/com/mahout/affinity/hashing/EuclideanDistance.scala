package com.mahout.affinity.hashing

import com.mahout.affinity.DataVector


class EuclideanDistance extends DistanceMeasure {

  override def distance(one: DataVector, other: DataVector): Double = {
    var sum = 0.0
    for (d <- 0 until one.getDimensions) {
      val delta = one.get(d) - other.get(d)
      sum += delta * delta
    }
    Math.sqrt(sum)
  }
}
