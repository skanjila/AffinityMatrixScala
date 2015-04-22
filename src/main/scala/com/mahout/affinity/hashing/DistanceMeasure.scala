package com.mahout.affinity.hashing

import com.mahout.affinity.DataVector

trait DistanceMeasure {

  def distance(one: DataVector, other: DataVector): Double
}
