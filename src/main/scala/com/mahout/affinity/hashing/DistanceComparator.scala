package com.mahout.affinity.hashing

import java.util.Comparator
import com.mahout.affinity.DataVector

class DistanceComparator(private val query: DataVector, private val distanceMeasure: DistanceMeasure)
    extends Comparator[DataVector] {

  override def compare(one: DataVector, other: DataVector): Int = {
    val oneDistance = distanceMeasure.distance(query, one)
    val otherDistance = distanceMeasure.distance(query, other)
    oneDistance.compareTo(otherDistance)
  }
}