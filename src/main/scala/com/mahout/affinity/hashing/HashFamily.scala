package com.mahout.affinity.hashing

import java.io.Serializable

trait HashFamily extends Serializable {

  def createHashFunction(): HashFunction

  def combine(hashes: Array[Int]): java.lang.Integer

  def createDistanceMeasure(): DistanceMeasure
}