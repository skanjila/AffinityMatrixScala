package com.mahout.affinity.hashing

import java.io.Serializable
import com.mahout.affinity.DataVector

trait HashFunction extends Serializable {

  def hash(vector: DataVector): Int
}