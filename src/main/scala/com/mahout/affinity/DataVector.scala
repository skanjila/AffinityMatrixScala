package com.mahout.affinity


import java.io.Serializable
import java.util.Arrays
import java.util.Random

//remove if not needed
import collection.JavaConversions._
@SerialVersionUID(5169504339456492327L)
class DataVector(var key: String, var values: Array[Double])
    extends Serializable {

  def this(dimensions: Int) {
    this(null, Array.ofDim[Double](dimensions))
  }

  def getKey:String= {
    return key
  }
  
  def setKey(keyToSet:String)= {
    if (keyToSet!=" ") {
      key=keyToSet
    }
  }
  
  
  def this(other: DataVector) {
    this(other.getKey, Arrays.copyOf(other.values, other.values.length))
  }

  def moveSlightly(radius: Double) {
    val rand = new Random()
    for (d <- 0 until getDimensions) {
      val diff = radius + (-radius - radius) * rand.nextDouble()
      val point = get(d) + diff
      set(d, point)
    }
  }

  def set(dimension: Int, value: Double) {
    values(dimension) = value
  }

  def get(dimension: Int): Double = values(dimension)

  def getDimensions(): Int = values.length

  def dot(other: DataVector): Double = {
    var sum = 0.0
    for (i <- 0 until getDimensions) {
      sum += values(i) * other.values(i)
    }
    sum
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("values:[")
    for (d <- 0 until getDimensions - 1) {
      sb.append(values(d)).append(",")
    }
    sb.append(values(getDimensions - 1)).append("]")
    sb.toString
  }
}