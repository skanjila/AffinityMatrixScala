package com.mahout.affinity.utils


import java.util.ArrayList
import java.util.List
import java.util.Random
import com.mahout.affinity.DataVector

object TestUtils {

  def generate(dimensions: Int, datasetSize: Int, maxValue: Int): List[DataVector] = {
    val rand = new Random()
    val ret = new ArrayList[DataVector]()
    for (i <- 0 until datasetSize) {
      val item = new DataVector(dimensions)
      for (d <- 0 until dimensions) {
        val point = rand.nextInt(maxValue)
        item.set(d, point)
      }
      ret.add(item)
    }
    ret
  }

  def addNeighbours(dataset: List[DataVector], numberOfNeighboursToAdd: Int, radius: Double) {
    val datasetSize = dataset.size
    for (i <- 0 until datasetSize) {
      val original = dataset.get(i)
      for (neighbours <- 0 until numberOfNeighboursToAdd) {
        val neighbour = new DataVector(original)
        neighbour.moveSlightly(radius)
        dataset.add(neighbour)
      }
    }
  }
}
