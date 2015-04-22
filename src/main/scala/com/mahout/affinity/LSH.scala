package com.mahout.affinity

import java.util.ArrayList
import java.util.HashSet
import java.util.List
import java.util.PriorityQueue
import java.util.Random
import java.util.Set
import java.util.concurrent.Callable
import java.util.concurrent.ExecutionException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import com.mahout.affinity.hashing.DistanceComparator
import com.mahout.affinity.hashing.DistanceMeasure
import com.mahout.affinity.hashing.HashFamily
import com.mahout.affinity.utils.FileUtils
import com.mahout.affinity.utils.CommandLineInterface
import collection.JavaConversions._

object LSH {

  def linearSearch(dataset: List[DataVector], 
      query: DataVector, 
      resultSize: Int, 
      measure: DistanceMeasure): List[DataVector] = {
    val dc = new DistanceComparator(query, measure)
    val pq = new PriorityQueue[DataVector](dataset.size, dc)
    pq.addAll(dataset)
    val vectors = new ArrayList[DataVector]()
    for (i <- 0 until resultSize) {
      vectors.add(pq.poll())
    }
    vectors
  }

  def readDataset(file: String, maxSize: Int): List[DataVector] = {
    val ret = new ArrayList[DataVector]()
    var data = FileUtils.readCSVFile(file, " ", -1)
    if (data.size > maxSize) {
      data = data.subList(0, maxSize)
    }
    var firstColumnIsKey = false
    try {
      java.lang.Double.parseDouble(data.get(0)(0))
    } catch {
      case e: Exception => firstColumnIsKey = true
    }
    val dimensions = if (firstColumnIsKey) data.get(0).length - 1 else data.get(0).length
    val startIndex = if (firstColumnIsKey) 1 else 0
    for (row <- data) {
      val item = new DataVector(dimensions)
      if (firstColumnIsKey) {
        item.setKey(row(0))
      }
      for (d <- startIndex until row.length) {
        val value = java.lang.Double.parseDouble(row(d))
        item.set(d - startIndex, value)
      }
      ret.add(item)
    }
    ret
  }

  def determineRadius(dataset: List[DataVector], measure: DistanceMeasure, timeout: Int): Double = {
    val executor = Executors.newSingleThreadExecutor()
    var radius = 0.0
    val drt = new DetermineRadiusTask(dataset, measure)
    val future = executor.submit(drt)
    try {
      println("Determine radius..")
      radius = 0.90 * future.get(timeout, TimeUnit.SECONDS)
      println("Determined radius: " + radius)
    } catch {
      case e: TimeoutException => {
        System.err.println("Terminated!")
        radius = 0.90 * drt.getRadius
      }
      case e: InterruptedException => {
        System.err.println("Execution interrupted!" + e.getMessage)
        radius = 0.90 * drt.getRadius
      }
      case e: ExecutionException => radius = 0.90 * drt.getRadius
    }
    executor.shutdownNow()
    radius
  }

  class DetermineRadiusTask(private val dataset: List[DataVector], private val measure: DistanceMeasure)
      extends Callable[Double] {

    private var queriesDone: Double = 0

    private var radiusSum: Double = 0.0

    private val rand = new Random()

    override def call(): Double = {
      for (i <- 0 until 30) {
        val query = dataset.get(rand.nextInt(dataset.size))
        val result = linearSearch(dataset, query, 2, measure)
        radiusSum += measure.distance(query, result.get(1))
        queriesDone += 1
      }
      radiusSum / queriesDone
    }

    def getRadius(): Double = radiusSum / queriesDone
  }

  def main(args: Array[String]) {
    val cli = new CommandLineInterface(args)
    cli.parseArguments()
    cli.startApplication()
  }
}

class LSH(var dataset: List[DataVector], private val hashFamily: HashFamily) {

  private var index: Index = _

  def buildIndex(numberOfHashes: Int, numberOfHashTables: Int) {
    index = Index.deserialize(hashFamily, numberOfHashes, numberOfHashTables)
    if (dataset != null) {
      for (theVector<-dataset) {
        index.index(theVector)
      }
      Index.serialize(index)
    }
  }

  def benchmark(neighboursSize: Int, measure: DistanceMeasure) {
    var startTime = 0.0
    var linearSearchTime = 0.0
    var lshSearchTime = 0.0
    var numbercorrect = 0.0
    var falsePositives = 0.0
    var truePositives = 0.0
    var falseNegatives = 0.0
    for (i <- 0 until dataset.size) {
      val query = dataset.get(i)
      startTime = System.currentTimeMillis()
      val lshResult = index.query(query, neighboursSize)
      lshSearchTime += System.currentTimeMillis() - startTime
      startTime = System.currentTimeMillis()
      val linearResult = LSH.linearSearch(dataset, query, neighboursSize, measure)
      linearSearchTime += System.currentTimeMillis() - startTime
      val set = new HashSet[DataVector]()
      set.addAll(lshResult)
      set.addAll(linearResult)
      falsePositives += set.size - linearResult.size
      truePositives += lshResult.size + linearResult.size - set.size
      falseNegatives = set.size - lshResult.size
      var correct = true
      for (j <- 0 until Math.min(lshResult.size, linearResult.size)) {
        correct = correct && lshResult.get(j) == linearResult.get(j)
      }
      if (correct) {
        numbercorrect += 1
      }
    }
    val numberOfqueries = dataset.size
    val dataSetSize = dataset.size
    val precision = truePositives / 
      java.lang.Double.valueOf(truePositives + falsePositives) * 
      100
    val recall = truePositives / 
      java.lang.Double.valueOf(truePositives + falseNegatives) * 
      100
    val percentageCorrect = numbercorrect / dataSetSize * 100
    val percentageTouched = index.getTouched / numberOfqueries / dataSetSize * 100
    linearSearchTime /= 1000.0
    lshSearchTime /= 1000.0
    val hashes = index.getNumberOfHashes
    val hashTables = index.getNumberOfHashTables
    System.out.printf("%10d%15d%9.2f%%%9.2f%%%9.4fs%9.4fs%9.2f%%%9.2f%%\n", hashes, hashTables, percentageCorrect, percentageTouched, linearSearchTime, lshSearchTime, precision, recall)
  }

  def query(query: DataVector, neighboursSize: Int): List[DataVector] = index.query(query, neighboursSize)
}
