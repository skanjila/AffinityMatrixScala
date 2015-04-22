package com.mahout.affinity.utils

import com.mahout.affinity.hashing.DistanceMeasure


import java.util.List
import com.mahout.affinity.hashing.DistanceMeasure
import com.mahout.affinity.DataVector
import com.mahout.affinity.hashing.EuclideanDistance
import com.mahout.affinity.hashing.EuclidianHashFamily
import com.mahout.affinity.hashing.HashFamily
import com.mahout.affinity.LSH
import scala.collection.JavaConversions._


class CommandLineInterface(args: Array[String]) {

  private var arguments: Array[String] = args

  private var numberOfHashTables: Int = _

  private var numberOfHashes: Int = _

  private var numberOfNeighbours: Int = _

  private var radius: Double = _

  private var dataset: List[DataVector] = _

  private var queries: List[DataVector] = _

  private var dimensions: Int = _

  private var measure: DistanceMeasure = _

  private var timeout: Int = 40

  private var family: HashFamily = _

  private var benchmark: Boolean = _

  private var printHelp: Boolean = _

  def parseArguments() {
    numberOfHashTables = getIntegerValue("-t", 4)
    numberOfHashes = getIntegerValue("-h", 4)
    numberOfNeighbours = getIntegerValue("-n", 4)
    val hashFamilyType = getValue("-f", "l2")
    radius = getDoubleValue("-r", 0.0)
    printHelp = hasOption("--help") || arguments.length == 0
    benchmark = hasOption("-b")
    val datasetFile = getValue("-d", null)
    val queryFile = getValue("-q", null)
    if (benchmark) {
      dimensions = 256
      if (radius == 0) {
        radius = 10
      }
      dataset = TestUtils.generate(dimensions, 100, 512)
      TestUtils.addNeighbours(dataset, numberOfNeighbours, radius)
    }
    if (datasetFile != null) {
      dataset = LSH.readDataset(datasetFile, java.lang.Integer.MAX_VALUE)
      dimensions = dataset.get(0).getDimensions
    }
    if (queryFile != null) {
      queries = LSH.readDataset(queryFile, java.lang.Integer.MAX_VALUE)
      dimensions = queries.get(0).getDimensions
    }
    /*if (radius == 0 && hashFamilyType.equalsIgnoreCase("l1")) {
      measure = new CityBlockDistance()
      radius = LSH.determineRadius(dataset, measure, timeout)
    } else if (radius == 0 && hashFamilyType.equalsIgnoreCase("l2")) {
      measure = new CityBlockDistance()
      radius = LSH.determineRadius(dataset, measure, timeout)
    }*/
    family = getHashFamily(radius, hashFamilyType, dimensions)
  }

  def startApplication() {
    if (printHelp) {
      printTheHelp()
    } else if (benchmark) {
      printLine()
      startBenchmark()
    } else {
      printLine()
      startLSH()
    }
  }

  def startBenchmark() {
    println("Starting LSH benchmark with " + dataset.size + 
      " random vectors")
    println("   Four close neighbours have been added to 100 vectors (100+4x100=500).")
    println("   The results of LSH are compared with a linear search.")
    println("   The first four results of LSH and linear are compared.")
    println("Radius for Euclidean distance.")
    val radiusEuclidean = LSH.determineRadius(dataset, new EuclideanDistance(), 20).toInt
    //println("\nRadius for City block distance.")
    //val radiusCityBlock = LSH.determineRadius(dataset, new CityBlockDistance(), 20).toInt
    //val families = Array(new EuclidianHashFamily(radiusEuclidean, dimensions), new CityBlockHashFamily(radiusCityBlock, 
    //  dimensions), new CosineHashFamily(dimensions))
    val families = Array(new EuclidianHashFamily(radiusEuclidean, dimensions))
    for (family <- families) {
      val numberOfHashes = Array(2, 4, 8)
      /*if (family.isInstanceOf[CosineHashFamily]) {
        numberOfHashes(0) *= 8
        numberOfHashes(1) *= 8
        numberOfHashes(2) *= 8
      }*/
      val numberOfHashTables = Array(2, 4, 8, 16)
      val lsh = new LSH(dataset, family)
      println("\n--" + family.getClass.getName)
      System.out.printf("%10s%15s%10s%10s%10s%10s%10s%10s\n", "#hashes", "#hashTables", "Correct", "Touched", 
        "linear", "LSH", "Precision", "Recall")
      for (i <- 0 until numberOfHashes.length; j <- 0 until numberOfHashTables.length) {
        lsh.buildIndex(numberOfHashes(i), numberOfHashTables(j))
        lsh.benchmark(numberOfNeighbours, family.createDistanceMeasure())
      }
    }
  }

  private def startLSH() {
    val lsh = new LSH(dataset, family)
    lsh.buildIndex(numberOfHashes, numberOfHashTables)
    for (query <- queries) {
        lsh.query(query, numberOfNeighbours)
    }
  }

  private def getHashFamily(radius: Double, hashFamilyType: String, dimensions: Int): HashFamily = {
    var family: HashFamily = null
    /*if (hashFamilyType.equalsIgnoreCase("cos")) {
      family = new CosineHashFamily(dimensions)
    } else if (hashFamilyType.equalsIgnoreCase("l1")) {
      var w = (10 * radius).toInt
      w = if (w == 0) 1 else w
      family = new CityBlockHashFamily(w, dimensions)
    } else*/ 
    if (hashFamilyType.equalsIgnoreCase("l2")) {
      var w = (10 * radius).toInt
      w = if (w == 0) 1 else w
      family = new EuclidianHashFamily(w, dimensions)
    } else {
      new IllegalArgumentException(hashFamilyType + " is unknown, should be one of cos|l1|l2")
    }
    family
  }

  private def hasOption(option: String): Boolean = {
    var index = -1
    for (i <- 0 until arguments.length if arguments(i).equalsIgnoreCase(option)) {
      index = i
    }
    index >= 0
  }

  private def getIntegerValue(option: String, defaultValue: java.lang.Integer): java.lang.Integer = {
    val value = getValue(option, defaultValue.toString)
    var integerValue: java.lang.Integer = null
    try {
      integerValue = java.lang.Integer.parseInt(value)
    } catch {
      case e: NumberFormatException => {
        var message: String = null
        message = "Expected integer argument for option " + option + ",  " + 
          value + 
          " is" + 
          " not an integer."
        printError(message)
      }
    }
    integerValue
  }

  private def getDoubleValue(option: String, defaultValue: java.lang.Double): java.lang.Double = {
    val value = getValue(option, defaultValue.toString)
    var doubleValue: java.lang.Double = null
    try {
      doubleValue = java.lang.Double.parseDouble(value)
    } catch {
      case e: NumberFormatException => {
        var message: String = null
        message = "Expected integer argument for option " + option + ",  " + 
          value + 
          " is" + 
          " not an integer."
        printError(message)
      }
    }
    doubleValue
  }

  private def getValue(option: String, defaultValue: String): String = {
    var index: Integer = -1
    var value: String = null
    for (i <- 0 until arguments.length if arguments(i).equalsIgnoreCase(option)) {
      index = i
    }
    value = if (index >= 0) arguments(index + 1) else defaultValue
    value
  }


  private def printLine() {
    System.err.println("----------------------------------------------------")
  }

  private def printTheHelp() {
    printLine()
    println("Name")
    println(" LSH: finds the nearest neighbours in a data set quickly, using LSH.")
    println("Synopsis     ")
    println(" java - jar TarsosLSH.jar [options]")
    println("Description")
    println(" Tries to find nearest neighbours for each vector in the")
    println(" query file, using Euclidean (L<sub>2</sub>) distance by default.")
    println(" ")
    println(" Both dataset.txt and queries.txt have a similar format:")
    println(" an optional identifier for the vector and a list of N")
    println(" coordinates (which should be doubles).")
    println(" ")
    println(" [Identifier] coord1 coord2 ... coordN")
    println(" [Identifier] coord1 coord2 ... coordN")
    println(" ")
    println(" For an example data set with two elements and 4 dimensions:")
    println("   ")
    println(" Hans 12 24 18.5 -45.6")
    println(" Jane 13 19 -12.0 49.8")
    println(" ")
    println(" Options are:")
    println("   ")
    println(" -d dataset.txt  ")
    println("   The dataset with vectors to store in the hash table")
    println(" -q queries.txt  ")
    println("   A list of vectors to query against the stored dataset")
    println(" -f cos|l1|l2")
    println("   Defines the hash family to use:")
    println("     l1  City block hash family (L<sub>1</sub>)")
    println("     l2  Euclidean hash family(L<sub>2</sub>)")
    println("     cos Cosine distance hash family")
    println(" -r radius")
    println("   Defines the radius in which near neighbours should")
    println("   be found. Should be a double. By default a reasonable")
    println("   radius is determined automatically.")
    println(" -h n_hashes")
    println("   An integer that determines the number of hashes to")
    println("   use. By default 4, 32 for the cosine hash family.")
    println(" -t n_tables")
    println("   An integer that determines the number of hash tables,")
    println("   each with n_hashes, to use. By default 4.")
    println(" -n n_neighbours")
    println("   Number of neighbours in the neighbourhood, defaults to 3.")
    println(" -b")
    println("   Benchmark the settings.")
    println(" --help")
    println("   Prints this helpful message.")
    println(" ")
    println("Examples")
    println(" Search for nearest neighbours using the l2 hash family with a radius of 500")
    println(" and utilizing 5 hash tables, each with 3 hashes.")
    println(" ")
    println(" java -jar TarsosLSH.jar -f l2 -r 500 -h 3 -t 5 -d dataset.txt -q queries.txt")
  }

  private def printError(message: String) {
    printTheHelp()
    printLine()
    println("GURU MEDITATION:")
    printLine()
    println(message)
  }
}
