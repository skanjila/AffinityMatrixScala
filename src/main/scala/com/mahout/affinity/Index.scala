package com.mahout.affinity


import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectInput
import java.io.ObjectInputStream
import java.io.ObjectOutput
import java.io.ObjectOutputStream
import java.io.OutputStream
import java.io.Serializable
import java.util.ArrayList
import java.util.Collections
import java.util.HashSet
import java.util.List
import java.util.Set
import java.util.logging.Logger
import com.mahout.affinity.hashing.DistanceComparator
import com.mahout.affinity.hashing.DistanceMeasure
import com.mahout.affinity.hashing.HashFamily
import com.mahout.affinity.utils.FileUtils
import scala.collection.JavaConversions._

object Index {

  private val LOG = Logger.getLogger(classOf[Index].getName)

  def serialize(index: Index) {
    try {
      val serializationFile = serializationName(index)

      val file = new FileOutputStream(serializationFile)
      val buffer = new BufferedOutputStream(file)
      val output = new ObjectOutputStream(buffer)
      try {
        output.writeObject(index)
      } finally {
        output.close()
      }
    } catch {
      case ex: IOException => 
    }
  }

  private def serializationName(index: Index): String = {
    val name = index.family.getClass.getName
    val numberOfHashes = index.getNumberOfHashes
    val numberOfHashTables = index.getNumberOfHashTables
    name + "_" + numberOfHashes + "_" + numberOfHashTables + 
      ".bin"
  }

  def deserialize(family: HashFamily, numberOfHashes: Int, numberOfHashTables: Int): Index = {
    var index = new Index(family, numberOfHashes, numberOfHashTables)
    val serializationFile = serializationName(index)
    if (FileUtils.exists(serializationFile)) {
      try {
        val file = new FileInputStream(serializationFile)
        val buffer = new BufferedInputStream(file)
        val input = new ObjectInputStream(buffer)
        try {
          index = input.readObject().asInstanceOf[Index]
        } finally {
          input.close()
        }
      } catch {
        case ex: ClassNotFoundException => LOG.severe("Could not find class during deserialization: " + ex.getMessage)
        case ex: IOException => {
          LOG.severe("IO exeption during during deserialization: " + ex.getMessage)
          ex.printStackTrace()
        }
      }
    }
    index
  }
}

@SerialVersionUID(3757702142917691272L)
class Index(private var family: HashFamily, numberOfHashes: Int, numberOfHashTables: Int)
    extends Serializable {

  private var hashTable: List[HashTable] = new ArrayList[HashTable]()

  private var evaluated: Int = 0

  for (i <- 0 until numberOfHashTables) {
    hashTable.add(new HashTable(numberOfHashes, family))
  }

  def index(vector: DataVector) {
    hashTable.foreach(table=>table.add(vector))
  }

  def getNumberOfHashTables(): Int = hashTable.size

  def getNumberOfHashes(): Int = hashTable.get(0).getNumberOfHashes

  def query(query: DataVector, maxSize: Int): List[DataVector] = {
    val candidateSet = new HashSet[DataVector]()
    for (table <- hashTable) {
      val v = table query query
      candidateSet.addAll(v)
    }
    var candidates = new ArrayList[DataVector](candidateSet)
    evaluated += candidates.size
    val measure = family.createDistanceMeasure()
    val dc = new DistanceComparator(query, measure)
    Collections.sort(candidates, dc)
    if (candidates.size > maxSize) {
      candidates = new ArrayList[DataVector](candidates.subList(0, maxSize))
    }
    candidates
  }

  def getTouched(): Int = evaluated
}
