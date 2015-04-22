package com.mahout.affinity

import java.io.Serializable
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import com.mahout.affinity.hashing.HashFamily
import com.mahout.affinity.hashing.HashFunction

@SerialVersionUID(-5410017645908038641L)
class HashTable(numberOfHashes: Int, private var family: HashFamily) extends Serializable {

  private var hashTable: HashMap[Integer, List[DataVector]] = new HashMap[Integer, List[DataVector]]()

  private var hashFunctions: Array[HashFunction] = new Array[HashFunction](numberOfHashes)

  for (i <- 0 until numberOfHashes) {
    hashFunctions(i) = family.createHashFunction()
  }

  def query(query: DataVector): List[DataVector] = {
    val combinedHash = hash(query)
    if (hashTable.containsKey(combinedHash)) hashTable.get(combinedHash) else new ArrayList[DataVector]()
  }

  def add(vector: DataVector) {
    val combinedHash = hash(vector)
    if (!hashTable.containsKey(combinedHash)) {
      hashTable.put(combinedHash, new ArrayList[DataVector]())
    }
    hashTable.get(combinedHash).add(vector)
  }

  private def hash(vector: DataVector): java.lang.Integer = {
    val hashes = Array.ofDim[Int](hashFunctions.length)
    for (i <- 0 until hashFunctions.length) {
      hashes(i) = hashFunctions(i).hash(vector)
    }
    val combinedHash = family.combine(hashes)
    combinedHash
  }

  def getNumberOfHashes(): Int = hashFunctions.length
}
