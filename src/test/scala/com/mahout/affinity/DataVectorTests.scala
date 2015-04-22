package com.mahout.affinity

import org.junit.Assert
import org.junit.Test

class DataVectorTests {

  @Test
  def testDotProduct() {
    val one = new DataVector(3)
    val two = new DataVector(3)
    one.set(0, 1)
    two.set(0, 7)
    one.set(1, 1)
    two.set(1, -7)
    one.set(2, 0)
    two.set(2, 1)
    Assert.assertEquals(0, one.dot(two), 0.000000000001)
    Assert.assertEquals(two.dot(one), one.dot(two), 0.000000000001)
    one.set(2, 3)
    Assert.assertEquals(3, one.dot(two), 0.000000000001)
  }
}