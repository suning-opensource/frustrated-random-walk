//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved.

package frustrated.random.walk

import scala.collection.mutable.HashMap

class SparseVector[T]
{
    var map: HashMap[Int, T] = new HashMap[Int, T]
    def this(map: HashMap[Int, T]) = 
    {
        this()
        val keys: Array[Int] = map.keys.to[Array]
        for (key <- keys)
        {
            this.map(key) = map(key)
        }
    }
    def this(that: SparseVector[T]) =
    {
        this()
        val keys: Array[Int] = that.map.keys.to[Array]
        for (index <- keys)
        {
            this.map += (index -> that.map(index))
        }
    }
    def assignment(that: SparseVector[T]): Unit = 
    {
        this.map.clear
        val keys: Array[Int] = that.map.keys.to[Array]
        for (key <- keys)
        {
            this.map(key) = that.map(key)
        }
    }
    def printSparseVector: Unit = Utilities.printSparseVector(this.map)
    override def toString: String = Utilities.sparseVectorToString(this.map)

    def saveToFile(outputFileName: String): Unit = Utilities.saveSparseVectorToFile(this.map, outputFileName)
    def save(outputFileName: String): Unit = Utilities.saveSparseVectorToFile(this.map, outputFileName)

    def sum: T = Utilities.sumSparseVector(this.map)
    def add(that: SparseVector[T]): SparseVector[T] = new SparseVector[T](Utilities.addSparseVector(this.map, that.map))
    def subtract(that: SparseVector[T]): SparseVector[T] = new SparseVector[T](Utilities.subtractSparseVector(this.map, that.map))
    def innerProduct(that: SparseVector[T]): T = Utilities.innerProduct(this.map, that.map)
    def +(that: SparseVector[T]): SparseVector[T] = this.add(that)
    def -(that: SparseVector[T]): SparseVector[T] = this.subtract(that)
    def *(that: SparseVector[T]): T = this.innerProduct(that)
    def *(factor: T): SparseVector[T] =
    {
        val tempMap: HashMap[Int, T] = new HashMap[Int, T]
        val keys: Array[Int] = this.map.keys.to[Array]
        for (index <- keys)
        {
            tempMap(index) = Utilities.multiply(factor, map(index))
        }
        return new SparseVector[T](tempMap)
    }

    def L1Norm: T = Utilities.sparseVectorL1Norm(this.map)
    def L2Norm: Double = Utilities.sparseVectorL2Norm(this.map)
    def normalizeByL1: SparseVector[Double] =
    {
        return new SparseVector[Double](Utilities.normalizeSparseVectorByL1(this.map))
    }
    def normalizeByL2: SparseVector[Double] =
    {
        return new SparseVector[Double](Utilities.normalizeSparseVectorByL2(this.map))
    }
}
