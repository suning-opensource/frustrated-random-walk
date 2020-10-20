//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved.

package frustrated.random.walk

import scala.collection.mutable.HashMap

class SparseMatrix[T]
{
    var map: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]

    def this(map: HashMap[(Int, Int), T]) = 
    {
        this()
        val keys: Array[(Int, Int)] = map.keys.to[Array]
        for ((row, col) <- keys)
        {
            this.map((row, col)) = map((row, col))
        }
    }

    def this(that: SparseMatrix[T]) = 
    {
        this()
        val keys: Array[(Int, Int)] = that.map.keys.to[Array]
        for ((row, col) <- keys)
        {
            this.map((row, col)) = that.map((row, col))
        }
    }

    def assignment(that: SparseMatrix[T]): Unit = 
    {
        this.map.clear
        val keys: Array[(Int, Int)] = that.map.keys.to[Array]
        for ((row, col) <- keys)
        {
            this.map((row, col)) = that.map((row, col))
        }
    }

    def getDimension: Int = Utilities.getDimension(this.map)

    def printMatrix(matrixFormat: Boolean = false): Unit = 
    {
        if (matrixFormat)
        {
            val dimension: Int = this.getDimension
            Utilities.printSparseMatrix(this.map, dimension)
        }
        else
        {
            Utilities.printSparseMatrix(map)
        }
    }

    def saveToFile(outputFileName: String): Unit = Utilities.saveSparseMatrix(this.map, outputFileName)
    def save(outputFileName: String): Unit = Utilities.saveSparseMatrix(this.map, outputFileName)

    def printToMathematicaFormat: Unit = println(Utilities.sparseMatrixToMathematicaFormat(this.map))

    def printToLatexFormat: Unit = println(Utilities.sparseMatrixToLatex(this.map))

    def transpose: SparseMatrix[T] = 
    {
        val newMap: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
        val keys: Array[(Int, Int)] = map.keys.to[Array]
        for ((row, col) <- keys)
        {
            newMap((col, row)) = map((row, col))
        }
        return new SparseMatrix[T](newMap)
    }

    def *(that: SparseMatrix[T]): SparseMatrix[T] = 
    {
        val dimension1: Int = this.getDimension
        val dimension2: Int = that.getDimension
        assert(dimension1 == dimension2, "Matrix dimensions incompatible. ")
        val mapResult: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
        val rowVectors: HashMap[Int, HashMap[Int, T]] = Utilities.splitSparseMatrixByRow(this.map)
        val colVectors: HashMap[Int, HashMap[Int, T]] = Utilities.splitSparseMatrixByCol(that.map)
        val dimension: Int = dimension1
        for (row <- 0 until dimension)
        {
            for (col <- 0 until dimension)
            {
                if (rowVectors.contains(row) && colVectors.contains(col))
                {
                    val product: T = Utilities.innerProduct(rowVectors(row), colVectors(col))
                    if (!Utilities.isZero(product))
                    {
                        mapResult((row, col)) = product
                    }
                }
            }
        }
        return new SparseMatrix[T](mapResult)
    }

    def *(vector: HashMap[Int, T]): HashMap[Int, T] =
    {
        val result: HashMap[Int, T] = new HashMap[Int, T]
        if (vector.size == 0)
        {
            return result
        }
        val rowVectors: HashMap[Int, HashMap[Int, T]] = Utilities.splitSparseMatrixByRow(this.map)
        val dimension: Int = this.getDimension
        for (row <- 0 until dimension)
        {
            if (rowVectors.contains(row))
            {
                val product: T = Utilities.innerProduct(rowVectors(row), vector)
                if (!Utilities.isZero(product))
                {
                    result(row) = product
                }
            }
        }
        return result
    }

    def *(vector: SparseVector[T]): SparseVector[T] = 
    {
        return new SparseVector[T](this*(vector.map))
    }

    def *(factor: Double): SparseMatrix[Double] = 
    {
        val newMap: HashMap[(Int, Int), Double] = new HashMap[(Int, Int), Double]
        for ((row, col) <- map.keys)
        {
            newMap((row, col)) = map((row, col)).asInstanceOf[Double]*factor
        }
        return new SparseMatrix[Double](newMap)
    }

    def +(that: SparseMatrix[T]): SparseMatrix[T] = 
    {
        return new SparseMatrix(Utilities.addSparseMatrix(this.map, that.map))
    }

    def -(that: SparseMatrix[T]): SparseMatrix[T] = 
    {
        return new SparseMatrix(Utilities.subtractSparseMatrix(this.map, that.map))
    }

    def matrixPower(power: Int): SparseMatrix[T] = 
    {
        assert(power >= 0, "Only non-negative power is accepted. ")
        val dimension: Int = this.getDimension
        if (power == 0)
        {
            val identity: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
            for (i <- 0 until dimension)
            {
                identity((i, i)) = 1.asInstanceOf[T]
            }
            return new SparseMatrix[T](identity)
        }
        else if (power == 1)
        {
            return this
        }
        else
        {
            var result: SparseMatrix[T] = this
            for (p <- 1 to power)
            {
                result = result*this
            }
            return result
        }
    }
}
