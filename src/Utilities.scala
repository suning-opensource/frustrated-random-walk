//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved.

package frustrated.random.walk

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.nio.file.{Files, Paths}
import java.io.{PrintWriter, File}
import scala.math._
import scala.collection.mutable.HashMap
import scala.util.Random
import util.control.Breaks._

object Utilities
{
    def getDimension[T](map: HashMap[(Int, Int), T]): Int =
    {
        var rowMax: Int = -1
        var colMax: Int = -1
        val keys = map.keys.to[Array]
        for ((row, col) <- keys)
        {
            if (rowMax < row)
            {
                rowMax = row
            }
            if (colMax < col)
            {
                colMax = col
            }
        }
        return max(rowMax+1, colMax+1)
    }

    def lessThan(p: (Int, Int), q: (Int, Int)): Boolean = 
    {
        if (p._1 < q._1)
        {
            return true
        }
        else if (p._1 == q._1)
        {
            return p._2 < q._2
        }
        return false
    }

    def printSparseMatrix[T](sparse: HashMap[(Int, Int), T]): Unit =
    {
        val keys: Array[(Int, Int)] = sparse.keys.to[Array].sortWith((p, q) => lessThan(p, q))
        for ((row, col) <- keys)
        {
            println(row + ", " + col + ", " + sparse((row, col)))
        }
    }

    def roundOff[T](number: T, decimalPoint: Int): T = 
    {
        number match 
        {
            case a: Int => return number.asInstanceOf[T]
            case a: Double => 
            {
                assert(decimalPoint >= 1)
                return BigDecimal(number.asInstanceOf[Double]).setScale(decimalPoint, BigDecimal.RoundingMode.HALF_UP).toDouble.asInstanceOf[T]
            }
            case _ => throw new IllegalArgumentException("Illegal argument type in Utilities.roundOff. ")
        }
        
    }

    def printSparseMatrix[T](sparse: HashMap[(Int, Int), T], dimension: Int): Unit = 
    {
        for (i <- 0 until dimension)
        {
            for (j <- 0 until dimension)
            {
                //print("%25s".format(sparse.getOrElse((i, j), 0)))
                print("%10s".format(roundOff(sparse.getOrElse((i, j), 0), 4).toString))
            }
            print("\n")
        }
    }

    def numberToString[T](a: T): String = 
    {
        a match
        {
            case temp: Double => return a.toString
            case temp: Int => return a.toString
            case _ => throw new IllegalArgumentException("Wrong argument in Utilities.numberToString. ")
        }
    }

    def saveSparseMatrix[T](sparse: HashMap[(Int, Int), T], outputFileName: String): Unit = 
    {
        val writer = new PrintWriter(new File(outputFileName))
        val keys: Array[(Int, Int)] = sparse.keys.to[Array].sortWith((p, q) => lessThan(p, q))
        for ((row, col) <- keys)
        {
            writer.write(row.toString + "," + col.toString + "," + numberToString(sparse((row, col))) + "\n")
        }
        writer.close()
    }

    def sparseMatrixToLatex[T](sparse: HashMap[(Int, Int), T]): String = 
    {
        val dimension: Int = this.getDimension(sparse)
        var result: String = "\\begin{pmatrix}\n"
        for (i <- 0 until dimension)
        {
            for (j <- 0 until dimension)
            {
                if (j < dimension - 1)
                {
                    result += roundOff(sparse.getOrElse((i, j), 0), 4).toString + " & "
                }
                else
                {
                    result += roundOff(sparse.getOrElse((i, j), 0), 4).toString + " \\\\ "
                }
            }
            result += "\n"
        }
        result += "\\end{pmatrix}"
        return result
    }

    def sparseMatrixToMathematicaFormat[T](sparse: HashMap[(Int, Int), T]): String = 
    {
        val dimension: Int = this.getDimension(sparse)
        var result: String = "{"
        for (i <- 0 until dimension)
        {
            for (j <- 0 until dimension)
            {
                if (j == 0)
                {
                    result += "{" + roundOff(sparse.getOrElse((i, j), 0), 4).toString + ", "
                }
                else if (j < dimension - 1)
                {
                    result += roundOff(sparse.getOrElse((i, j), 0), 4).toString + ", "
                }
                else
                {
                    result += roundOff(sparse.getOrElse((i, j), 0), 4).toString + "}"
                }
            }
            if (i < dimension - 1)
            {
                result += ", "
            }
            else
            {
                result += "}"
            }
        }
        return result
    }

    def splitSparseMatrixByRow[T](sparseMatrix: HashMap[(Int, Int), T]): HashMap[Int, HashMap[Int, T]] =
    {
        val result: HashMap[Int, HashMap[Int, T]] = new HashMap[Int, HashMap[Int, T]]
        val keys: Array[(Int, Int)] = sparseMatrix.keys.to[Array].sortWith(_._1 < _._1)
        for ((row, col) <- keys)
        {
            if (result.contains(row))
            {
                result(row) += (col -> sparseMatrix((row, col)))
            }
            else
            {
                result(row) = HashMap[Int, T](col -> sparseMatrix((row, col)))
            }
        }
        return result
    }

    def splitSparseMatrixByCol[T](sparseMatrix: HashMap[(Int, Int), T]): HashMap[Int, HashMap[Int, T]] =
    {
        val result: HashMap[Int, HashMap[Int, T]] = new HashMap[Int, HashMap[Int, T]]
        val keys: Array[(Int, Int)] = sparseMatrix.keys.to[Array].sortWith(_._2 < _._2)
        for ((row, col) <- keys)
        {
            if (result.contains(col))
            {
                result(col) += (row -> sparseMatrix((row, col)))
            }
            else
            {
                result(col) = HashMap[Int, T](row -> sparseMatrix((row, col)))
            }
        }
        return result
    }

    def sparseVectorToString[T](sparseVector: HashMap[Int, T]): String = 
    {
        var res: String = ""
        val keys: Array[Int] = sparseVector.keys.to[Array].sorted
        for (i <- keys)
        {
            res += i.toString + ":" + sparseVector(i).toString + "; "
        }
        return res.slice(0, res.size - 2)
    }
    
    def absoluteValue[T](a: T): T = 
    {
        a match 
        {
            case a: Int => return abs(a).asInstanceOf[T]
            case a: Double => return abs(a).asInstanceOf[T]
            case _ => throw new IllegalArgumentException("Wrong argument in Utilities.absoluteValue. ")
        }
    }

    def squareRoot[T](a: T): Double = 
    {
        a match 
        {
            case a: Int => return sqrt(a).asInstanceOf[Double]
            case a: Double => return sqrt(a).asInstanceOf[Double]
            case _ => throw new IllegalArgumentException("Wrong argument in Utilities.squareRoot. ")
        }
    }
    
    def add[T](a: T, b: T): T =
    {
        (a, b) match
        {
            case (a: Int, b: Int) => return (a + b).asInstanceOf[T]
            case (a: Double, b: Double) => return (a + b).asInstanceOf[T]
            case (a: Int, b: Double) => return (a + b).asInstanceOf[T]
            case (a: Double, b: Int) => return (a + b).asInstanceOf[T]
            case _ => throw new IllegalArgumentException("Wrong argument for Utilities.add. ")
        }
    }

    def negate[T](a: T): T = 
    {
        a match
        {
            case a: Int => return (-a).asInstanceOf[T]
            case a: Double => return (-a).asInstanceOf[T]
            case _ => throw new IllegalArgumentException("Wrong argument type for Utilities.negate. ")
        }
    }

    def subtract[T](a: T, b: T): T = 
    {
        val c: T = negate(b)
        return add(a, c)
    }

    def multiply[T](a: T, b: T): T =
    {
        (a, b) match
        {
            case (a: Int, b: Int) => return (a * b).asInstanceOf[T]
            case (a: Double, b: Double) => return (a * b).asInstanceOf[T]
            case _ => throw new IllegalArgumentException("Wrong argument for Utilities.multiply. ")
        }
    }
    
    def division[T](a: T, b: T): Double =
    {
        (a, b) match
        {
            case (a: Int, b: Int) => {
                if (b == 0)
                {
                    throw new IllegalArgumentException("Wrong argument for Utilities.division ")
                }
                return (a.toDouble/b.toDouble)
            }
            case (a: Double, b: Double) => {
                if (abs(b) < 1.0e-15)
                {
                    throw new IllegalArgumentException("Wrong argument for Utilities.division. ")
                }
                return a/b
            }
            case (a: Int, b: Double) => {
                if (abs(b) < 1.0e-15)
                {
                    throw new IllegalArgumentException("Wrong argument for Utilities.division. ")
                }
                return a/b
            }
            case (a: Double, b: Int) => {
                if (b == 0)
                {
                    throw new IllegalArgumentException("Wrong argument for Utilities.division. ")
                }
                return a/b

            }
            case _ => throw new IllegalArgumentException("Wrong argument for Utilities.division. ")
        }
    }

    def innerProduct[T](a: HashMap[Int, T], b: HashMap[Int, T]): T = 
    {
        if (a.size == 0 || b.size == 0)
        {
            return 0.asInstanceOf[T]
        }
        val akeys: Set[Int] = a.keys.to[Set]
        val bkeys: Set[Int] = b.keys.to[Set]
        val intersection: Set[Int] = akeys.intersect(bkeys)
        if (intersection.size == 0)
        {
            return 0.asInstanceOf[T]
        }
        var result: T = 0.asInstanceOf[T]
        for (index <- intersection)
        {
            result = add(result, multiply(a(index), b(index)))
        }
        return result
    }


    def innerProduct(a: HashMap[Int, Double], b: HashMap[Int, Double]): Double = 
    {
        if (a.size == 0 || b.size == 0)
        {
            return 0.0
        }
        val akeys: Set[Int] = a.keys.to[Set]
        val bkeys: Set[Int] = b.keys.to[Set]
        val intersection: Set[Int] = akeys.intersect(bkeys)
        if (intersection.size == 0)
        {
            return 0.0
        }
        var result: Double = 0.0
        for (index <- intersection)
        {
            result = add(result, multiply(a(index), b(index)))
        }
        return result
    }


    def isZero[T](a: T): Boolean = 
    {
        a match 
        {
            case a: Int => return a == 0
            case a: Double => return abs(a) < 1.0e-8
            case _ => throw new IllegalArgumentException("Wrong argument for Utilities.isZero. ")
        }
    }

    def sparseVectorL1Norm[T](vector: HashMap[Int, T]): T =
    {
        var result: T = 0.asInstanceOf[T]
        val keys: Array[Int] = vector.keys.to[Array]
        for (key <- keys)
        {
            result = add(result, absoluteValue(vector(key)))
        }
        return result
    }
    
    def sumSparseVector[T](vector: HashMap[Int, T]): T = 
    {
        var result: T = 0.asInstanceOf[T]
        val keys: Array[Int] = vector.keys.to[Array]
        for (index <- keys)
        {
            result = add(result, vector(index))
        }
        return result
    }
    
    def sparseVectorL2Norm[T](vector: HashMap[Int, T]): Double =
    {
        val keys: Array[Int] = vector.keys.to[Array]
        if (keys.size == 0)
        {
            return 0.0
        }
        vector(keys(0)) match
        {
            case a: Int => return squareRoot(innerProduct(vector, vector))
            case a: Double => return squareRoot(innerProduct(vector, vector))
            case _ => throw new IllegalArgumentException("Wrong argument in Utilities.sparseVectorL2Norm. ")
        }
    }

    def normalizeSparseVectorByL1[T](vector: HashMap[Int, T]): HashMap[Int, Double] =
    {
        val result: HashMap[Int, Double] = new HashMap[Int, Double]
        if (vector.size == 0)
        {
            return result
        }
        val L1Norm: T = sparseVectorL1Norm(vector)
        val keys = vector.keys
        for (key <- keys)
        {
            result(key) = division(vector(key), L1Norm)
        }
        return result
    }
    
    def normalizeSparseVectorByL2[T](vector: HashMap[Int, T]): HashMap[Int, Double] =
    {
        val result: HashMap[Int, Double] = new HashMap[Int, Double]
        if (vector.size == 0)
        {
            return result
        }
        val L2Norm: Double = sparseVectorL2Norm(vector)
        val keys = vector.keys
        for (key <- keys)
        {
            result(key) = division(vector(key), L2Norm)
        }
        return result
    }

    def printSparseVector[T](vector: HashMap[Int, T]): Unit = 
    {
        val keys: Array[Int] = vector.keys.to[Array].sorted
        for (i <- keys.indices)
        {
            if (i < keys.size - 1)
            {
                print(vector(keys(i)) + ", ")
            }
            else
            {
                print(vector(keys(i)) + "\n")
            }
        }
    }

    def saveSparseVectorToFile[T](vector: HashMap[Int, T], outputFileName: String): Unit = 
    {
        val keys: Array[Int] = vector.keys.to[Array].sorted
        val writer = new PrintWriter(new File(outputFileName))
        for (key <- keys)
        {
            writer.write(key.toString + ":" + vector(key).asInstanceOf[Double].toString + "\n")
        }
        writer.close()
    }

    def generateRandomSparseVector(dimension: Int): HashMap[Int, Double] =
    {
        val result: HashMap[Int, Double] = new HashMap[Int, Double]
        for (i <- 0 until dimension)
        {
            result(i) = Random.nextDouble
        }
        return result
    }

    def generateAllOneSparseVector(dimension: Int): HashMap[Int, Double] =
    {
        val result: HashMap[Int, Double] = new HashMap[Int, Double]
        for (i <- 0 until dimension)
        {
            result(i) = 1.0
        }
        return result
    }

    def generateL1NormalizedRandomSparseVector(dimension: Int): HashMap[Int, Double] = 
    {
        val result: HashMap[Int, Double] = new HashMap[Int, Double]
        var s: Double = 0.0
        for (i <- 0 until dimension)
        {
            val ele: Double = Random.nextDouble
            result(i) = ele
            s += ele
        }
        for (i <- 0 until dimension)
        {
            result(i) = result(i)/s
        }
        return result
    }

    def addSparseVector[T](u: HashMap[Int, T], v: HashMap[Int, T]): HashMap[Int, T] = 
    {
        if (u.size == 0)
        {
            return v
        }
        if (v.size == 0)
        {
            return u
        }
        val result: HashMap[Int, T] = new HashMap[Int, T]
        val ukeys: Set[Int] = u.keys.to[Set]
        val vkeys: Set[Int] = v.keys.to[Set]
        val intersection: Set[Int] = ukeys.intersect(vkeys)
        val udiff: Set[Int] = ukeys.diff(intersection)
        val vdiff: Set[Int] = vkeys.diff(intersection)
        for (index <- udiff)
        {
            result(index) = u(index)
        }
        for (index <- vdiff)
        {
            result(index) = v(index)
        }
        for (index <- intersection)
        {
            result(index) = add(u(index), v(index))
        }
        return result
    }

    def negateSparseVector[T](vector: HashMap[Int, T]): HashMap[Int, T] =
    {
        val result: HashMap[Int, T] = new HashMap[Int, T]
        val keys: Array[Int] = vector.keys.to[Array]
        for (index <- keys)
        {
            result(index) = negate(vector(index))
        }
        return result
    }

    def subtractSparseVector[T](u: HashMap[Int, T], v: HashMap[Int, T]): HashMap[Int, T] = 
    {
        if (u.size == 0)
        {
            return negateSparseVector(v)
        }
        if (v.size == 0)
        {
            return u
        }
        val ukeys: Set[Int] = u.keys.to[Set]
        val vkeys: Set[Int] = v.keys.to[Set]
        val intersection: Set[Int] = ukeys.intersect(vkeys)
        val udiff: Set[Int] = ukeys.diff(intersection)
        val vdiff: Set[Int] = vkeys.diff(intersection)
        val result: HashMap[Int, T] = new HashMap[Int, T]
        for (index <- udiff)
        {
            result(index) = u(index)
        }
        for (index <- vdiff)
        {
            result(index) = negate(v(index))
        }
        for (index <- intersection)
        {
            result(index) = subtract(u(index), v(index))
        }
        return result
    }

    def negateSparseMatrix[T](matrix: HashMap[(Int, Int), T]): HashMap[(Int, Int), T] = 
    {
        val result: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
        val keys: Array[(Int, Int)] = matrix.keys.to[Array]
        for ((row, col) <- keys)
        {
            result((row, col)) = negate(matrix(row, col))
        }
        return result
    }

    def addSparseMatrix[T](a: HashMap[(Int, Int), T], b: HashMap[(Int, Int), T]): HashMap[(Int, Int), T] = 
    {
        val result: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
        val akeys: Set[(Int, Int)] = a.keys.to[Set]
        val bkeys: Set[(Int, Int)] = b.keys.to[Set]
        //val intersection: Array[(Int, Int)] = akeys.intersect(bkeys).to[Array]
        val union: Array[(Int, Int)] = akeys.union(bkeys).to[Array]
        for ((row, col) <- union)
        {
            result((row, col)) = add(a.getOrElse((row, col), 0).asInstanceOf[T], b.getOrElse((row, col), 0).asInstanceOf[T])
        }
        return result
    }

    def subtractSparseMatrix[T](a: HashMap[(Int, Int), T], b: HashMap[(Int, Int), T]): HashMap[(Int, Int), T] = 
    {
        val result: HashMap[(Int, Int), T] = new HashMap[(Int, Int), T]
        val akeys: Set[(Int, Int)] = a.keys.to[Set]
        val bkeys: Set[(Int, Int)] = b.keys.to[Set]
        val union: Array[(Int, Int)] = akeys.union(bkeys).to[Array]
        for ((row, col) <- union)
        {
            result((row, col)) = subtract(a.getOrElse((row, col), 0).asInstanceOf[T], b.getOrElse((row, col), 0).asInstanceOf[T])
        }
        return result
    }

    def saveSortedVertices(verticesFileName: String, stationaryStateFileName: String, outputFileName: String): Unit = 
    {
        assert(Files.exists(Paths.get(verticesFileName)))
        assert(Files.exists(Paths.get(stationaryStateFileName)))
        val stationaryStates: ArrayBuffer[(Int, Double)] = new ArrayBuffer[(Int, Double)]
        val vertices: ArrayBuffer[String] = new ArrayBuffer[String]
        for (line <- Source.fromFile(verticesFileName).getLines)
        {
            vertices.append(line)
        }
        for (line <- Source.fromFile(stationaryStateFileName).getLines)
        {
            val temp: Array[String] = line.split(":")
            val index: Int = temp(0).toInt
            val pageRankValue: Double = temp(1).toDouble
            stationaryStates.append((index, pageRankValue))
        }
        assert(stationaryStates.size == vertices.size)
        val sortedStates: Array[(Int, Double)] = stationaryStates.sortWith(_._2 > _._2).to[Array]
        val writer = new PrintWriter(new File(outputFileName))
        val pageRankValues: HashMap[Double, Int] = new HashMap[Double, Int]
        for (stateIndex <- sortedStates.indices)
        {
            val vertexIndex: Int = sortedStates(stateIndex)._1
            val pageRankValue: Double = sortedStates(stateIndex)._2
            if (pageRankValues.contains(pageRankValue))
            {
                pageRankValues(pageRankValue) += 1
            }
            else
            {
                pageRankValues(pageRankValue) = 1
            }
            writer.write((stateIndex + 1).toString + ";" + vertices(vertexIndex).toString + ":" + pageRankValue.toString + "\n")
        }
        writer.write("pageRankValue,valueCounts\n")
        val keys: Array[Double] = pageRankValues.keys.to[Array].sortWith(_ > _)
        for (key <- keys)
        {
            writer.write(key.toString + "," + pageRankValues(key).toString + "\n")
        }
        writer.close()
    }
    
    def getVertexStatistics(vertices: ArrayBuffer[String], outputFileName: String): Unit = 
    {
        import scala.collection.immutable.ListMap
        val counts: HashMap[String, Int] = new HashMap[String, Int]
        for (vertex <- vertices)
        {
            if (counts.contains(vertex))
            {
                counts(vertex) += 1
            }
            else
            {
                counts(vertex) = 1
            }
        }
        val sortedCounts = ListMap(counts.toSeq.sortWith(_._2 > _._2): _*)
        val writer = new PrintWriter(new File(outputFileName))
        writer.write("vertex_id,count\n")
        for (id <- sortedCounts.keys)
        {
            writer.write(id + "," + sortedCounts(id).toString + "\n")
        }
        writer.close()
    }

    def getVertexStatistics(vertices: ArrayBuffer[Vertex]): HashMap[Vertex, Int] = 
    {
        val result: HashMap[Vertex, Int] = new HashMap[Vertex, Int]
        for (i <- vertices.indices)
        {
            if (result.contains(vertices(i)))
            {
                result(vertices(i)) += 1
            }
            else
            {
                result(vertices(i)) = 1
            }
        }
        return result
    }
    
    def getStringStatistics(vertices: ArrayBuffer[String]): HashMap[String, Int] = 
    {
        val result: HashMap[String, Int] = new HashMap[String, Int]
        for (i <- vertices.indices)
        {
            if (result.contains(vertices(i)))
            {
                result(vertices(i)) += 1
            }
            else
            {
                result(vertices(i)) = 1
            }
        }
        return result
    }

    def countFrequenciesInPath(path: ArrayBuffer[String]): HashMap[String, Double] = 
    {
        val stats: HashMap[String, Int] = getStringStatistics(path)
        val result: HashMap[String, Double] = new HashMap[String, Double]
        val keys: Array[String] = stats.keys.to[Array]
        for (key <- keys)
        {
            result(key) = stats(key).toDouble/path.size.toDouble
        }
        return result
    }

    def sortHashMapByValue(map: HashMap[String, Double]): Array[(String, Double)] = 
    {
        val list: ArrayBuffer[(String, Double)] = new ArrayBuffer[(String, Double)]
        val keys: Array[String] = map.keys.to[Array]
        for (key <- keys)
        {
            list.append((key, map(key)))
        }
        val sortedList: Array[(String, Double)] = list.to[Array].sortWith(_._2 > _._2)
        return sortedList
    }
    
    def getVertexWeights(vertices: ArrayBuffer[(Vertex, Double)]): HashMap[Vertex, Double] = 
    {
        val result: HashMap[Vertex, Double] = new HashMap[Vertex, Double]
        for (data <- vertices)
        {
            val vertex: Vertex = data._1
            val weight: Double = data._2
            result(vertex) = result.getOrElse(vertex, 0.0) + weight
            /*if (result.contains(vertex))
            {
                result(vertex) += weight
            }
            else
            {
                result += (vertex -> weight)
            }*/
        }
        return result
    }

    def getRadiantWeights(adjacentVertices: Array[(Vertex, Double)]): Double = 
    {
        var result: Double = 0.0
        for ((vertex, weight) <- adjacentVertices)
        {
            result += weight
        }
        return result
    }

    def selectAtRandomByWeight(vertices: Array[(Vertex, Double)]): Int = 
    {
        assert(vertices.size > 0)
        val radiantWeights: Double = getRadiantWeights(vertices)
        val probabilityIntervals: ArrayBuffer[(Double, Double)] = new ArrayBuffer[(Double, Double)]
        var accumulator: Double = 0.0
        for ((vertex, weight) <- vertices)
        {
            probabilityIntervals.append((accumulator/radiantWeights, (accumulator + weight)/radiantWeights))
            accumulator += weight
        }
        val randomNumber: Double = Random.nextDouble
        for (i <- probabilityIntervals.indices)
        {
            val (left, right) = probabilityIntervals(i)
            if (randomNumber >= left && randomNumber < right)
            {
                return i
            }
        }
        return -1
    }

    def selectVertexByProbability(candidateVertices: HashMap[String, Double]): String = 
    {
        val probabilityIntervals: ArrayBuffer[(Double, Double)] = new ArrayBuffer[(Double, Double)]
        val keys: Array[String] = candidateVertices.keys.to[Array]
        var s: Double = 0.0
        for (key <- keys)
        {
            val left: Double = s
            val right: Double = left + candidateVertices(key)
            s = right
            probabilityIntervals.append((left, right))
        }
        assert(abs(s - 1.0) < 1.0e-15)
        val randomNumber: Double = Random.nextDouble
        for (index <- probabilityIntervals.indices)
        {
            val left: Double = probabilityIntervals(index)._1
            val right: Double = probabilityIntervals(index)._2
            if (randomNumber >= left && randomNumber < right)
            {
                return keys(index)
            }
        }
        println("Warning: probably some error with your candiate vertices. ")
        return "null"
    }

    def readAccts(inputFileName: String): ArrayBuffer[Set[String]] =
    {
        assert(Files.exists(Paths.get(inputFileName)))
        val result: ArrayBuffer[Set[String]] = new ArrayBuffer[Set[String]]
        for (line <- Source.fromFile(inputFileName).getLines)
        {
            val acct_set: Set[String] = line.replace("[", "").replace("]", "").replace("\"", "").split(",").to[Set]
            result.append(acct_set)
        }
        return result
    }

    def mergeUndirectedGraphs(a: Graph, b: Graph): Graph = 
    {
        assert(!a.isDirected && !b.isDirected)
        val combinedGraph = new Graph(a)
        val aVertexIds: Array[String] = a.vertices.map(vertex => vertex.id)
        val bVertexIds: Array[String] = b.vertices.map(vertex => vertex.id)
        val intersection: Set[String] = aVertexIds.to[Set].intersect(bVertexIds.to[Set])

        for (vid <- bVertexIds)
        {
            if (!combinedGraph.allVertices.contains(vid))
            {
                combinedGraph.allVertices += (vid -> new Vertex(b.allVertices(vid)))
            }
        }

        if (intersection.size == 0)
        {
            val bGraphMap: HashMap[Vertex, ArrayBuffer[(Vertex, Double)]] = b.graphMap
            for (vertex <- bGraphMap.keys.to[Array])
            {
                val bNeighbors: ArrayBuffer[(Vertex, Double)] = bGraphMap(vertex)
                val neighbors: ArrayBuffer[(Vertex, Double)] = new ArrayBuffer[(Vertex, Double)]
                for (i <- bNeighbors.indices)
                {
                    neighbors.append((combinedGraph.allVertices(bNeighbors(i)._1.id), bNeighbors(i)._2))
                }
                combinedGraph.graphMap += (combinedGraph.allVertices(vertex.id) -> neighbors)
            }
            combinedGraph.getVertices()
            combinedGraph.getEdges()
            return combinedGraph
        }

        for (vid <- bVertexIds)
        {
            if (intersection.contains(vid))
            {
                val bNeighbors: ArrayBuffer[(Vertex, Double)] = b.graphMap(b.allVertices(vid))
                val currentNeighbors: ArrayBuffer[(Vertex, Double)] = combinedGraph.graphMap(combinedGraph.allVertices(vid))
                for (j <- bNeighbors.indices)
                {
                    var currentWeight: Double = 0.0
                    var index: Int = -1
                    breakable
                    {
                        for (k <- currentNeighbors.indices)
                        {
                            if (currentNeighbors(k)._1.id == bNeighbors(j)._1.id)
                            {
                                currentWeight = currentNeighbors(k)._2
                                index = k
                                break
                            }
                        }
                    }
                    if (index >= 0)
                    {
                        currentNeighbors(index) = (combinedGraph.allVertices(bNeighbors(j)._1.id), currentWeight + bNeighbors(j)._2)
                    }
                    else
                    {
                        currentNeighbors.append((combinedGraph.allVertices(bNeighbors(j)._1.id), bNeighbors(j)._2))
                    }
                }
            }
            else
            {
                val bNeighbors: ArrayBuffer[(Vertex, Double)] = b.graphMap(b.allVertices(vid))
                val neighbors: ArrayBuffer[(Vertex, Double)] = new ArrayBuffer[(Vertex, Double)]
                for (j <- bNeighbors.indices)
                {
                    neighbors.append((combinedGraph.allVertices(bNeighbors(j)._1.id), bNeighbors(j)._2))
                }
                combinedGraph.graphMap += (combinedGraph.allVertices(vid) -> neighbors)
            }
        }
        combinedGraph.getVertices()
        combinedGraph.getEdges()
        return combinedGraph
    }

    def getBoundaryIndices(dimension: Int, numberOfWorkers: Int): (ArrayBuffer[Int], ArrayBuffer[Int]) =//(inclusive, inclusive) 
    {
        //assert(dimension/numberOfWorkers >= 1)
        val startIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        val endIndices: ArrayBuffer[Int] = new ArrayBuffer[Int]
        val quotient: Int = dimension/numberOfWorkers
        val remains: Int = dimension%numberOfWorkers
        var startIndex: Int = 0
        var endIndex: Int = -1
        for (i <- 0 until numberOfWorkers)
        {
            if (i < remains)
            {
                startIndex = endIndex + 1
                endIndex = startIndex + quotient
            }
            else
            {
                startIndex = endIndex + 1
                endIndex = startIndex + quotient - 1
            }
            startIndices.append(startIndex)
            endIndices.append(endIndex)
        }
        return (startIndices, endIndices)
    }

    def saveGraphMapToFile(graphMap: HashMap[Vertex, ArrayBuffer[(Vertex, Double)]], outputFileName: String): Unit = 
    {
        val vertices: Array[Vertex] = graphMap.keys.to[Array]
        val writer = new PrintWriter(new File(outputFileName))
        for (v <- vertices)
        {
            val neighbors: ArrayBuffer[(Vertex, Double)] = graphMap(v)
            val line: String = neighbors.map(ele => ele._1.id + "," + ele._2.toString).mkString(";")
            writer.write(v.id + "->" + line + "\n")
        }
        writer.close()
    }

    def readMatrixResult(matrixFileName: String): (Double, SparseVector[Double]) = 
    {
        assert(Files.exists(Paths.get(matrixFileName)))
        val lines: ArrayBuffer[String] = new ArrayBuffer[String]
        for (line <- Source.fromFile(matrixFileName).getLines)
        {
            lines.append(line)
        }
        val eigenvalue: Double = lines(0).toDouble
        val a: Array[Double] = lines(1).split(",").map(ele => ele.toDouble)
        val map: HashMap[Int, Double] = new HashMap[Int, Double]
        for (i <- a.indices)
        {
            map(i) = a(i).toDouble
        }
        val eigenvector: SparseVector[Double] = new SparseVector(map)
        return (eigenvalue, eigenvector)
    }

    def hashMapToArray(map: HashMap[String, Double]): Array[(String, Double)] = 
    {
        val buffer: ArrayBuffer[(String, Double)] = new ArrayBuffer[(String, Double)]
        val keys: Array[String] = map.keys.to[Array]
        for (key <- keys)
        {
            buffer.append((key, map(key)))
        }
        val sorted: Array[(String, Double)] = buffer.to[Array].sortWith(_._2 > _._2)
        return sorted
    }

    def readSparseVectorFromFile(fileName: String): SparseVector[Double] = 
    {
        assert(Files.exists(Paths.get(fileName)))
        val vector: SparseVector[Double] = new SparseVector[Double]
        val interval: Int = 100000
        var counter: Int = 0
        for (line <- Source.fromFile(fileName).getLines)
        {
            counter += 1
            if (counter % interval == 0)
            {
                println("Counter = " + counter)
            }
            val a: Array[String] = line.split(":")
            vector.map(a(0).toInt) = a(1).toDouble
        }
        return vector
    }

    def saveModularityInfo(A: SparseMatrix[Double], k: SparseVector[Double], r: SparseVector[Double], m: Double, communityIndices: ArrayBuffer[Int], modularityFileName: String): Unit = 
    {
        val writer = new PrintWriter(new File(modularityFileName))
        val keys1: Array[(Int, Int)] = A.map.keys.to[Array]
        for (i <- keys1.indices)
        {
            if (i < keys1.size - 1)
            {
                writer.write(keys1(i)._1.toString + "," + keys1(i)._2.toString + "," + A.map(keys1(i)).toString + ";")
            }
            else
            {
                writer.write(keys1(i)._1.toString + "," + keys1(i)._2.toString + "," + A.map(keys1(i)).toString + "\n")
            }
        }
        val keys2: Array[Int] = k.map.keys.to[Array]
        for (i <- keys2.indices)
        {
            if (i < keys2.size - 1)
            {
                writer.write(keys2(i).toString + "," + k.map(keys2(i)).toString + ";")
            }
            else
            {
                writer.write(keys2(i).toString + "," + k.map(keys2(i)).toString + "\n")
            }
        }
        val keys3: Array[Int] = r.map.keys.to[Array]
        for (i <- keys3.indices)
        {
            if (i < keys3.size - 1)
            {
                writer.write(keys3(i).toString + "," + r.map(keys3(i)).toString + ";")
            }
            else
            {
                writer.write(keys3(i).toString + "," + r.map(keys3(i)).toString + "\n")
            }
        }
        writer.write(m.toString + "\n")
        writer.write(communityIndices.map(ele => ele.toString).mkString(",") + "\n")
        writer.close()
    }

    def saveModularityInfo(A: SparseMatrix[Double], k: SparseVector[Double], m: Double, communityIndices: ArrayBuffer[Int], akmFileName: String, communityIndicesFileName: String): Unit = 
    {
        val writer = new PrintWriter(new File(communityIndicesFileName))
        writer.write(communityIndices.map(ele => ele.toString).mkString(",") + "\n")
        writer.close()
        if (!Files.exists(Paths.get(akmFileName)))
        {
            val writer = new PrintWriter(new File(akmFileName))
            val keys1: Array[(Int, Int)] = A.map.keys.to[Array]
            for (i <- keys1.indices)
            {
                if (i < keys1.size - 1)
                {
                    writer.write(keys1(i)._1.toString + "," + keys1(i)._2.toString + "," + A.map(keys1(i)).toString + ";")
                }
                else
                {
                    writer.write(keys1(i)._1.toString + "," + keys1(i)._2.toString + "," + A.map(keys1(i)).toString + "\n")
                }
            }
            val keys2: Array[Int] = k.map.keys.to[Array]
            for (i <- keys2.indices)
            {
                if (i < keys2.size - 1)
                {
                    writer.write(keys2(i).toString + "," + k.map(keys2(i)).toString + ";")
                }
                else
                {
                    writer.write(keys2(i).toString + "," + k.map(keys2(i)).toString + "\n")
                }
            }
            writer.write(m.toString + "\n")
            writer.close()
        }
    }
    
    def readEig(eigenResultsFileName: String): (Double, SparseVector[Double], Double) = 
    {
        assert(Files.exists(Paths.get(eigenResultsFileName)))
        val lines: Array[String] = Source.fromFile(eigenResultsFileName).getLines.to[Array]
        assert(lines.size == 3)
        val eigenvalue: Double = lines(0).toDouble
        val eigenvector: SparseVector[Double] = new SparseVector
        val temp: Array[Double] = lines(1).split(",").map(ele => ele.toDouble)
        for (i <- temp.indices)
        {
            if (temp(i) != 0)
            {
                eigenvector.map(i) = temp(i)
            }
        }
        val modularity: Double = lines.last.toDouble
        return (eigenvalue, eigenvector, modularity)
    }

    def binarySearch(target: Int, array: Array[Int]): Int = 
    {
        val length: Int = array.size
        if (length == 0)
        {
            return -1
        }
        if (length == 1)
        {
            if (target == array.head)
            {
                return 0
            }
            else
            {
                return -1
            }
        }
        var start: Int = 0
        var end: Int = length - 1
        var middle: Int = (start + end)/2
        while(start <= end)
        {
            if (target == array(middle))
            {
                return middle
            }
            else if (target > array(middle))
            {
                start = middle + 1
                middle = (start + end)/2
            }
            else
            {
                end = middle - 1
                middle = (start + end)/2
            }
        }
        return -1
    }

    def binarySearch(id: String, array: Array[String]): Boolean = 
    {
        if (array.size == 0)
        {
            return false
        }
        if (array.size == 1)
        {
            return array(0) == id
        }
        val length: Int = array.size
        var start: Int = 0
        var end: Int = length - 1
        var middle: Int = (start + end)/2
        while (start <= end)
        {
            if (id == array(middle))
            {
                return true
            }
            else if (id > array(middle))
            {
                start = middle + 1
            }
            else
            {
                end = middle - 1
            }
            middle = (start + end)/2
        }
        return false
    }

    def getAverage(array: ArrayBuffer[Double]): Double = 
    {
        var s: Double = 0.0
        for (i <- array)
        {
            s += i
        }
        return s/array.size.toDouble
    }

    def getVariance(array: ArrayBuffer[Double], mean: Double): Double = 
    {
        var s: Double = 0.0
        for (i <- array)
        {
            s += (i - mean)*(i - mean)
        }
        return s/array.size.toDouble
    }

    def updateGraph(graph: HashMap[String, ArrayBuffer[(String, Double)]], left: String, right: String, weight: Double): Unit = 
    {
        if (graph.contains(left))
        {
            graph(left).append((right, weight))
        }
        else
        {
            graph(left) = ArrayBuffer[(String, Double)]((right, weight))
        }
    }

    def filterDegree(inputFileName: String, outputFileName: String, directed: Boolean, degreeLowerBound: Int): Unit = 
    {
        assert(Files.exists(Paths.get(inputFileName)))
        val graph: HashMap[String, ArrayBuffer[(String, Double)]] = new HashMap[String, ArrayBuffer[(String, Double)]]
        if (!directed)
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                val a: Array[String] = line.split(";")
                assert(a.size >= 2)
                val left: String = a(0)
                val right: String = a(1)
                var weight: Double = 1.0
                if (a.size == 3)
                {
                    weight = a(2).toDouble
                }
                if (left != right)
                {
                    updateGraph(graph, left, right, weight)
                    updateGraph(graph, right, left, weight)
                }
            }
        }
        else
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                val a: Array[String] = line.split(";")
                assert(a.size >= 2)
                val left: String = a(0)
                val right: String = a(1)
                
                var weight: Double = 1.0
                if (a.size == 3)
                {
                    weight = a(2).toDouble
                }

                if (left != right)
                {
                    updateGraph(graph, left, right, weight)
                }
            }
        }

        val writer = new PrintWriter(new File(outputFileName))
        val vertices: Array[String] = graph.keys.to[Array]
        for (vertex <- vertices)
        {
            if (graph(vertex).size >= degreeLowerBound)
            {
                val neighbors: ArrayBuffer[(String, Double)] = graph(vertex)
                for ((neighbor, weight) <- neighbors)
                {
                    if (graph(neighbor).size >= degreeLowerBound && vertex <= neighbor)
                    {
                        writer.write(vertex + ";" + neighbor + ";" + weight.toString + "\n")
                    }
                }
            }
        }
        writer.close()
    }

    def getVertexWeight(graphMap: HashMap[Vertex, ArrayBuffer[(Vertex, Double)]], vertex: Vertex): Double = 
    {
        var totalWeight: Double = 0
        val neighbors: ArrayBuffer[(Vertex, Double)] = graphMap.getOrElse(vertex, new ArrayBuffer[(Vertex, Double)])
        for ((v, weight) <- neighbors)
        {
            totalWeight += weight
        }
        return totalWeight
    }

    def sparsifyEdges(edgeFileName: String, outputFileName: String, probability: Double): Unit = 
    {
        assert(Files.exists(Paths.get(edgeFileName)))
        assert(probability < 1.0 && probability > 0)
        val writer = new PrintWriter(new File(outputFileName))
        val removedEdgesWriter = new PrintWriter(new File("removed_edges.csv"))
        for (line <- Source.fromFile(edgeFileName).getLines)
        {
            val random: Double = Random.nextDouble
            if (random > probability)
            {
                writer.write(line + "\n")
            }
            else
            {
                removedEdgesWriter.write(line + "\n")
            }
        }
        removedEdgesWriter.close()
        writer.close()
    }
}
