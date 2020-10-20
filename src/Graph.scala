//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved. 

package frustrated.random.walk 

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap, Stack, Queue}
import java.nio.file.{Files, Paths}
import java.io.{PrintWriter, File}
import scala.math._
import util.control.Breaks._
import java.util.Calendar
import scala.util.Random
import sys.process._

class Graph
{
    var vertices: Array[Vertex] = Array[Vertex]() //Vertex set of the graph. 
    val allVertices: HashMap[String, Vertex] = new HashMap[String, Vertex]//String is id. Given a vertex id, we can retreive its corresponding vertex. When we need to modify a vertex, 
    //we always obtain the vertex from this data structure to gurantee that we are modifying the vertex itself, rather than a copy. 
    val vertexIndex: HashMap[Vertex, Int] = new HashMap[Vertex, Int]//Given a vertex, we can retrieve its corresponding index. 
    val adjacencyMap: HashMap[(Int, Int), Double] = new HashMap[(Int, Int), Double]//The adjacency matrix is represented as a sparse matrix.
    // Only non-zero matrix elements are stored. Given row and col index, we can retrieve its matrix element value. 
    val graphMap: HashMap[Vertex, ArrayBuffer[(Vertex, Double)]] = new HashMap[Vertex, ArrayBuffer[(Vertex, Double)]]//This is used to store the graph structure.
    //The graph structure can be reconstructed from graphMap. Given vertex, we can retrieve all its neighboring vertices, 
    //and the corresponding edge weight for each neighboring vertex. 
    val edges: ArrayBuffer[Edge] = new ArrayBuffer[Edge]//All edges in the graph are stored in this data structure. 
    val graphEdgeMap: HashMap[Vertex, ArrayBuffer[Edge]] = new HashMap[Vertex, ArrayBuffer[Edge]]//Given a vertex, we can retrieve all the edges it radiates. 
    val transferMap: HashMap[(Int, Int), Double] = new HashMap[(Int, Int), Double]//Transfer matrix used in PageRank algorithm is created from this HashMap. Generally, the transfer
    //matrix is sparse, like the adjacency matrix. Thus, here I represent the transfer matrix again using a sparse matrix. Given row and col indices, we can retrieve its corresponding
    //matrix element value. Only non-zero matrix elements are stored. 
    var adjacencyMatrix: SparseMatrix[Double] = new SparseMatrix[Double](adjacencyMap)//Adjacency matrix is created here.
    var transferMatrix: SparseMatrix[Double] = new SparseMatrix[Double](transferMap)//Transfer matrix is created here. 
    var alpha: Double = 0.85 //Damping factor used in PageRank algorithm. 
    var isDirected: Boolean = false //Whether the graph is directed or not.
    var isSimple: Boolean = true//Whether the graph contains self-loop. A simple graph contains no self-loop. 
    var pleonastic: Boolean = false //Whether to print out the intermediate results.
    var laplaceMatrix: SparseMatrix[Double] = new SparseMatrix[Double]
    var communityIterationCounter: Int = 0
    var numberOfCommunities: Int = 1
    var numberOfSCCs: Int = 0
    val sccSizes: HashMap[String, Int] = new HashMap[String, Int]
    var numberOfEdges: Int = 0
    var vertexCounter: Int = 0

    def printGraphMap: Unit = 
    {
        val keys: Array[Vertex] = this.graphMap.keys.to[Array]
        println("***********************************************")
        for (key <- keys)
        {
            println(key + " -> " + this.graphMap(key).map(pair => pair._1.toString + ", weight:" + pair._2.toString).mkString(" ;; "))
        }
        println("***********************************************")
    }

    /*Copy constructor. Create a new graph from an existing graph. Deep copy is implemented. */
    def this(that: Graph) = 
    {
        this()
        this.edges.clear
        this.graphMap.clear
        this.graphEdgeMap.clear
        this.allVertices.clear
        this.isDirected = that.isDirected
        this.isSimple = that.isSimple
        this.numberOfEdges = that.numberOfEdges
        for (id <- that.allVertices.keys.to[Array])
        {
            this.allVertices += (id -> new Vertex(that.allVertices(id)))
        }
        this.getVertices(false)//false means vertices are not saved into a file. 
        val thatKeys: Array[Vertex] = that.graphMap.keys.to[Array]
        for (i <- thatKeys.indices)
        {
            val neighbors: ArrayBuffer[(Vertex, Double)] = new ArrayBuffer[(Vertex, Double)]
            val thatNeighbors: ArrayBuffer[(Vertex, Double)] = that.graphMap(thatKeys(i))
            for (j <- thatNeighbors.indices)
            {
                neighbors.append((this.allVertices(thatNeighbors(j)._1.id), thatNeighbors(j)._2))
                //neighbors.append((new Vertex(thatNeighbors(j)._1), thatNeighbors(j)._2))
            }
            this.graphMap += (this.allVertices(thatKeys(i).id) -> neighbors) //(new Vertex(thatKeys(i)) -> neighbors)
        }
    }

    /*This function takes in a set of vertices, and creates a complete graph from that set. 
    By Complete graph, I mean a graph in which each vertex is conncted to all the other veritces. 
     */
    def generateCompleteUndirectedGraph(vertexSet: Set[String], weight: Double, label: String): Unit = 
    {
        this.isDirected = false
        this.isSimple = true
        this.edges.clear
        this.graphMap.clear
        this.graphEdgeMap.clear
        this.allVertices.clear
        val temp: Array[String] = vertexSet.to[Array]
        for (i <- temp.indices)
        {
            val vertex: Vertex = new Vertex(temp(i), label)
            this.allVertices += (temp(i) -> vertex)
        }
        this.getVertices(false)
        for (vertex <- this.vertices)
        {
            val id: String = vertex.id
            this.graphMap += (vertex -> new ArrayBuffer[(Vertex, Double)])
            for (i <- temp.indices)
            {
                if (id != temp(i))
                {
                    this.graphMap(vertex).append((this.allVertices(temp(i)), weight))
                }
            }
        }
        this.numberOfEdges = vertexSet.size * (vertexSet.size - 1)/2
    }

    /*This function is used to update graphMap. If source vertex is already contained in the graphMap, 
     then append the destination vertex to its corresponding neighbor list; else create a neighbor list to
     store the destination vertex. The edge connecting the source vertex and the destination vertex is assigned weight = connectionWeight. */
    def updateGraphMap(sourceVertex: Vertex, destinationVertex: Vertex, connectionWeight: Double): Unit = 
    {
        if (this.graphMap.contains(sourceVertex))
        {
            this.graphMap(sourceVertex).append((destinationVertex, connectionWeight))
        }
        else
        {
            this.graphMap += (sourceVertex -> ArrayBuffer[(Vertex, Double)]((destinationVertex, connectionWeight)))
        }
    }
   
    /*This function takes in a graph file and creates a graph. 
     The graph file contains three columns. The first column contains source vertices, the second column contains the 
     destination vertices, and the third column contains the edge weight of the edge connecting source to destination. 
     The first and second columns are required, whereas the third column is optional. In absence of the third column, the edge 
     weight is assigned a default value 1.0. */
    /*@param inputFileName: graph file. 
    @param separator: used to split each line in graph file. 
    @param isSimple: Whether the graph should contain self loops or not. A simple graph is a graph that does not have any self loop for each of its nodes.
    */
    def readFile(inputFileName: String, separator: String, isSimple: Boolean = true): Unit = 
    {
        this.isSimple = isSimple
        val label: String = "ACCT"
        assert(Files.exists(Paths.get(inputFileName)))
        val allIds: ArrayBuffer[String] = new ArrayBuffer[String]
        var lineCounter: Int = 0
        val interval: Int = 5000000
        if (isSimple)
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter%interval == 0)
                {
                    println("Number of lines processed = " + lineCounter + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    allIds.append(s)
                    allIds.append(d)
                    if (!this.allVertices.contains(s))
                    {
                        val sourceVertex: Vertex = new Vertex(s, label)
                        allVertices += (s -> sourceVertex)
                    }
                    if (!this.allVertices.contains(d))
                    {
                        val destinationVertex: Vertex = new Vertex(d, label)
                        allVertices += (d -> destinationVertex)
                    }
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
            }
        }
        else
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter%interval == 0)
                {
                    println("Number of lines processed = " + lineCounter + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    allIds.append(s)
                    allIds.append(d)
                    if (!this.allVertices.contains(s))
                    {
                        val sourceVertex: Vertex = new Vertex(s, label)
                        allVertices += (s -> sourceVertex)
                    }
                    if (!this.allVertices.contains(d))
                    {
                        val destinationVertex: Vertex = new Vertex(d, label)
                        allVertices += (d -> destinationVertex)
                    }
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
                else
                {
                    allIds.append(s)
                    if (!this.allVertices.contains(s))
                    {
                        val sourceVertex: Vertex = new Vertex(s, label)
                        allVertices += (s -> sourceVertex)
                    }
                    this.updateGraphMap(this.allVertices(s), this.allVertices(s), connectionWeight)
                }
            }
        }
        
        this.numberOfEdges = lineCounter
        Utilities.saveGraphMapToFile(this.graphMap, "graph_map.txt")
        Utilities.getVertexStatistics(allIds, "vertex_counts.txt")
        if (false && this.graphMap.size < 20)
        {
            val keys: Array[Vertex] = this.graphMap.keys.to[Array]
            for (key <- keys)
            {
                println(key + " -> " + this.graphMap(key).map(ele => ele._1.toString).mkString("; "))
            }
        }
    }
   
    /*This function is an optimized version of the function readFile. This function is recommended for reading a large graph file. 
    @param inputFileName: This file contains all the edges in a graph. 
    @param separator: edge.split(separator)
     */
    def fastReadFile(inputFileName: String, separator: String, isSimple: Boolean = true): Unit =
    {
        this.isSimple = isSimple
        val label: String = "ACCT"
        assert(Files.exists(Paths.get(inputFileName)))
        var lineCounter: Int = 0
        val interval: Int = 5000000
        val verticesBuffer: ArrayBuffer[String] = new ArrayBuffer[String]
        println("Reading all the lines ..., time = " + Calendar.getInstance.getTime)
        if (isSimple)
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter%interval == 0)
                {
                    println("Number of lines read = " + lineCounter + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                if (a.size > 1)
                {
                    val s: String = a(0)
                    val d: String = a(1)
                    if (s != d)
                    {
                        verticesBuffer.append(s)
                        verticesBuffer.append(d)
                    }
                }
                else
                {
                    println("Error: " + line)
                }
            }
        }
        else
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter%interval == 0)
                {
                    println("Number of lines read = " + lineCounter + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                if (a.size > 1)
                {
                    val s: String = a(0)
                    val d: String = a(1)
                    verticesBuffer.append(s)
                    verticesBuffer.append(d)
                }
                else
                {
                    println("Error: " + line)
                }
            }
        }
        
        this.numberOfEdges = lineCounter
        println("Generating vertex set ... , time = " + Calendar.getInstance.getTime)
        val vertexSet: Array[String] = verticesBuffer.to[Set].to[Array]
        verticesBuffer.clear
        for (i <- vertexSet.indices)
        {
            this.allVertices += (vertexSet(i) -> new Vertex(vertexSet(i), label))
        }
        println("Generating graph map ..., time = " + Calendar.getInstance.getTime)
        lineCounter = 0
        if (isSimple)
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter % interval == 0)
                {
                    println("Number of lines processed = " + lineCounter.toString + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
            }
        }
        else
        {
            for (line <- Source.fromFile(inputFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter % interval == 0)
                {
                    println("Number of lines processed = " + lineCounter.toString + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
                else
                {
                    this.updateGraphMap(this.allVertices(s), this.allVertices(s), connectionWeight)
                }
            }
        }

        println("Saving the graph map ... , time = " + Calendar.getInstance.getTime)
        Utilities.saveGraphMapToFile(this.graphMap, "graph_map.txt")
    }

    /*
    This function is to generate graph from edge file and vertex set file. 
    @param edgeFileName: This file contains all the edges in the graph. 
    @param vertexSetFileName: This file contains all the distinct vertices in the graph. 
    @param separator: A string that separates the two ends of each edge. 
    */
    def fastReadGraphFromEdgeAndVertices(edgeFileName: String, vertexSetFileName: String, separator: String, isSimple: Boolean = true): Unit =
    {
        this.isSimple = isSimple
        val label: String = "label"
        assert(Files.exists(Paths.get(edgeFileName)))
        assert(Files.exists(Paths.get(vertexSetFileName)))
        var counter: Int = 0
        println("Reading vertex set ... ")
        for (line <- Source.fromFile(vertexSetFileName).getLines)
        {
            counter += 1
            this.allVertices(line) = new Vertex(line, label)
        }
        println("Reading all the edges from file " + edgeFileName)
        var lineCounter: Int = 0
        val interval: Int = 100000
        if (isSimple)
        {
            for (line <- Source.fromFile(edgeFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter % interval == 0)
                {
                    println("Number of lines processed = " + lineCounter.toString + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
            }
        }
        else
        {
            for (line <- Source.fromFile(edgeFileName).getLines)
            {
                lineCounter += 1
                if (this.pleonastic && lineCounter % interval == 0)
                {
                    println("Number of lines processed = " + lineCounter.toString + ", time = " + Calendar.getInstance.getTime)
                }
                val a: Array[String] = line.split(separator)
                val s: String = a(0)
                val d: String = a(1)
                var connectionWeight: Double = 1.0
                if (a.size == 3)
                {
                    connectionWeight = a(2).toDouble
                }
                if (s != d)
                {
                    if (this.isDirected)
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                    }
                    else
                    {
                        this.updateGraphMap(this.allVertices(s), this.allVertices(d), connectionWeight)
                        this.updateGraphMap(this.allVertices(d), this.allVertices(s), connectionWeight)
                    }
                }
                else
                {
                    this.updateGraphMap(this.allVertices(s), this.allVertices(s), connectionWeight)
                }
            }
        }
        this.numberOfEdges = lineCounter
    }

    /**
    This function is used to create undirected graph from input file.
    The difference between this method and the fastReadFile is that this function can only be used to create undirected graphs. 
    The advantage is that this method uses less memory. 
    @param inputFileName: A csv file that contains all the edges in the graph. Since the graph is undirected, the graph edges have no direction. 
    @param separator: A string the separates the two end points of each edge. 
    **/
    def createUndirectedGraphFromFile(inputFileName: String, separator: String, isSimple: Boolean = true): Unit = 
    {
        this.isSimple = isSimple
        assert(!this.isDirected)
        val label: String = "ACCT"
        assert(Files.exists(Paths.get(inputFileName)))
        var lineCounter: Int = 0
        val interval: Int = 5000000
        val verticesBuffer: ArrayBuffer[String] = new ArrayBuffer[String]
        println("Reading all the lines ..., time = " + Calendar.getInstance.getTime)
        for (line <- Source.fromFile(inputFileName).getLines)
        {
            lineCounter += 1
            if (this.pleonastic && lineCounter%interval == 0)
            {
                println("Number of lines read = " + lineCounter + ", time = " + Calendar.getInstance.getTime)
            }
            val a: Array[String] = line.split(separator)
            val s: String = a(0)
            val d: String = a(1)
            if (s != d)
            {
                verticesBuffer.append(s)
                verticesBuffer.append(d)
            }
            else
            {
                if (!isSimple)
                {
                    verticesBuffer.append(s)
                }
            }
        }
        this.numberOfEdges = lineCounter
        println("Generating vertex set ... , time = " + Calendar.getInstance.getTime)
        val vertexSet: Array[String] = verticesBuffer.to[Set].to[Array]
        verticesBuffer.clear
        for (i <- vertexSet.indices)
        {
            this.allVertices += (vertexSet(i) -> new Vertex(vertexSet(i), label))
            this.graphMap(this.allVertices(vertexSet(i))) = ArrayBuffer[(Vertex, Double)]()
        }
        println("Generating graph map ..., time = " + Calendar.getInstance.getTime)
        lineCounter = 0
        for (line <- Source.fromFile(inputFileName).getLines)
        {
            lineCounter += 1
            if (this.pleonastic && lineCounter % interval == 0)
            {
                println("Number of lines processed = " + lineCounter.toString + ", time = " + Calendar.getInstance.getTime)
            }
            val a: Array[String] = line.split(separator)
            val s: String = a(0)
            val d: String = a(1)
            var connectionWeight: Double = 1.0
            if (a.size == 3)
            {
                connectionWeight = a(2).toDouble
            }
            if (s != d)
            {
                val sourceVertex: Vertex = this.allVertices(s)
                val destinationVertex: Vertex = this.allVertices(d)
                this.graphMap(sourceVertex).append((destinationVertex, connectionWeight))
                this.graphMap(destinationVertex).append((sourceVertex, connectionWeight))
            }
            else
            {
                if (!isSimple)
                {
                    val sourceVertex: Vertex = this.allVertices(s)
                    val destinationVertex: Vertex = this.allVertices(s)
                    this.graphMap(sourceVertex).append((destinationVertex, connectionWeight))
                    this.graphMap(destinationVertex).append((sourceVertex, connectionWeight))
                }
            }
        }
        println("Saving the graph map ... , time = " + Calendar.getInstance.getTime)
        Utilities.saveGraphMapToFile(this.graphMap, "graph_map.txt")
        println("Saving all the vertices ..., time = " + Calendar.getInstance.getTime)
        val writer = new PrintWriter(new File("vertices.txt"))
        for (i <- vertexSet.indices)
        {
            writer.write(vertexSet(i) + "\n")
        }
        writer.close()
        println("All vertices have been saved. Graph creation completed. Time = " + Calendar.getInstance.getTime)
    }

    /*This function fills the vertices buffer with vertices retrieved from this.allVertices. 
     The vertices in the graph are sorted as strings, and each vertex is assigned an index. This index is used to enumerate 
     the row and column indices of adjacency matrix and transfer matrix. */
    def getVertices(iprint: Boolean = true): Unit = 
    {
        /*val sources: Array[Vertex] = this.graphMap.keys.to[Array]
        val duplicateVertices: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]
        for (vertex <- sources)
        {
            duplicateVertices.append(vertex)
            for (adj <- graphMap(vertex))
            {
                duplicateVertices.append(adj._1)
            }
        }*/
        val keys: Array[String] = this.allVertices.keys.to[Array]
        val verticesBuffer: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]
        for (vertexId <- keys)
        {
            verticesBuffer.append(this.allVertices(vertexId))
        }
        this.vertices = verticesBuffer.to[Array].sortWith(_ < _)
        if (iprint)
        {
            val writer = new PrintWriter(new File("vertices.txt"))
            for (i <- this.vertices.indices)
            {
                writer.write(vertices(i).id + "\n")
            }
            writer.close()
        }
        
        for (i <- this.vertices.indices)
        {
            this.vertexIndex(this.vertices(i)) = i
        }
    }

    /*This function fills edges ArrayBuffer. graphEdgeMap is instantiated here. 
    graphEdgeMap takes each vertex as key, and gives its corrsponding neighboring edges. 
    Warning: This function is too expensive to call. Avoid using it. 
    */
    def getEdges(iprint: Boolean = true): Unit = 
    {
        this.edges.clear
        this.graphEdgeMap.clear
        val sources: Array[Vertex] = this.graphMap.keys.to[Array]
        var counter: Int = 0
        val interval: Int = 100000
        for (i <- sources.indices)
        {
            if ((i+1)%interval == 0)
            {
                println("i = " + (i+1) + ", total = " + sources.size)
            }
            val vertex: Vertex = sources(i)
            val radiantEdges: ArrayBuffer[Edge] = new ArrayBuffer[Edge]
            val adjacentVertices: ArrayBuffer[(Vertex, Double)] = this.graphMap(vertex)
            val adjacentVertexWeights: HashMap[Vertex, Double] = new HashMap[Vertex, Double]//Utilities.getVertexWeights(adjacentVertices)
            for ((v, w) <- adjacentVertices)
            {
                adjacentVertexWeights(v) = w
            }
            val keys: Array[Vertex] = adjacentVertexWeights.keys.to[Array]
            for (adjacentVertex <- keys)
            {
                val left: Vertex = vertex
                val right: Vertex = adjacentVertex
                counter += 1
                val id: String = counter.toString
                val weight: Double = adjacentVertexWeights(adjacentVertex)
                val label: String = "ShareUser"
                val edge: Edge = new Edge(left, right, weight, id, label)
                this.edges.append(edge)
                radiantEdges.append(edge)
            }
            this.graphEdgeMap += (vertex -> radiantEdges)
        }

        if (iprint && graphEdgeMap.size < 10)
        {
            for (key <- graphEdgeMap.keys)
            {
                println(key.toString + " : " + graphEdgeMap(key).map(_.toString).mkString("||"))
            }
        }
       
        if (false && iprint)
        {
            var writer = new PrintWriter(new File("edges.txt"))
            for (edge <- this.edges)
            {
                writer.write(edge.toString + "\n")
            }
            writer.close()

            writer = new PrintWriter(new File("graphEdgeMap.txt"))
            for (vertex <- graphEdgeMap.keys)
            {
                writer.write(vertex.toString + " : " + graphEdgeMap(vertex).map(_.toString).mkString("||") + "\n")
            }
            writer.close()
        }
    }

    def getNodeDegrees(outputFileName: String): HashMap[String, Double] =
    {
        val degrees: HashMap[String, Double] = new HashMap[String, Double]
        if (this.vertices.size == 0)
        {
            this.getVertices(false)
        }
        for (vertex <- this.vertices)
        {
            var sum: Double = 0.0
            val adjacencyList: Array[(Vertex, Double)] = this.graphMap(vertex).to[Array]
            for ((v, weight) <- adjacencyList)
            {
                sum += weight
            }
            degrees(vertex.id) = sum
        }
        val writer = new PrintWriter(new File(outputFileName))
        val sortedPairs: Array[(String, Double)] = degrees.to[Array].sortBy(ele => ele._2).reverse
        for ((id, weight) <- sortedPairs)
        {
            writer.write(id + " -> " + weight.toString + "\n")
        }
        writer.close()
        return degrees
    }

    /*This function instantiates the adjacency matrix. Adjacency Matrix values are either 0 or 1. It is 0 if 
     there is no edge connection, and 1 otherwise. This is a sparse matrix. */
    def createAdjacencyMatrix: Unit = 
    {
        if (this.vertices.size == 0)
        {
            this.getVertices(false)
        }
        for (vertex <- this.vertices)
        {
            val adjacencyList: Array[(Vertex, Double)] = graphMap(vertex).to[Array]
            for (adj <- adjacencyList)
            {
                val row: Int = vertexIndex(vertex)
                val col: Int = vertexIndex(adj._1)
                val weight: Double = adj._2
                this.adjacencyMap((row, col)) = weight
            }
        }
        //Utilities.printSparseMatrix(adjacencyMap) 
        //Utilities.printSparseMatrix(adjacencyMap, this.vertices.size, this.vertices.size)
        adjacencyMatrix = new SparseMatrix(adjacencyMap)
        //adjacencyMatrix.printMatrix()
        //adjacencyMatrix.printMatrix(true)
    }


    /*Transfer matrix as used by PageRank is created here. 
    See http://www.ams.org/publicoutreach/feature-column/fcarc-pagerank for a thorough explanation of PageRank algorithm.*/
    def createTransferMatrix(transferMatrixFileName: String = "transfer_matrix.txt"): Unit = 
    {
        this.transferMap.clear
        val interval: Int = 10000
        for (i <- this.vertices.indices)
        {
            val vertex: Vertex = this.vertices(i)
            //val radiantEdges: Array[Edge] = this.graphEdgeMap.getOrElse(vertex, new ArrayBuffer[Edge]()).to[Array]
            val neighbors: ArrayBuffer[(Vertex, Double)] = this.graphMap.getOrElse(vertex, new ArrayBuffer[(Vertex, Double)]())
            var outWeight: Double = 0.0
            for ((v, weight) <- neighbors)
            {
                outWeight += weight
            }
            for ((thatVertex, weight) <- neighbors)
            {
                val row: Int = this.vertexIndex(vertex)
                val col: Int = this.vertexIndex(thatVertex)
                if (transferMap.contains((col, row)))
                {
                    this.transferMap((col, row)) += weight/outWeight
                }
                else
                {
                    this.transferMap((col, row)) = weight/outWeight
                }
            }
            if (this.pleonastic && (i+1)%interval == 0)
            {
                println("Number of vertices processed = " + (i+1) + ", total = " + this.vertices.size + ", time = " + Calendar.getInstance.getTime)
            }
        }

        //Utilities.printSparseMatrix(transferMap)
        //Utilities.printSparseMatrix(transferMap, this.vertices.size, this.vertices.size)
        transferMatrix = new SparseMatrix(transferMap)//.transpose
        println("Saving transfer matrix to file " + transferMatrixFileName)
        transferMatrix.save(transferMatrixFileName)
        println("transfer matrix created. ")
        if (false && this.vertices.size < 20)
        {
            transferMatrix.printMatrix(true)
            //transferMatrix.printToMathematicaFormat
            transferMatrix.printToLatexFormat
        }
        /*println("Row vectors of transfer matrix: ")
        val rowVectors: HashMap[Int, HashMap[Int, Double]] = Utilities.splitSparseMatrixByRow(transferMatrix.map)
        val keys: Array[Int] = rowVectors.keys.to[Array].sorted
        for (row <- keys)
        {
            println(row + " -> " + Utilities.sparseVectorToString(rowVectors(row)))
        }
        println("Col vectors of transfer matrix: ")
        val colVectors: HashMap[Int, HashMap[Int, Double]] = Utilities.splitSparseMatrixByCol(transferMatrix.map)
        val colKeys: Array[Int] = colVectors.keys.to[Array].sorted
        for (col <- keys)
        {
            println(col + " -> " + Utilities.sparseVectorToString(colVectors(col)))
        }*/
    }

    def createTransferMatrix: Unit = this.createTransferMatrix("transfer_matrix.txt")

    /***
     This function creates the probability transition matrix from a source vertex to the target vertex, and calls a Python program to
     calculate the hitting times from each non-target vertex to the target node. Here we use a simple random walk process.  
    */
    def calculateHittingTimesOfSimpleRandomWalks(target: String, transitionMatrixFileName: String = "transition_matrix.txt"): Unit = 
    {
        val transitionMatrix: SparseMatrix[Double] = new SparseMatrix[Double]
        //val initialProbability: SparseVector[Double] = new SparseVector[Double]
        if (this.vertices.size == 0)
        {
            println("Creating all the vertices in the graph ...")
            val iprint: Boolean = true
            this.getVertices(iprint)
        }
        val targetVertex: Vertex = this.allVertices(target)
        val adherents: ArrayBuffer[String] = new ArrayBuffer[String]//This is used to store all the loyal adherents to target. A vertex is called a loyal adherent to target if this vertex has only one single neighbor, and this very neighbor is exactly the target we are interested in. 
        for (i <- this.vertices.indices)
        {
            val sourceVertex: Vertex = this.vertices(i)
            if (sourceVertex.id != target)
            {
                val neighbors: ArrayBuffer[(Vertex, Double)] = this.graphMap.getOrElse(sourceVertex, new ArrayBuffer[(Vertex, Double)]())
                var outweight: Double = 0.0
                var isAdjacentToTarget: Boolean = false//if the source vertex is adjacent to target
                for ((v, weight) <- neighbors)
                {
                    if (v.id == target)
                    {
                        isAdjacentToTarget = true//source vertex is adjacent to target. 
                    }
                    outweight += weight
                }
                for ((thatVertex, weight) <- neighbors)
                {
                    if (thatVertex.id != target)
                    {
                        val row: Int = this.vertexIndex(sourceVertex)
                        val col: Int = this.vertexIndex(thatVertex)
                        if (true || neighbors.size != 1)
                        {
                            transitionMatrix.map((row, col)) = weight/outweight
                            //if (isAdjacentToTarget)
                            //{
                            //    initialProbability.map(row) = weight/outweight
                            //}
                        }
                    }
                    else
                    {
                        if (neighbors.size == 1)//found a loyal adherent to target
                        {
                            adherents.append(sourceVertex.id)//This sourceVertex has only one neighbor, and this neighbor is our target. 
                        }
                    }
                }
            }
        }
        println("Saving probability transition matrix to file " + transitionMatrixFileName)
        transitionMatrix.save(transitionMatrixFileName)
        //println("Saving initial probability vector to file " + initialProbabilityFileName)
        //initialProbability.save(initialProbabilityFileName)
        if (true)
        {
            println("Saving all the loyal adherents to vertex " + target)
            val writer = new PrintWriter(new File("adherents.txt"))
            writer.write("target = " + target + "\n")
            for (v <- adherents)
            {
                writer.write(v + "\n")
            }
            writer.close()
        }
        println("Calling python program to calculate the expected hitting times ... ")
        ("python calculate_hitting_time.py " + transitionMatrixFileName + " vertices.txt adherents.txt").!
        ("mv hitting_times_mean_variance.txt " + target + ".txt").!
    }

    /***
     This function creates the probability transition matrix from a source vertex to the target vertices, and calls a Python program to
     calculate the hitting times from each non-target vertex to the target nodes. Here, we use simple random walks.  
    */
    def calculateHittingTimesOfSimpleRandomWalks(inputTargets: Array[String], transitionMatrixFileName: String): Unit = 
    {
        val targets: Array[String] = inputTargets.sorted
        val transitionMatrix: SparseMatrix[Double] = new SparseMatrix[Double]
        if (this.vertices.size == 0)
        {
            println("Creating all the vertices in the graph ...")
            val iprint: Boolean = true
            this.getVertices(iprint)
        }
        val targetVertices: Array[Vertex] = targets.map(target => this.allVertices(target))
        val adherents: ArrayBuffer[String] = new ArrayBuffer[String]//This is used to store all the loyal adherents to targets. A vertex is called a loyal adherent to targets if this vertex has only one single neighbor, and this very neighbor is exactly one of the targets we are interested in. 
        for (i <- this.vertices.indices)
        {
            val sourceVertex: Vertex = this.vertices(i)
            if (!Utilities.binarySearch(sourceVertex.id, targets))//If source vertex does not concide with any of the targets. 
            {
                val neighbors: ArrayBuffer[(Vertex, Double)] = this.graphMap.getOrElse(sourceVertex, new ArrayBuffer[(Vertex, Double)]())
                var outweight: Double = 0.0
                var isAdjacentToTarget: Boolean = false//if the source vertex is adjacent to target
                for ((v, weight) <- neighbors)
                {
                    if (Utilities.binarySearch(v.id, targets))
                    {
                        isAdjacentToTarget = true
                    }
                    outweight += weight
                }
                var neighboringTargetCounter: Int = 0
                for ((thatVertex, weight) <- neighbors)
                {
                    if (!Utilities.binarySearch(thatVertex.id, targets))//if the neighbor of the source vertex is not one of the targets
                    {
                        val row: Int = this.vertexIndex(sourceVertex)
                        val col: Int = this.vertexIndex(thatVertex)
                        transitionMatrix.map((row, col)) = weight/outweight
                    }
                    else
                    {
                        neighboringTargetCounter += 1
                        //if (neighbors.size == 1)//found a loyal adherent to target
                        //{
                        //    adherents.append(sourceVertex.id)//This sourceVertex has only one neighbor, and this neighbor is our target. 
                        //}
                    }
                }
                if (neighboringTargetCounter == neighbors.size)//If all the neighbors of a source vertex are targets, then this source vertex is a loyal adherent to the targets. 
                {
                    adherents.append(sourceVertex.id)
                }
            }
        }
        println("Saving probability transition matrix to file " + transitionMatrixFileName)
        transitionMatrix.save(transitionMatrixFileName)
        if (true)
        {
            println("Saving all the loyal adherents to vertices (" + targets.mkString(", ") + ")")
            val writer = new PrintWriter(new File("adherents.txt"))
            writer.write("targets = " + targets.mkString(",") + "\n")
            for (v <- adherents)
            {
                writer.write(v + "\n")
            }
            writer.close()
        }
        println("Calling python program to calculate the expected hitting times ... ")
        ("python calculate_hitting_time.py " + transitionMatrixFileName + " vertices.txt adherents.txt").!
    }

    /*
    This function calculates the hitting times from all other nodes to the target node via frustrated random walks. 
    @param target: The target node. 
    @param transitionMatrixFileName: Then name of the file containing non diagonal transition matrix elements. Since we are using a frustrated random walk, the diagonal elements of probability transition matrix may be non-zero. These non-zero elements are to be calculated in the Python file calculate_hitting_times_with_acceptance_probabilities.py. 
    */
    def calculateHittingTimesOfFrustratedRandomWalks(target: String, transitionMatrixFileName: String = "non_digonal_transition_matrix_elements.txt"): Unit = 
    {
        val transitionMatrix: SparseMatrix[Double] = new SparseMatrix[Double]
        val leakingProbability: HashMap[Int, Double] = new HashMap[Int, Double]
        if (this.vertices.size == 0)
        {
            println("Creating all the vertices in the graph ... ")
            val iprint: Boolean = true
            this.getVertices(iprint)
        }
        val targetVertex: Vertex = this.allVertices(target)
        val adherents: ArrayBuffer[(String, Double)] = new ArrayBuffer[(String, Double)]
        for (i <- this.vertices.indices)
        {
            val sourceVertex: Vertex = this.vertices(i)
            if (sourceVertex.id != target)
            {
                val neighbors: ArrayBuffer[(Vertex, Double)] = this.graphMap.getOrElse(sourceVertex, new ArrayBuffer[(Vertex, Double)])
                var outweight: Double = 0.0
                var isAdjacentToTarget: Boolean = false
                for ((v, weight) <- neighbors)
                {
                    if (v.id == target)
                    {
                        isAdjacentToTarget = true
                    }
                    outweight += weight
                }
                for ((thatVertex, weight) <- neighbors)
                {
                    if (thatVertex.id != target)
                    {
                        val row: Int = this.vertexIndex(sourceVertex)
                        val col: Int = this.vertexIndex(thatVertex)
                        val proposalProbability: Double = weight/outweight
                        val thatTotalWeight: Double = Utilities.getVertexWeight(this.graphMap, thatVertex)
                        assert(thatTotalWeight > 0)
                        val acceptanceProbability: Double = weight/thatTotalWeight   
                        transitionMatrix.map((row, col)) = proposalProbability * acceptanceProbability
                    }
                    else
                    {
                        if (neighbors.size == 1)
                        {
                            adherents.append((sourceVertex.id, weight/Utilities.getVertexWeight(this.graphMap, targetVertex)))
                        }
                        else
                        {
                            val row: Int = this.vertexIndex(sourceVertex)
                            val proposalProbability: Double = weight/outweight
                            val acceptanceProbability: Double = weight/Utilities.getVertexWeight(this.graphMap, targetVertex)
                            leakingProbability(row) = proposalProbability * acceptanceProbability
                        }
                    }
                }
            }
        }

        val keys: Array[Int] = leakingProbability.keys.to[Array].sorted
        val leakingProbabilityFileName: String = "leaking_probabilities.txt"
        val writer = new PrintWriter(new File(leakingProbabilityFileName))
        for (key <- keys)
        {
            writer.write(key.toString + ":" + leakingProbability(key).toString + "\n")
        }
        writer.close()
        
        println("Saving probability transition matrix to file " + transitionMatrixFileName)
        transitionMatrix.save(transitionMatrixFileName)

        println("Saving adherents and their transition probabilities to file ... ")
        val adherentsFileName: String = "adherents_probabilities.txt"
        val adherentsWriter = new PrintWriter(new File(adherentsFileName))
        adherentsWriter.write("target = " + target + "\n")
        for ((v, p) <- adherents)
        {
            adherentsWriter.write(v + "," + p.toString + "\n")
        }
        adherentsWriter.close()

        println("Calling Python program to calculate the expected hitting times ... ")
        ("python calculate_hitting_time_with_acceptance_probabilities.py " + transitionMatrixFileName + " vertices.txt " + adherentsFileName + " " + leakingProbabilityFileName).!
        ("mv hitting_times.txt " + target + ".txt").!
        //println("python calculate_hitting_time_with_acceptance_probabilities.py " + transitionMatrixFileName + " vertices.txt " + adherentsFileName + " " + leakingProbabilityFileName)
    }

    /*
    This function calculates the hitting times from start vertex to target vertex via frustrated random walk using Monte Carlo simulation.  
    @param start: Start vertex. 
    @param target: Target vertex. 
    This function returns the hitting time for a frustrated random walk starting from start and ending at target. If the random walk failed to reach target, then this function returns -1. 
    */
    def randomWalkWithAcceptanceProbability(start: Vertex, target: String): Int = 
    {
        if (this.vertices.size == 0)
        {
            this.getVertices(false)
        }
        var counter: Int = 0
        var current: Vertex = start
        val counterMax: Int = this.vertices.size * 100
        breakable
        {
            while (counter < counterMax)
            {
                val neighbors: ArrayBuffer[(Vertex, Double)] = this.graphMap(current)
                if (neighbors.size == 0)
                {
                    break
                }
                counter += 1
                val outweight: Double = Utilities.getVertexWeight(this.graphMap, current)
                val candidateVertices: HashMap[String, Double] = new HashMap[String, Double]
                var ps: Double = 0.0
                for ((neighbor, weight) <- neighbors)
                {
                    val proposalProbability: Double = weight/outweight
                    val acceptanceProbability: Double = weight/Utilities.getVertexWeight(this.graphMap, neighbor)
                    val p: Double = proposalProbability * acceptanceProbability
                    candidateVertices(neighbor.id) = p
                    ps += p
                }
                candidateVertices(current.id) = 1.0 - ps
                val next: String = Utilities.selectVertexByProbability(candidateVertices)
                if (next == target)
                {
                    return counter
                }
                else
                {
                    current = this.allVertices(next)
                }
            }
        }
        println("Warning: failed to find " + target + " starting from " + start.id)
        return -1
    }
    

    def calculateFrustratedHittingTimesUsingMonteCarlo(start: Vertex, target: String, walkNumber: Int): Double = 
    {
        if (this.vertices.size == 0)
        {
            this.getVertices(false)
        }
        val steps: ArrayBuffer[Int] = new ArrayBuffer[Int]
        assert(walkNumber >= 1)
        val interval: Int = 100
        val total: Int = walkNumber/interval
        for (i <- 0 until walkNumber)
        {
            if (this.pleonastic && (i+1)%interval == 0)
            {
                println("counter = " + (i+1).toString + ", total = " + walkNumber.toString)
            }
            steps.append(this.randomWalkWithAcceptanceProbability(start, target))
        }
        return Utilities.getAverage(steps.map(ele => ele.toDouble))
    }

    /*
    This function calculates the mean and variance of hitting times for a frustrated random walk with a target using Monte Carlo simulations. 
    The final results are saved in outputFileName. 
    @param target: The target of a frustrated random walk. 
    @param outputFileName: The output file name. This file contains the hitting times starting from each non-target vertex and ending at the target vertex. 
    @param numberOfWalks: The number of random walks from each non-target node to the target node. A larger number of random walks gives us a more accurate estimation of the hitting time expectation.  
    */
    def calculateFrustratedHittingTimesUsingMonteCarlo(target: String, outputFileName: String, numberOfWalks: Int): Unit = 
    {
        if (this.vertices.size == 0)
        {
            println("Getting all vertices in the graph ... ")
            this.getVertices(false)
        }
    
        val writer = new PrintWriter(new File(outputFileName))
        writer.write("target = " + target + "\n")
        for (vertex <- this.vertices)
        {
            if (vertex.id != target)
            {
                println("Calculating hitting time from " + vertex.id + " to " + target)
                val hittingTimes: ArrayBuffer[Double] = new ArrayBuffer[Double]
                for (i <- 0 until numberOfWalks)
                {
                    val hittingTime: Int = this.randomWalkWithAcceptanceProbability(vertex, target)
                    hittingTimes.append(hittingTime.toDouble)
                }
                val mean: Double = Utilities.getAverage(hittingTimes)
                val variance: Double = Utilities.getVariance(hittingTimes, mean)
                writer.write(vertex.id + " -> target:" + mean.toString + "," + variance.toString + "\n")
                println(vertex.id + " -> target:" + mean.toString + "," + variance.toString)
            }
        }
        writer.close()
    }

    /*PageRank algorithm is implemented here. 
    initialState and updatedState must be normalized by L1 norm during the power iteartion process. 
    */
    def getStationaryState(useAlpha: Boolean = true): SparseVector[Double] = 
    {
        val dimension: Int = this.vertices.size
        //var initialState: SparseVector[Double] = new SparseVector[Double](Utilities.generateL1NormalizedRandomSparseVector(dimension))
        val useRandomInitialState: Boolean = true
        var initialState: SparseVector[Double] = new SparseVector[Double]
        if (useRandomInitialState)
        {
            for (i <- 0 until dimension)
            {
                initialState.map(i) = Random.nextDouble
            }
        }
        else
        {
            for (i <- 0 until dimension)
            {
                initialState.map(i) = i + 1.1
            }
        }
        val norm: Double = initialState.L1Norm
        for (i <- 0 until dimension)
        {
            initialState.map(i) /= norm
        }
        var stationaryState: SparseVector[Double] = new SparseVector[Double](initialState.map)
        val eps: Double = 1.0e-15
        var error: Double = -1.0
        var counter: Int = 0
        val iterationMax: Int = 1200
        if (useAlpha)
        {
            println("Damping factor = " + this.alpha)
            assert(this.alpha <= 1.0 && this.alpha >= 0)
            val tempMap: HashMap[Int, Double] = new HashMap[Int, Double]
            val constant: Double = (1.0 - this.alpha)/dimension.toDouble
            for (i <- 0 until dimension)
            {
                tempMap(i) = constant
            }
            val googleVector: SparseVector[Double] = new SparseVector[Double](tempMap)
            breakable
            {
                while(true)
                {
                    counter += 1
                    //println("L1 norm of initial state vector = " + initialState.L1Norm)
                    //stationaryState = transferMatrix * initialState * this.alpha + googleVector
                    stationaryState.assignment((transferMatrix * initialState * this.alpha + googleVector).normalizeByL1)
                    //println("L1 norm of updated state vector = " + stationaryState.L1Norm)
                    error = (stationaryState - initialState).L2Norm
                    println("counter = " + counter + ", error = " + error + ", time = " + Calendar.getInstance.getTime)
                    if (error < eps || counter > iterationMax)
                    {
                        break
                    }
                    initialState.assignment(stationaryState)
                }
            }
        }
        else
        {
            breakable
            {
                while(true)
                {
                    counter += 1
                    stationaryState.assignment((transferMatrix * initialState).normalizeByL1)
                    error = (stationaryState - initialState).L2Norm
                    //println("counter = " + counter + ", error = " + error + ", time = " + Calendar.getInstance.getTime)
                    if (error < eps || counter > iterationMax)
                    {
                        break
                    }
                    initialState.assignment(stationaryState)
                }
            }
        }
        println("Counter = " + counter + ", error = " + error)
        var largestEigenvalue: Double = -1.0
        if (useAlpha)//stationaryState.L1Norm = 1
        {
            val constant: Double = (1.0 - this.alpha)/dimension.toDouble
            largestEigenvalue = (stationaryState * (this.transferMatrix * stationaryState) * this.alpha + (1.0 - this.alpha)/dimension.toDouble)/(stationaryState * stationaryState)
        }
        else
        {
            largestEigenvalue = stationaryState * (this.transferMatrix * stationaryState)/(stationaryState * stationaryState)
        }
        println("Max eigenvalue = " + largestEigenvalue)
        val writer = new PrintWriter(new File("max_eigenvalue.txt"))
        writer.write(largestEigenvalue.toString + "\n")
        writer.close()
        return stationaryState.normalizeByL1
    }

    def pageRank(useAlpha: Boolean): Unit = 
    {
        if (this.vertices.size == 0)
        {
            println("Getting vertices and edges ..., time = " + Calendar.getInstance.getTime)
            this.getVertices()
        }
        
        println("Creating transfer matrix ... , time = " + Calendar.getInstance.getTime)
        val transferMatrixFileName: String = "transfer_matrix.txt"
        this.createTransferMatrix(transferMatrixFileName)
        println("Calculating stationary state ..., time = " + Calendar.getInstance.getTime)
        if (useAlpha)
        {
            ("python analyze_transfer_matrix.py " + transferMatrixFileName + " " + this.alpha.toString).!
        }
        else
        {
            ("python analyze_transfer_matrix.py " + transferMatrixFileName + " " + 1.toString).!
        }
        //val stationaryState: SparseVector[Double] = this.getStationaryState(useAlpha)
        val pageRankResultFileName: String = "page_rank_final_state.txt"//This file is generated from analyze_transfer_matrix.py 
        //val stationaryState: SparseVector[Double] = Utilities.readSparseVectorFromFile(pageRankResultFileName) 
        //println("Saving the final stationary state ..., time = " + Calendar.getInstance.getTime)
        //stationaryState.saveToFile("stationary_state.txt")
        println("Processing the final results ..., time = " + Calendar.getInstance.getTime)
        Utilities.saveSortedVertices("vertices.txt", pageRankResultFileName, "final_result.txt")
        println("Done. Time = " + Calendar.getInstance.getTime)
    }
    
    def pageRank: Unit = this.pageRank(true)

    def personalizedPageRank(target: String): Unit = 
    {
        if (this.vertices.size == 0)
        {
            println("Getting vertices and edges ..., time = " + Calendar.getInstance.getTime)
            this.getVertices()
        }
        assert(this.allVertices.contains(target))
        val targetIndex: Int = this.vertexIndex(this.allVertices(target))
        val personalizationVectorFileName: String = "personalization_vector.txt"
        val writer = new PrintWriter(new File(personalizationVectorFileName))
        writer.write(targetIndex.toString + ":" + 1.toString + "\n")
        writer.close()

        println("Creating transfer matrix ... , time = " + Calendar.getInstance.getTime)
        val transferMatrixFileName: String = "transfer_matrix.txt"
        this.createTransferMatrix(transferMatrixFileName)
        println("Calculating stationary state ..., time = " + Calendar.getInstance.getTime)
        ("python analyze_transfer_matrix.py " + transferMatrixFileName + " " + this.alpha.toString + " " + personalizationVectorFileName).!
        val pageRankResultFileName: String = "page_rank_final_state.txt"//This file is generated from analyze_transfer_matrix.py 
        println("Processing the final results ..., time = " + Calendar.getInstance.getTime)
        Utilities.saveSortedVertices("vertices.txt", pageRankResultFileName, "final_result.txt")
        val target_writer = new PrintWriter(new File(target + ".txt"))
        target_writer.write("target = " + target + "\n")
        breakable
        {
            for (line <- Source.fromFile("final_result.txt").getLines)
            {
                if (line.contains("pageRankValue"))
                {
                    break
                }
                val a: Array[String] = line.split(";").last.split(":")
                val name: String = a.head
                val value: Double = a.last.toDouble
                if (name != target)
                {
                    target_writer.write(name + " -> target:" + (1.0/value).toString + "\n")
                }
            }
        }
        target_writer.close()
        println("Done. Time = " + Calendar.getInstance.getTime)
    }

   
    /*
    This method implments a simple random walk on a graph. In the random walk process, a random walker starts from a starting vertex, and explores its nearby nodes recursively and 
    randomly. The random walk process will break after the random walker has found a pre-specified target. We maintain a counter to record how many steps the random walker
    has walked before it finally reaches the target. This method returns the step counter as a final result. 
    @param start: This is the starting vertex for the random walk on graph
    @param target: This is the target node for the random walk on graph. If we have reached target, then the random walk stops. 
    The returned value is the number of steps before the random walker reaches the target. A negative step number indicates the target is not found by the random walker.  
    */
    def randomWalk(start: String, target: String): Int = 
    {
        //assert(this.vertices.contains(start))
        if (start == target)
        {
            return 0
        }
        var counter: Int = 0
        val counterMax: Int = this.vertices.size * 100
        var currentIterator: Vertex = this.allVertices(start)
        breakable
        {
            while(counter < counterMax)
            {
                counter += 1
                val adjacentVertices: Array[(Vertex, Double)] = graphMap.getOrElse(currentIterator, ArrayBuffer[(Vertex, Double)]()).to[Array]
                if (adjacentVertices.size == 0)
                {
                    break
                }
                else
                {
                    val randomIndex: Int = Utilities.selectAtRandomByWeight(adjacentVertices)
                    currentIterator = adjacentVertices(randomIndex)._1
                    if (currentIterator.id == target)
                    {
                        return counter
                    }
                }
            }
        }
        println("Warning: The random walker failed to locate the target. ")
        return -1
    }

    ///simple random walk is used. 
    def randomWalk(start: String, targets: Array[String]): Int = 
    {
        //assert(this.vertices.contains(start))
        if (Utilities.binarySearch(start, targets))
        {
            return 0
        }
        var counter: Int = 0
        val counterMax: Int = this.vertices.size * 100
        var currentIterator: Vertex = this.allVertices(start)
        breakable
        {
            while(counter < counterMax)
            {
                counter += 1
                val adjacentVertices: Array[(Vertex, Double)] = graphMap.getOrElse(currentIterator, ArrayBuffer[(Vertex, Double)]()).to[Array]
                if (adjacentVertices.size == 0)
                {
                    break
                }
                else
                {
                    val randomIndex: Int = Utilities.selectAtRandomByWeight(adjacentVertices)
                    currentIterator = adjacentVertices(randomIndex)._1
                    if (Utilities.binarySearch(currentIterator.id, targets))
                    {
                        return counter
                    }
                }
            }
        }
        println("Warning: The random walker failed to locate any of the targets. ")
        return -1
    }



    /*
    This function calculates the average hitting times for simple random walks starting from each non-target vertex, and ending 
    at a target vertex. 
    @param inputTargets: The target nodes for each random walk. 
    @param simulationTimes: The number of Monte Carlo simulation times. 
    @param outputFileName: This file has four columns structured in this format: starting_vertex -> target_nodes:mean,variance
    It means a random walker starts from start_vertex, and ends at one of the target_nodes. Repeat this random walk many times. Each time, the random walk 
    returns an integer, which is the number of steps in the random walk path. Calculate the mean and variance of these step numbers, and record 
    these values into the output file. 
    */
    def calculateSimpleHittingTimesUsingMonteCarlo(inputTargets: Array[String], simulationTimes: Int, outputFileName: String): Unit = 
    {
        if (this.vertices.size == 0)
        {
            println("Getting vertices and edges ..., time = " + Calendar.getInstance.getTime)
            this.getVertices(false)
        }
        val targets: Array[String] = inputTargets.sorted
        println("Getting hitting times for each non-target vertex toward nodes (" + targets.mkString(",") + "), time = " + Calendar.getInstance.getTime)
        val start: Long = System.currentTimeMillis
        val lines: ArrayBuffer[String] = new ArrayBuffer[String]
        for (i <- this.vertices.indices)
        {
            val v: Vertex = this.vertices(i)
            println("Vertex index = " + (i+1).toString + ", total = " + this.vertices.size.toString + ", time = " + Calendar.getInstance.getTime)
            if (!Utilities.binarySearch(v.id, targets))
            {
                val hittingTimes: ArrayBuffer[Double] = new ArrayBuffer[Double]
                for (i <- 0 until simulationTimes)
                {
                    val hittingTime: Double = this.randomWalk(v.id, targets).toDouble
                    if (hittingTime > 0)
                    {
                        hittingTimes.append(hittingTime)
                    }
                }
                val mean: Double = Utilities.getAverage(hittingTimes)
                val variance: Double = Utilities.getVariance(hittingTimes, mean)
                lines.append(v.id + " -> (" + targets.mkString(",") + "):"  + mean.toString + "," + variance.toString)
            }
        }
        println(lines.mkString("\n"))
        val writer = new PrintWriter(new File(outputFileName))
        val sortedLines = lines.sortBy(line => line.split(":").last.split(",").head.toDouble)
        for (line <- sortedLines)
        {
            writer.write(line + "\n")
        }
        writer.close()

        val sortedWriter = new PrintWriter(new File("simple_random_walks_simulations.txt"))
        for (i <- sortedLines.indices)
        {
            sortedWriter.write(i.toString + "  " + sortedLines(i).split(":").last.split(",").head + "\n")
        }
        sortedWriter.close()

        val end: Long = System.currentTimeMillis
        println("Hitting times recorded. Total time used in seconds = " + (end - start)/1000.toDouble)
    }

    /****
    DeepWalk algorithm is used for node embedding. This algorithm takes random paths as input, and outputs dense vectors that are used to represent nodes in the graph. 
    This function generates a random path starting from one single vertex. The number of walk steps in the random path is passed as a function argument. 
    @param start: The starting vertex for a random walk
    @param walkLength: The number of walk steps 
    The return value is an array buffer that stores the whole path (all the vertices that are traversed) generated during this random walk. 
    */
    def randomWalk(start: Vertex, walkLength: Int): ArrayBuffer[Vertex] = 
    {
        assert(this.vertices.contains(start))
        val path: ArrayBuffer[Vertex] = new ArrayBuffer[Vertex]
        var counter: Int = 0
        var currentIterator: Vertex = start
        path.append(currentIterator)
        breakable
        {
            while(counter < walkLength)
            {
                counter += 1
                val adjacentVertices: Array[(Vertex, Double)] = graphMap.getOrElse(currentIterator, ArrayBuffer[(Vertex, Double)]()).to[Array]
                //val radiantWeights: Double = Utilities.getRadiantWeights(adjacentVertices)
                if (adjacentVertices.size == 0)
                {
                    path.append(currentIterator)
                    break
                }
                else
                {
                    val randomIndex: Int = Utilities.selectAtRandomByWeight(adjacentVertices)
                    currentIterator = adjacentVertices(randomIndex)._1
                    path.append(currentIterator)
                }
            }
        }
        return path
    }


    /*This function generates a bunch of random paths. The starting point of each random path is randomly selected from all the vertices in the graph. 
     The number of random paths is specified in the function argument. All the random paths are stored into a file. 
    @param walkNumber: The number of walks
    @param walkLength: The number of steps in a path. 
    @param pathFileName: The output file that contains all the paths generated. 
    */
    def randomWalk(walkNumber: Int, walkLength: Int, pathFileName: String): Unit = 
    {
        if (this.vertices.size == 0)
        {
            println("Getting vertices and edges ..., time = " + Calendar.getInstance.getTime)
            this.getVertices()
            //this.getEdges()
        }
        val writer = new PrintWriter(new File(pathFileName))
        val sourceVertices: Array[Vertex] = this.graphMap.keys.to[Array]
        val size: Int = sourceVertices.size
        for (i <- 0 until walkNumber)
        {
            if ((i+1)%100 == 0)
            {
                println("i = " + (i+1).toString + ", total = " + walkNumber)
            }
            val randomIndex: Int = Random.nextInt(size)
            val start: Vertex = sourceVertices(randomIndex)
            val path: ArrayBuffer[Vertex] = this.randomWalk(start, walkLength)
            val pathString: String = path.map(vertex => vertex.id).mkString("  ")
            writer.write(pathString + "\n")
        }
        writer.close()
    }


    /*A Depth first traversal of the graph. 
    @param start: The starting point of this DFS traversal. 
    @param componentFileName: The output file that contains all the vertices that can be reached from the starting point.
    @param iprint: Controls whether to print out the subgraph or not
    return value is the number of veritces that can be reached from start. 
    */
    def DFS(start: Vertex, componentFileName: String, iprint: Boolean): Int = 
    {
        assert(!this.isDirected)
        var size: Int = 0
        if (start.color == "black")
        {
            return 0
        }
        
        val connectedComponent: ArrayBuffer[String] = new ArrayBuffer[String]
        val stack: Stack[Vertex] = Stack[Vertex]()
        stack.push(start)
        while(!stack.isEmpty)
        {
            val popped: Vertex = stack.pop
            popped.color = "black"
            size += 1
            //println(popped)
            val neighbors: Array[(Vertex, Double)] = this.graphMap(popped).to[Array]
            //println("neighbors of " + popped.id.toString + ": " + neighbors.mkString(";"))
            for ((neighbor, weight) <- neighbors)
            {
                /**************************************************************************************************************
                if (this.isSimple)//no self loop is allowed. 
                {
                    if (neighbor.color != "black")
                    {
                        if (iprint && popped.id.toString <= neighbor.id.toString)
                        {
                            connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                        }
                    }
                }
                else
                {
                    if (iprint && popped.id.toString <= neighbor.id.toString)
                    {
                        connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                    }
                }
                **************************************************************************************************************/

                if (iprint && popped.id.toString <= neighbor.id.toString)
                {
                    connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                }
                
                if (neighbor.color == "white")
                {
                    stack.push(neighbor)
                    neighbor.color = "grey"
                }
            }
        }
       
        if (iprint)
        {
            val writer = new PrintWriter(new File(componentFileName))
            for (i <- connectedComponent.indices)
            {
                writer.write(connectedComponent(i) + "\n")
            }
            writer.close()
        }
        return size
    }


    /****** reset all the colors of vertices to white. **/
    def resetColorsOfAllVertices: Unit =
    {
        val ids: Array[String] = this.allVertices.keys.to[Array]
        for (id <- ids)
        {
            this.allVertices(id).color = "white"
        }
    }
    
    /*A breadth first traversal of the graph. 
    @param start: The starting point of this BFS traversal. 
    @componentFileName: The output file that contains all the vertices that can be reached from the starting point. 
    @iprint: Controls whether to print out the subgraph or not
    return value is the number of veritces that can be reached from start. 
    */
    def BFS(start: Vertex, componentFileName: String, iprint: Boolean): Int = 
    {
        assert(!this.isDirected)
        var size: Int = 0
        if (start.color == "black")
        {
            return 0
        }
       
        val connectedComponent: ArrayBuffer[String] = new ArrayBuffer[String]
        val queue: Queue[Vertex] = Queue[Vertex]()
        queue.enqueue(start)
        while(!queue.isEmpty)
        {
            val popped: Vertex = queue.dequeue
            popped.color = "black"
            size += 1
            //println(popped)
            val neighbors: Array[(Vertex, Double)] = this.graphMap(popped).to[Array]
            //println("neighbors of " + popped.id.toString + ": " + neighbors.mkString(";"))
            for ((neighbor, weight) <- neighbors)
            {
                /**************************************************************************************************************
                if (this.isSimple)//no self loop is allowed. 
                {
                    if (neighbor.color != "black")
                    {
                        if (iprint && popped.id.toString <= neighbor.id.toString)
                        {
                            connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                        }
                    }
                }
                else
                {
                    if (iprint && popped.id.toString <= neighbor.id.toString)
                    {
                        connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                    }
                }
                **************************************************************************************************************/

                if (iprint && popped.id.toString <= neighbor.id.toString)
                {
                    connectedComponent.append(popped.id.toString + ";" + neighbor.id.toString + ";" + weight.toString)
                }

                if (neighbor.color == "white")
                {
                    queue.enqueue(neighbor)
                    neighbor.color = "grey"
                }
            }
        }

        if (iprint)
        {
            val writer = new PrintWriter(new File(componentFileName))
            for (i <- connectedComponent.indices)
            {
                writer.write(connectedComponent(i) + "\n")
            }
            writer.close()
        }

        return size
    }


    /*This function counts the number of disconnected components in the graph. */
    def getDisconnectedComponentNumber(iprint: Boolean): Int = 
    {
        assert(!this.isDirected)
        if (this.vertices.size == 0)
        {
            println("Getting vertices and edges ..., time = " + Calendar.getInstance.getTime)
            this.getVertices()
            //this.getEdges()
        }
        var counter: Int = 0
        for (vertex <- this.vertices)
        {
            if (vertex.color == "white")
            {
                //println("******************  Connected components  ************************")
                counter += 1
                val componentFileName: String = "connected_component_" + counter.toString + ".txt"
                this.DFS(vertex, componentFileName, iprint)
                //println("******************************************************************")
            }
        }
        println("Number of disconnected components = " + counter)
        return counter
    }
    
    def getDisconnectedComponentNumber: Int = getDisconnectedComponentNumber(false)

    /*
    This function prints out the leading subgraphs. Subgraphs are sorted by size
    */
    def getLeadingSubgraphs(numberOfLeadingSubgraphs: Int): Unit = 
    {
        assert(!this.isDirected)
        if (this.vertices.size == 0)
        {
            println("Getting vertices ... , time = " + Calendar.getInstance.getTime)
            this.getVertices()
        }
        else
        {
            for (vertex <- this.vertices)
            {
                vertex.color = "white"
            }
        }
        val subgraphSizes: HashMap[Vertex, Int] = new HashMap[Vertex, Int]
        
        for (vertex <- this.vertices)
        {
            if (vertex.color == "white")
            {
                val subgraphSize: Int = this.DFS(vertex, "any", false)
                subgraphSizes(vertex) = subgraphSize
            }
        }
        println("Total number of disconnected subgraphs = " + subgraphSizes.size + ", time = " + Calendar.getInstance.getTime)
        val writer = new PrintWriter(new File("subgraph_sizes.txt"))
        val sizesSorted: Array[(Vertex, Int)] = subgraphSizes.to[Array].sortBy(ele => ele._2).reverse//Sort subgraphs by sizes, from large to small. 
        for ((v, size) <- sizesSorted)
        {
            writer.write(v.id + " -> " + size.toString + "\n")
        }
        writer.close()
        println("Resetting all the vertices ... , Time = " + Calendar.getInstance.getTime)
        for (vertex <- this.vertices)
        {
            vertex.color = "white"
        }
        println("Saving leading subgraphs ..., time = " + Calendar.getInstance.getTime)
        breakable
        {
            for (i <- sizesSorted.indices)
            {
                if (i >= numberOfLeadingSubgraphs)
                {
                    break
                }
                val start: Vertex = sizesSorted(i)._1
                val size: Int = sizesSorted(i)._2
                if (size == 1)
                {
                    break
                }
                val subgraphFileName: String = "subgraph_index_" + i.toString + "_size_" + size.toString + ".csv"
                this.DFS(start, subgraphFileName, true)//iprint = true
                println("Subgraph index = " + i + ", total = " + min(sizesSorted.size, numberOfLeadingSubgraphs) + ", time = " + Calendar.getInstance.getTime)
            }
        }
    }
}
