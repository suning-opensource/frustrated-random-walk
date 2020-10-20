package frustrated.random.walk

object test
{
    def main(args: Array[String]): Unit = 
    {
        val graph = new Graph
        graph.fastReadFile("../data/harry_potter.csv", ";")
        val target: String = "Harry_Potter"
        //graph.calculateHittingTimesOfSimpleRandomWalks(target)
        graph.calculateHittingTimesOfFrustratedRandomWalks(target)
    }
}
