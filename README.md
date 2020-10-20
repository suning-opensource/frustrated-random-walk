# frustrated-random-walk
 A graph algorithm for evaluating node distances.

This repository is an implementation of the algorithm presented here: https://arxiv.org/abs/1908.09644

## What should you install to run the program?
This implementation contains both Scala and Python codes. To run Scala, you need to install sbt 1.0.4 and Scala 2.11.8 or 2.12.3. To run Python, you need to install Python 2.7. You also need numpy, scipy and networkx to run the Python programs.

## How to run the program?
You can run the program either using ```sbt run``` or using Scala REPL. To use the first method, you need to create your own `main.scala`. An example of `main.scala` is already present in the folder `src`. To use the second method, first create a package using

```
>> sbt package
```

Then run

```
>> Scala
>> import frustrated.random.walk._
>> val graph = new Graph
>> val separator: String = ";"
>> graph.fastReadFile("../data/harry_potter.csv", seperator)
>> val target: String = "Harry_Potter"
>> graph.calculateHittingTimesOfFrustratedRandomWalks(target)
```

After running these lines, you will see your results in file Harry_Potter.txt. The first line of this file is the target, and the following lines list all the other nodes and their distances to the target, and these nodes are ranked according to their distances with respect to the target, from near to far.

## Copyright declarations
This algorithm together with its program implementation is created by Enzhi Li at Suning R&D Center, Palo Alto, USA, under the supervision of Zhengyi Le. If you find this algorithm and its program implementation useful, please cite us:

```
@article{li2019frustrated,
  title={Frustrated Random Walks: A Faster Algorithm to Evaluate Node Distances on Connected and Undirected Graphs},
  author={Li, Enzhi and Le, Zhengyi},
  journal={arXiv preprint arXiv:1908.09644},
  year={2019}
}
```

## Sponsor and Supporting Team
The author Enzhi Li finished this project at at Big Data Lab of [Suning R&D Center, Palo Alto](http://www.ussuning.com/). Thanks to the help of our team members.


© 2020 Big Data lab, Suning, USA.
