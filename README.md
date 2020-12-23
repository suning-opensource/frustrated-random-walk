# Frustrated random walk

A graph algorithm for evaluating node distances. 

This repository is an implementation of the algorithm presented here: https://journals.aps.org/pre/abstract/10.1103/PhysRevE.102.052135

See this link for a preview: https://arxiv.org/abs/1908.09644

For a Chinese translation, see this link: https://zhuanlan.zhihu.com/p/334361052


Code written and tested by [Enzhi Li](https://github.com/PrimerLi). 
All the test data sets that we used can be found via these links:  
1. [Dream of the Red Chamber (红楼梦) data set](https://github.com/PrimerLi/red_chamber_dream_network) 
2. [arXiv data set and Harry Potter data set](https://github.com/PrimerLi/graph-data)
3. [IMDB movie star data set](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset). 

## What should you install to run the program?
This implementation contains both Scala and Python codes. To run Scala, you need to install sbt 1.0.4 and Scala 2.11.8 or 2.12.3. To run Python, you need to install Python 2.7. You also need numpy, scipy and networkx to run the Python programs.

## How to run the program?
First, use `src` as the working directory. You can run the program either using ```sbt run``` or using Scala REPL. To use the first method, you need to create your own `main.scala`. An example of `main.scala` is already present in the folder `src`. To use the second method, first create a package using

```
>> sbt package
```

Then run the following line to enter Scala REPL environment: 

```
>> scala -classpath ./target/scala-2.11/frustrated-random-walk_2.11-1.0.jar
```

In the Scala REPL, run these lines:
```
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
@article{PhysRevE.102.052135,
  title = {Frustrated random walks: A faster algorithm to evaluate node distances on connected and undirected graphs},
  author = {Li, Enzhi and Le, Zhengyi},
  journal = {Phys. Rev. E},
  volume = {102},
  issue = {5},
  pages = {052135},
  numpages = {13},
  year = {2020},
  month = {Nov},
  publisher = {American Physical Society},
  doi = {10.1103/PhysRevE.102.052135},
  url = {https://link.aps.org/doi/10.1103/PhysRevE.102.052135}
}
```

## Sponsor and supporting team
The author Enzhi Li finished this project at at Big Data Lab of [Suning R&D Center, Palo Alto](http://www.ussuning.com/). Thanks to the help of our team members.

## Contact us

If you have quesetions with this project, please contact Enzhi Li via enzhililsu@gmail.com. 


© 2020 Big Data lab, Suning, USA.
