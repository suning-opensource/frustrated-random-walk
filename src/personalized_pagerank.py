import networkx as nx
import os
from matplotlib import pyplot as plt

def create_graph(edge_file_name, vertex_file_name):
    assert(os.path.exists(edge_file_name))
    graph = nx.Graph()
    print "Reading the graph nodes ... "
    if os.path.exists(vertex_file_name):
        reader = open(vertex_file_name, "r")
        vertices = map(lambda line: line.strip("\n"), reader.readlines())
        reader.close()
    else:
        reader = open(edge_file_name, "r")
        vertices = []
        for (index, string) in enumerate(reader):
            a = string.strip("\n").split(";")
            assert(len(a) >= 2)
            vertices.append(a[0])
            vertices.append(a[1])
        reader.close()
        vertices = list(set(vertices))
    graph.add_nodes_from(vertices)

    print "Adding edges to the graph ... "
    reader = open(edge_file_name, "r")
    for (index, string) in enumerate(reader):
        a = string.strip("\n").split(";")
        assert(len(a) >= 2)
        left = a[0]
        right = a[1]
        weight = 1
        if (len(a) == 3):
            weight = float(a[2])
        graph.add_edge(left, right, weight = weight)
        graph.add_edge(right, left, weight = weight)
    reader.close()

    nx.draw_networkx(graph, pos = nx.spring_layout(graph), with_labels = True, width = 1)
    plt.show()
    return graph

def personalized_pagerank(graph, target):
    nodes = map(lambda node: node.decode("utf-8"), list(graph.nodes))
    
    assert(target in nodes)
    personalization = dict()
    for node in nodes:
        if node == target:
            personalization[node] = 1
        else:
            personalization[node] = 0
    print_dictionary(personalization)
    #ppr = nx.pagerank(graph, alpha = 0.85)
    ppr = nx.pagerank(graph, alpha = 0.85, personalization = personalization)
    return ppr

def print_dictionary(D):
    pairs = sorted(D.iteritems(), key = lambda (k, v): v, reverse = True)
    for (k, v) in pairs:
        print k, v

def main():
    import sys
    if len(sys.argv) != 3:
        print "edge_file_name = sys.argv[1], target = sys.argv[2]"
        return -1

    edge_file_name = sys.argv[1]
    target = sys.argv[2].decode("utf-8")

    print "Creating graph ... "
    graph = create_graph(edge_file_name, "vertices.txt")
    return 0

    print "Calculating PageRank results ... "
    ppr = personalized_pagerank(graph, target)
    print "Personalized PageRank results for target = " + target
    print_dictionary(ppr)
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
