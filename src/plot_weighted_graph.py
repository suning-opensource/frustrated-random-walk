#!/usr/bin/env python
"""
==============
Weighted Graph
==============

An example using Graph as a weighted network.
"""
# Author: Aric Hagberg (hagberg@lanl.gov)
import matplotlib.pyplot as plt
import networkx as nx

def readEdgeFile(edgeFileName, pageRankFileName):
    import os
    assert(os.path.exists(edgeFileName))
    page_ranked = False
    if (os.path.exists(pageRankFileName)):
        page_ranked = True
        rank_result = dict()
        ifile = open(pageRankFileName, "r")
        for (index, string) in enumerate(ifile):
            if ("value" in string):
                break
            rank_result[string.split(";")[1].split(":")[0]] = str(index + 1)
        ifile.close()

    G = nx.Graph()
    ifile = open(edgeFileName, "r")
    if (not page_ranked):
        for (index, string) in enumerate(ifile):
            a = string.strip("\n").split(";")
            src = a[0]
            dst = a[1]
            if (len(a) > 2):
                edge_weight = float(a[2])
            else:
                edge_weight = 1.0
            G.add_edge(src, dst, weight = edge_weight)
    else:
        #print rank_result
        writer = open(edgeFileName.replace(".csv", "_ranked.csv"), "w")
        for (index, string) in enumerate(ifile):
            a = string.strip("\n").split(";")
            src = rank_result[a[0]]
            dst = rank_result[a[1]]
            if (len(a) > 2):
                edge_weight = float(a[2])
            else:
                edge_weight = 1.0
            writer.write(src + ";" + dst + ";" + str(edge_weight) + "\n")
            G.add_edge(src, dst, weight = edge_weight)
        writer.close()
    ifile.close()
    return G

def main():
    import sys
    if (len(sys.argv) < 2):
        print "edgeFileName = sys.argv[1], pageRankFileName = sys.argv[2](optional). "
        return -1

    if (len(sys.argv) >= 2):
        edgeFileName = sys.argv[1]
        pageRankFileName = "non_existent_file_name"
    if (len(sys.argv) == 3):
        pageRankFileName = sys.argv[2]

    assert(".csv" in edgeFileName)
    G = readEdgeFile(edgeFileName, pageRankFileName)
    pos = nx.spring_layout(G)  # positions for all nodes
    nx.draw_networkx_nodes(G, pos, node_size = 10)
    # edges
    edges = G.edges()
    nx.draw_networkx(G, pos, edgelist = edges, width = 1, font_size = 15, font_color = "blue", with_labels = True, node_size = 100, node_color = "red")
    # labels
    #nx.draw_networkx_labels(G, pos, font_size = 25, font_family='sans-serif', font_color = "b")
    plt.axis('off')
    plt.show()

if __name__ == "__main__":
    import sys
    sys.exit(main())
