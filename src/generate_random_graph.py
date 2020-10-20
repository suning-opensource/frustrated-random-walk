import os
import numpy as np

def generate_random_graph(n, p, random_graph_file_name):
    assert(n > 1)
    assert(p > 0 and p <= 1)
    edges = []
    for i in range(1, n+1):
        for j in range(1, n+1):
            random = np.random.uniform(0, 1)
            if (random < p):
                edge = str(i) + ";" + str(j)
                edges.append(edge)
    
    edges = list(set(edges))
    writer = open(random_graph_file_name, "w")
    for edge in edges:
        writer.write(edge + "\n")
    writer.close()

def generate_undirected_random_graph(n, p, random_graph_file_name):
    assert(n > 1)
    assert(p > 0 and p <= 1)
    edges = []
    for i in range(1, n + 1):
        for j in range(i + 1, n + 1):
            random = np.random.uniform(0, 1)
            if (random < p):
                edge = str(i) + ";" + str(j)
                edges.append(edge)
   
    edges = list(set(edges))
    writer = open(random_graph_file_name, "w")
    for edge in edges:
        writer.write(edge + "\n")
    writer.close()

def generate_random_communities(community_vertex_number, community_number, pin, pout, community_file_name):
    assert(pin >= 0 and pin <= 1)
    assert(pout >= 0 and pout <= 1)
    all_vertices = []
    counter = 0
    for i in range(community_number):
        vertices = []
        for j in range(community_vertex_number):
            counter += 1
            vertices.append(counter)
        all_vertices.append(vertices)
    
    edges = []
    for i in range(len(all_vertices)):
        vertices = all_vertices[i]
        for j in range(len(vertices)):
            for k in range(len(vertices)):
                if j != k:
                    r = np.random.uniform()
                    if r < pin:
                        edges.append(str(vertices[j]) + ";" + str(vertices[k]))
    for i in range(len(all_vertices)):
        for j in range(i+1, len(all_vertices)):
            left_vertices = all_vertices[i]
            right_vertices = all_vertices[j]
            for k in range(len(left_vertices)):
                for l in range(len(right_vertices)):
                    r = np.random.uniform()
                    if (r < pout):
                        edges.append(str(left_vertices[k]) + ";" + str(right_vertices[l]))
    
    edges = list(set(edges))
    writer = open(community_file_name, "w")
    for edge in edges:
        writer.write(edge + "\n")
    writer.close()

def generate_undirected_random_communities(community_vertex_number, community_number, pin, pout, community_file_name):
    assert(pin >= 0 and pin <= 1)
    assert(pout >= 0 and pout <= 1)
    all_vertices = []
    counter = 0
    for i in range(community_number):
        vertices = []
        for j in range(community_vertex_number):
            counter += 1
            vertices.append(counter)
        all_vertices.append(vertices)
    
    edges = []
    for i in range(len(all_vertices)):
        vertices = all_vertices[i]
        for j in range(len(vertices)):
            for k in range(j + 1, len(vertices)):
                if j != k:
                    r = np.random.uniform()
                    if r < pin:
                        edge = str(vertices[j]) + ";" + str(vertices[k])
                        edges.append(edge)
    for i in range(len(all_vertices)):
        for j in range(i+1, len(all_vertices)):
            left_vertices = all_vertices[i]
            right_vertices = all_vertices[j]
            for k in range(len(left_vertices)):
                for l in range(k + 1, len(right_vertices)):
                    r = np.random.uniform()
                    if (r < pout):
                        edge = str(left_vertices[k]) + ";" + str(right_vertices[l])
                        edges.append(edge)
    
    edges = list(set(edges))
    writer = open(community_file_name, "w")
    for edge in edges:
        writer.write(edge + "\n")
    writer.close()

def main_community():
    import sys
    if (len(sys.argv) != 5):
        print "community_vertex_number = sys.argv[1], community_number = sys.argv[2], pin = sys.argv[3], pout = sys.argv[4]. "
        return -1

    community_vertex_number = int(sys.argv[1])
    community_number = int(sys.argv[2])
    pin = float(sys.argv[3])
    pout = float(sys.argv[4])

    generate_undirected_random_communities(community_vertex_number, community_number, pin, pout, "random_communities.csv")
    return 0

def main_graph():
    import sys
    if (len(sys.argv) != 3):
        print "n = sys.argv[1], p = sys.argv[2]. "
        return -1

    n = int(sys.argv[1])
    p = float(sys.argv[2])
    generate_undirected_random_graph(n, p, "random_graph.csv")
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main_community())
