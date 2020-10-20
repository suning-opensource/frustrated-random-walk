import os
import numpy as np
from scipy import *
from scipy.sparse import *
from scipy.sparse.linalg import inv
import time
import datetime

def normalize_similarity(similarity_result):
    pairs = sorted(similarity_result.iteritems(), key = lambda (k, v): v, reverse = True)
    result = dict()
    max_similarity = pairs[0][1]
    min_similarity = pairs[-1][1]
    for i in range(len(pairs)):
        k, v = pairs[i]
        if i == 0:
            normalization = v - min_similarity
            result[k] = 1.0
        else:
            result[k] = (v - min_similarity)/normalization
    return result

#read exact solution for average hitting times. 
def read_exact_solution(input_file_name):
    assert(os.path.exists(input_file_name))
    result = dict()
    reader = open(input_file_name, "r")
    min_distance = 0
    for (index, string) in enumerate(reader):
        if (index > 0):
            a = string.strip("\n").split(" -> ")
            node = a[0].decode("utf-8")
            distance = float(a[1].split(":")[1].split(",")[0])
            if (index == 1):
                min_distance = distance
            similarity = min_distance/distance
            result[node] = similarity
    reader.close()
    return normalize_similarity(result)

def print_dictionary(D):
    pairs = sorted(D.iteritems(), key = lambda (k, v): v, reverse = True)
    for (k, v) in pairs:
        print k, v

def save_dictionary(D, output_file_name):
    pairs = sorted(D.iteritems(), key = lambda (k, v): v, reverse = True)
    writer = open(output_file_name, "w")
    for (k, v) in pairs:
        writer.write(k.encode("utf-8") + "," + str(v) + "\n")
    writer.close()

def compare_dictionary_results(D1, D2):
    keys1 = set(D1.keys())
    keys2 = set(D2.keys())
    keys = list(keys1.intersection(keys2))
    assert(len(keys) > 0)
    result = 0
    for key in keys:
        result += abs(D1[key] - D2[key])
    return result/float(len(keys))

def compare_exact_solutions(hitting_times_file_1, hitting_times_file_2):
    assert(os.path.exists(hitting_times_file_1) and os.path.exists(hitting_times_file_2))
    results1 = read_exact_solution(hitting_times_file_1)
    results2 = read_exact_solution(hitting_times_file_2)
    return compare_dictionary_results(results1, results2)

def read_transition_matrix(transition_matrix_file_name, leaking_probability_file_name, target_indices, adherent_indices):
    assert(os.path.exists(transition_matrix_file_name))
    assert(os.path.exists(leaking_probability_file_name))
    row_indices = []
    col_indices = []
    data = []
    reader = open(transition_matrix_file_name)
    for (index, string) in enumerate(reader):
        a = string.strip("\n").split(",")
        row = int(a[0])
        col = int(a[1])
        val = float(a[2])
        row_indices.append(row)
        col_indices.append(col)
        data.append(val)
    reader.close()
    dimension = max(max(row_indices), max(col_indices)) + 1
    non_diagonal_elements = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, dimension))
    
    row_indices = range(dimension)
    col_indices = [0] * dimension
    data = [1] * dimension
    ones = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))

    row_sums = np.asarray((non_diagonal_elements * ones).todense())[:, 0]

    leaking_probabilities = dict()
    reader = open(leaking_probability_file_name, "r")
    for (index, string) in enumerate(reader):
        a = string.strip("\n").split(":")
        leaking_probabilities[int(a[0])] = float(a[1])
    reader.close()

    row_indices = [] 
    col_indices = []
    data = []
    for i in range(dimension):
        if (i in target_indices or i in adherent_indices):
            continue
        data.append(1.0 - row_sums[i] - leaking_probabilities.get(i, 0.0))
        row_indices.append(i)
        col_indices.append(i)
    diagonal_elements = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, dimension))

    transition_matrix = non_diagonal_elements + diagonal_elements
    
    if False:
        matrix = np.asarray(transition_matrix.todense())
        print "Transition matrix : "
        print matrix
        print "Eigenvalues of transition matrix: "
        print np.linalg.eigvals(matrix)
    return transition_matrix

def estimate_spectral_radius(transition_matrix):
    row, col = transition_matrix.shape
    assert(row == col)
    dimension = row
    estimated_spectral_radius = sum(transition_matrix)/float(dimension)
    print "Estimated spectral radius = " + str(estimated_spectral_radius)
    return estimated_spectral_radius 
    matrix = np.asarray(transition_matrix.todense())
    eigenvalues = np.linalg.eigvals(matrix)
    spectral_radius = max(map(lambda ele: abs(ele), eigenvalues))
    print "Spectral radius = " + str(spectral_radius)
    print "Difference between estimated and real spectral radius = " + str(abs(spectral_radius - estimated_spectral_radius))

def get_mean(transition_matrix, vertices, targets, adherents_probabilities):
    start = time.time()
    row, col = transition_matrix.shape
    assert(row == col)
    dimension = row
    print "Dimension of transition matrix = " + str(dimension)
    row_indices = range(dimension)
    col_indices = [0] * dimension
    data = [1] * dimension
    ones = csr_matrix((data, (row_indices, col_indices)), shape = (dimension, 1))
    eps = 1.0e-16
    lower_bound = 1.0e-6
    expectation = ones
    power = ones
    estimated_spectral_radius = estimate_spectral_radius(transition_matrix)
    expansion_order = 100000 #max(int(1.0/(1.0 - estimated_spectral_radius)), 1000000)
    counter = 0
    differences = []
    interval = 500
    while counter < expansion_order:
        power = transition_matrix * power
        expectation += power
        #error = np.linalg.norm(counter * power.todense())
        if counter % interval == 0:
            error = np.linalg.norm(power.todense())
            relative_error = error/sum(expectation.todense())
            print "Counter = " + str(counter) + ", relative error = " + str(relative_error) + ", time = " + str(datetime.datetime.now())
            #print "Counter = " + str(counter) + ", error = " + str(error) + ", time = " + str(datetime.datetime.now())
            output_file_name = "hitting_times_" + str(counter/interval) + ".txt"
            save_sorted_distances = False
            save_mean_hitting_times(np.asarray(expectation.todense())[:, 0], vertices, targets, adherents_probabilities, output_file_name, save_sorted_distances)
            if counter/interval == 0:
                os.system("mv " + output_file_name + " hitting_times_old.txt")
            else:
                diff = compare_exact_solutions("hitting_times_old.txt", output_file_name)
                print "Difference of two successive results = " + str(diff)
                differences.append((counter, diff))
                os.system("mv " + output_file_name + " hitting_times_old.txt")
                if diff < lower_bound:
                    os.system("rm hitting_times_old.txt")
                    break
        if relative_error < eps:
            break
        counter += 1
    print "Counter = " + str(counter) + ", relative error = " + str(relative_error)
    #print "Counter = " + str(counter) + ", error = " + str(error)
    end = time.time()
    print "Total time used in get_mean = " + str(end - start) + " seconds. "
    print "Saving differences of successive exact solutions... "
    writer = open("counter_diff.txt", "w")
    for (counter, diff) in differences:
        writer.write(str(counter) + "  " + str(diff) + "\n")
    writer.close()
    
    if False:
        writer = open("counter_log_diff.txt", "w")
        for (counter, diff) in differences:
            writer.write(str(counter) + "  " + str(np.log(diff)) + "\n")
        writer.close()
    os.system("rm hitting_times*.txt")
    return expectation

def read_vertices(vertices_file_name):
    assert(os.path.exists(vertices_file_name))
    vertices = []
    reader = open(vertices_file_name, "r")
    for (index, string) in enumerate(reader):
        vertices.append(string.strip("\n"))
    reader.close()
    return vertices

def read_adherents_probabilities(adherents_file_name):
    assert(os.path.exists(adherents_file_name))
    reader = open(adherents_file_name, "r")
    adherents_probabilities = []
    for (index, string) in enumerate(reader):
        if index == 0:
            targets = string.strip("\n").split(" = ")[1].split(",")
        else:
            a = string.strip("\n").split(",")
            vertex_id = a[0]
            transition_probability = float(a[1])
            adherents_probabilities.append((vertex_id, transition_probability))
    reader.close()
    targets_sorted = sorted(targets)
    adherents_sorted = sorted(adherents_probabilities, key = lambda (v, p): v)
    return targets_sorted, adherents_sorted

def binary_search(target, array):
    length = len(array)
    if length == 0:
        return -1
    if length == 1:
        if target == array[0]:
            return 0
        else:
            return -1
    start = 0
    end = length - 1
    middle = (start + end)/2
    while start <= end:
        if target == array[middle]:
            return middle
        elif target > array[middle]:
            start = middle + 1
        else:
            end = middle - 1
        middle = (start + end)/2
    return -1

def find_index(target, array):
    if len(array) == 0:
        return -1
    if len(array) == 1:
        if target == array[0]:
            return 0
        else:
            return -1
    start = 0
    end = len(array) - 1
    mid = (start + end)/2
    while start <= end:
        if target == array[mid]:
            return mid
        elif target > array[mid]:
            start = mid + 1
        else:
            end = mid - 1
        mid = (start + end)/2
    return -1

def read_info_from_original_files(transition_matrix_file_name, vertices_file_name, adherents_file_name, leaking_probability_file_name):
    import os

    assert(os.path.exists(transition_matrix_file_name))
    assert(os.path.exists(vertices_file_name))
    assert(os.path.exists(adherents_file_name))
    assert(os.path.exists(leaking_probability_file_name))

    vertices = sorted(read_vertices(vertices_file_name))
    
    targets, adherents_probabilities = read_adherents_probabilities(adherents_file_name)
    adherents = map(lambda (v, p): v, adherents_probabilities)
    target_indices = []
    adherent_indices = []
    
    for target in targets:
        index = find_index(target, vertices)
        assert(index >= 0)
        target_indices.append(index)
    for adherent in adherents:
        index = find_index(adherent, vertices)
        assert(index >= 0)
        adherent_indices.append(index)

    transition_matrix = read_transition_matrix(transition_matrix_file_name, leaking_probability_file_name, target_indices, adherent_indices)
    return transition_matrix, vertices, targets, adherents_probabilities

def save_mean_hitting_times(expectation, vertices, targets, adherents_probabilities, output_file_name, save_sorted_distances):
    print "Saving expectation results ... "
    adherents = map(lambda (v, p): v, adherents_probabilities)
    lines = []
    distances = dict()
    for i in range(len(vertices)):
        if binary_search(vertices[i], targets) >= 0:
            continue
        index = binary_search(vertices[i], adherents)
        if index >= 0:
            p = adherents_probabilities[index][1]
            distance = 1.0/p
            lines.append(vertices[i] + " -> targets:" + str(distance) + "," + str((1.0 - p)/p**2))
            distances[vertices[i]] = distance
        else:
            lines.append(vertices[i] + " -> targets:" + str(expectation[i]))
            distances[vertices[i]] = expectation[i]
    
    sorted_lines = sorted(lines, key = lambda line: float(line.split(":")[-1].split(",")[0]))
    sorted_distances = []
    writer = open(output_file_name, "w")
    writer.write("Targets = " + ",".join(targets) + "\n")
    for line in sorted_lines:
        writer.write(line + "\n")
        distance = float(line.split(":")[-1].split(",")[0])
        if distance == 1:
            continue
        sorted_distances.append(distance)
    writer.close()
    
    if save_sorted_distances:
        writer = open("sorted_distances.txt", "w")
        slopes = []
        for i in range(len(sorted_distances)):
            writer.write(str(i) + "  " + str(sorted_distances[i]) + "\n")
            if i < len(sorted_distances) - 1:
                slopes.append(sorted_distances[i+1] - sorted_distances[i])
        writer.close()

def calculate_and_save_mean(transition_matrix, vertices, targets, adherents_probabilities):
    expectation = get_mean(transition_matrix, vertices, targets, adherents_probabilities)
    expectation = np.asarray(expectation.todense())[:, 0]

    output_file_name = "hitting_times.txt"
    save_sorted_distances = False
    save_mean_hitting_times(expectation, vertices, targets, adherents_probabilities, output_file_name, save_sorted_distances)
    return 

def main():
    import sys
    if (len(sys.argv) != 5):
        print "transition_matrix_file_name = sys.argv[1], vertices_file_name = sys.argv[2], adherents_file_name = sys.argv[3], leaking_probability_file_name = sys.argv[4]. "
        return -1

    transition_matrix_file_name = sys.argv[1]
    vertices_file_name = sys.argv[2]
    adherents_file_name = sys.argv[3]
    leaking_probability_file_name = sys.argv[4]
    transition_matrix, vertices, targets, adherents_probabilities = read_info_from_original_files(transition_matrix_file_name, vertices_file_name, adherents_file_name, leaking_probability_file_name)
    calculate_and_save_mean(transition_matrix, vertices, targets, adherents_probabilities)
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
