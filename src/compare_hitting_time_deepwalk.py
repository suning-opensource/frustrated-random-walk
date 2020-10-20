import os

def normalize_similarity(deepwalk_similarity):
    pairs = sorted(deepwalk_similarity.iteritems(), key = lambda (k, v): v, reverse = True)
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

def read_deepwalk(input_file_name):
    assert(os.path.exists(input_file_name))
    result = dict()
    reader = open(input_file_name, "r")
    for (index, string) in enumerate(reader):
        if ("target" in string):
            target = string.strip("\n").split(" = ")[-1].decode("utf-8")
            result[target] = dict()
        else:
            a = string.strip("\n").split(",")
            node = a[0].decode("utf-8")
            similarity = float(a[1])
            result[target][node] = similarity
    reader.close()

    targets = list(result.keys())
    for target in targets:
        result[target] = normalize_similarity(result[target])
    return result

def read_deepwalk_with_target(input_file_name, input_target):
    assert(os.path.exists(input_file_name))
    result = dict()
    reader = open(input_file_name, "r")
    for (index, string) in enumerate(reader):
        if ("target" in string):
            target = string.strip("\n").split(" = ")[-1].decode("utf-8")
            if (target == input_target):
                result[target] = dict()
        else:
            if (target == input_target):
                a = string.strip("\n").split(",")
                node = a[0].decode("utf-8")
                similarity = float(a[1])
                result[target][node] = similarity
            else:
                continue
    reader.close()

    targets = list(result.keys())
    for target in targets:
        result[target] = normalize_similarity(result[target])
    return result

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

def get_node_ranks(d):
    pairs = sorted(d.iteritems(), key = lambda (k, v): v, reverse = True)
    indices = []
    for i in range(len(pairs)):
        indices.append((pairs[i][0], i+1))
    return indices

def binary_search(node, node_ranks):
    if (len(node_ranks) == 0):
        return -1
    start = 0
    end = len(node_ranks) - 1
    middle = (start + end)/2
    while start <= end:
        if node == node_ranks[middle][0]:
            return node_ranks[middle][1]
        elif node > node_ranks[middle][0]:
            start = middle + 1
        else:
            end = middle - 1
        middle = (start + end)/2
    return -1

def save_and_compare_ranks(d1, d2, output_file_name):
    node_ranks1 = get_node_ranks(d1)
    node_ranks2 = get_node_ranks(d2)
    node_ranks2 = sorted(node_ranks2, key = lambda(node, rank): node)
    writer = open(output_file_name, "w")
    writer.write("node,deepwalk,exact_solution\n")
    for (node, rank) in node_ranks1:
        writer.write(node.encode("utf-8") + "," + str(rank) + "," + str(binary_search(node, node_ranks2)) + "\n")
    writer.close()

def compare_hitting_time_deepwalk(deepwalk_results, path_to_exact_solutions):
    keys = deepwalk_results.keys()
    differences = []
    for key in keys:
        exact_solution_file_name = path_to_exact_solutions + "/" + key + ".txt"
        if (os.path.exists(exact_solution_file_name)):
            print "Processing " + exact_solution_file_name
            exact_solution = read_exact_solution(exact_solution_file_name)
            difference = compare_dictionary_results(exact_solution, deepwalk_results[key])
            differences.append((key, difference))
            save_and_compare_ranks(deepwalk_results[key], exact_solution, "node_rank_comparison_" + key + ".txt")
            save_dictionary(exact_solution, "exact_solution_" + key + ".txt")
            save_dictionary(deepwalk_results[key], "deepwalk_" + key + ".txt")
        else:
            #print exact_solution_file_name + " does not exist. "
            continue
    result = sorted(differences, key = lambda (name, diff): diff, reverse = True)
    writer = open("differences.txt", "w")
    writer.write("\n".join(map(lambda (name, diff): name.encode("utf-8") + ", " + str(diff), result)))
    writer.write("\n")
    writer.close()

def main():
    import sys
    if (len(sys.argv) != 3):
        print "deepwalk_results_file_name = sys.argv[1], path_to_exact_solutions = sys.argv[2]. "
        return -1

    deepwalk_results_file_name = sys.argv[1]
    path_to_exact_solutions = sys.argv[2]
    print "Reading deepwalk results ... "
    deepwalk_results = read_deepwalk(deepwalk_results_file_name)
    print "Comparing results of deepwalk and exact solution ... "
    compare_hitting_time_deepwalk(deepwalk_results, path_to_exact_solutions)
    return 0

def main_target():
    import sys
    if (len(sys.argv) != 4):
        print "deepwalk_results_file_name = sys.argv[1], path_to_exact_solutions = sys.argv[2], target = sys.argv[3]. "
        return -1

    deepwalk_results_file_name = sys.argv[1]
    path_to_exact_solutions = sys.argv[2]
    target = sys.argv[3].decode("utf-8")
    print "Reading deepwalk results ... "
    deepwalk_results = read_deepwalk_with_target(deepwalk_results_file_name, target)
    print "Comparing results of deepwalk and exact solution ... "
    compare_hitting_time_deepwalk(deepwalk_results, path_to_exact_solutions)
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main_target())
