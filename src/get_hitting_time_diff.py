import os
import re

def get_hitting_time_results(input_file_name):
    assert(os.path.exists(input_file_name))
    result = dict()
    reader = open(input_file_name, "r")
    min_distance = 0
    for (index, string) in enumerate(reader):
        if (index > 0):
            a = string.strip("\n").split(" -> ")
            node = a[0]
            if len(re.findall(r'[\u4e00-\u9fff]+', node)) > 0:
                node = node.decode("utf-8")
            distance = float(a[1].split(":")[1].split(",")[0])
            if (index == 1):
                min_distance = distance
            result[node] = min_distance/float(distance)
    reader.close()
    return result

def get_hitting_time_diff(file1, file2):
    assert(os.path.exists(file1) and os.path.exists(file2))
    D1 = get_hitting_time_results(file1)
    D2 = get_hitting_time_results(file2)
    keys1 = set(D1.keys())
    keys2 = set(D2.keys())
    keys = list(keys1.intersection(keys2))
    diff = dict()
    s = 0.0
    for key in keys:
        diff[key] = abs(D1[key] - D2[key])
        s += diff[key]
    print_dict(diff)
    print "Total difference = " + str(s)
    return diff

def print_dict(D):
    pairs = sorted(D.iteritems(), key = lambda (k, v): v, reverse = True)
    print pairs[0][0], pairs[0][1]

def main_diff():
    import sys
    if len(sys.argv) != 3:
        print "file1 = sys.argv[1], file2 = sys.argv[2]. "
        return -1

    file1 = sys.argv[1]
    file2 = sys.argv[2]
    get_hitting_time_diff(file1, file2)

    return 0

def main():
    import sys
    path = "/Users/enzhili/Documents/red_chamber_dream/hitting_time_results_with_acceptance_probability_all_edges_one_edge_removed"
    reader = open("file_names.txt", "r")
    file_names = []
    for (index, string) in enumerate(reader):
        file_names.append(string.strip("\n"))
    reader.close()

    for i in range(len(file_names)):
        file1 = file_names[i]
        file2 = path + "/" + file_names[i]
        print file1, file2
        get_hitting_time_diff(file1, file2)
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
