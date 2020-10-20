import os

def get_distances(hitting_time_file_name, distance_file_name):
    assert(os.path.exists(hitting_time_file_name))
    reader = open(hitting_time_file_name, "r")
    lines = reader.readlines()
    reader.close()

    writer = open(distance_file_name, "w")
    for i in range(len(lines)):
        distance = lines[i].split(":")[1].split(",")[0]
        writer.write(str(i) + "  " + distance + "\n")
    writer.close()
    

def main():
    get_distances("hitting_times.txt", "sorted_distances_monte_carlo.txt")
    get_distances("hitting_times_mean_variance.txt", "sorted_distances_numerical.txt")
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
