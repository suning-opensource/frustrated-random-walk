#!/usr/bin/python
# -*- coding: utf-8 -*-

import os, sys
import multiprocessing
import gensim 
import datetime

reload(sys)
sys.setdefaultencoding('utf-8')

def prepend(acct):
    return acct
    assert(len(acct) <= 19)
    s = ""
    for i in range(19 - len(acct)):
        s += "0"
    return s + acct

def word2vec_train(path_file_name, model_file_name, selection_probability):
    import random
    print "Reading in the file ... , time = " + str(datetime.datetime.now())
    reader = open(path_file_name, "r")
    outputFileName = path_file_name.replace(".txt", "_" + str(selection_probability) + ".txt")
    writer = open(outputFileName, "w")
    for (index, string) in enumerate(reader):
        r = random.uniform(0, 1)
        if r < selection_probability:
            writer.write(string)
    reader.close()
    writer.close()
    sentences = gensim.models.word2vec.LineSentence(outputFileName)
    print "File reading finished. Training the model ... , time = " + str(datetime.datetime.now())
    model = gensim.models.Word2Vec(sentences, size = 128, window = 5, min_count = 10, sg = 1, workers = multiprocessing.cpu_count())
    print "Model training finished. Saving the model ... , time = " + str(datetime.datetime.now())
    model.save(model_file_name)
    print "Model saved. Done. Time = " + str(datetime.datetime.now())

def read_vertices(vertexFileName):
    assert(os.path.exists(vertexFileName))
    result = []
    ifile = open(vertexFileName, "r")
    for (index, string) in enumerate(ifile):
        result.append(string.strip("\n").decode("utf-8"))
    ifile.close()
    return result

def load_model(model_file_name):
    vertexFileName = "vertices.txt"
    from gensim.models import Word2Vec
    model = Word2Vec.load(model_file_name)
    assert(os.path.exists(vertexFileName))
    assert(os.path.exists(model_file_name))
    accts = read_vertices(vertexFileName)
    acctSize = len(accts)
    resultFileName = "node2vec_results.txt"
    ofile = open(resultFileName, "w")
    for i in range(len(accts)):
        try:
            neighbors = model.most_similar(accts[i], topn = len(accts))
            #neighbors = model.most_similar(accts[i].encode("utf-8"), topn = len(accts))
            ofile.write("target = " + prepend(accts[i]) + "\n")
            ofile.write("\n".join(map(lambda ele: prepend(ele[0]) + "," + str(ele[1]), neighbors)) + "\n")
        except:
            print accts[i] + " not found. "
    ofile.close()

    if (False and os.path.exists("final_result.txt")):
        rank_result = dict()
        ifile = open("final_result.txt", "r")
        for (index, string) in enumerate(ifile):
            if ("value" in string):
                break
            rank_result[string.decode("utf-8").split(":")[0].split(";")[1]] = str(index + 1)
        ifile.close()

        rank_result_sorted = sorted(rank_result.items(), key = lambda p: int(p[1]))
        ofile = open("rank_result.txt", "w")
        for (key, value) in rank_result_sorted:
            ofile.write(str(key.encode("utf-8")) + "," + str(value) + "\n")
        ofile.close()

        ifile = open(resultFileName, "r")
        ofile = open(resultFileName.replace(".txt", "_ranked.txt"), "w")
        for (index, string) in enumerate(ifile):
            string = string.decode("utf-8")
            if ("target" in string):
                a = string.strip("\n").split(" = ")
                ofile.write(string.replace(a[-1], rank_result[str(int(a[-1]))]).encode("utf-8"))
            else:
                a = string.split(",")
                ofile.write(string.replace(a[0], rank_result[str(int(a[0]))]).encode("utf-8"))
        ofile.close()
        ifile.close()


def load_model_for_target(model_file_name, target):
    vertexFileName = "vertices.txt"
    from gensim.models import Word2Vec
    model = Word2Vec.load(model_file_name)
    assert(os.path.exists(vertexFileName))
    assert(os.path.exists(model_file_name))
    accts = read_vertices(vertexFileName)
    acctSize = len(accts)
    resultFileName = "node2vec_results.txt"
    ofile = open(resultFileName, "w")
    try:
        neighbors = model.most_similar(target, topn = len(accts))
        ofile.write("target = " + target + "\n")
        ofile.write("\n".join(map(lambda ele: prepend(ele[0]) + "," + str(ele[1]), neighbors)) + "\n")
    except:
        print target + " not found."
    ofile.close()

def deepwalk(path_file_name, model_file_name, selection_probability, path_to_exact_solutions):
    assert(os.path.exists(path_file_name))
    assert(selection_probability >= 0 and selection_probability <= 1.0)

    word2vec_train(path_file_name, model_file_name, selection_probability)
    print "Loading the model and writing out the results ... "
    load_model(model_file_name)
    deepwalk_results_file_name = "deepwalk_results_" + str(selection_probability) + ".txt"
    os.system("mv node2vec_results.txt " + deepwalk_results_file_name)
    
    print "Calling compare_hitting_time_deepwalk.py to calculate the differences between deepwalk and exact solution ... "
    os.system("python compare_hitting_time_deepwalk.py " + deepwalk_results_file_name + " " + path_to_exact_solutions)
    os.system("mv differences.txt differences_" + str(selection_probability) + ".txt")
    print "Done. "

def deepwalk_with_target(path_file_name, model_file_name, selection_probability, target, path_to_exact_solutions):
    assert(os.path.exists(path_file_name))
    assert(selection_probability >= 0 and selection_probability <= 1.0)

    word2vec_train(path_file_name, model_file_name, selection_probability)
    print "Loading the model and writing out the results ... "
    load_model_for_target(model_file_name, target)
    deepwalk_results_file_name = "deepwalk_results_" + str(selection_probability) + ".txt"
    os.system("mv node2vec_results.txt " + deepwalk_results_file_name)
    
    print "Calling compare_hitting_time_deepwalk.py to calculate the differences between deepwalk and exact solution ... "
    os.system("python compare_hitting_time_deepwalk.py " + deepwalk_results_file_name + " " + path_to_exact_solutions)
    os.system("mv differences.txt differences_" + str(selection_probability) + ".txt")
    print "Done. "

def read_differences(p, target):
    output_file_name = "difference_target_" + target + ".txt"
    writer = open(output_file_name, "w")
    for i in range(len(p)):
        difference_file_name = "differences_" + str(p[i]) + ".txt"
        reader = open(difference_file_name, "r")
        line = reader.readline()
        reader.close()
        difference = float(line.split(", ")[1])
        writer.write(str(p[i]) + "," + str(difference) + "\n")
    writer.close()

def main_target():
    import sys
    
    if len(sys.argv) != 5:
        print "path_file_name = sys.argv[1], model_file_name = sys.argv[2], target = sys.argv[3], path_to_exact_solutions = sys.argv[4]. "
        return -1

    p = [0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
    path_file_name = sys.argv[1]
    model_file_name = sys.argv[2]
    target = sys.argv[3].decode("utf-8")
    path_to_exact_solutions = sys.argv[4]

    for selection_probability in p:
        print "selection probability = " + str(selection_probability)
        deepwalk_with_target(path_file_name, model_file_name, selection_probability, target, path_to_exact_solutions)
    
    read_differences(p, target)
    return 0

def main():
    import sys
    
    if len(sys.argv) != 4:
        print "path_file_name = sys.argv[1], model_file_name = sys.argv[2], path_to_exact_solutions = sys.argv[3]. "
        return -1

    p = [0.5, 1.0]
    path_file_name = sys.argv[1]
    model_file_name = sys.argv[2]
    path_to_exact_solutions = sys.argv[3]

    for selection_probability in p:
        print "selection probability = " + str(selection_probability)
        deepwalk(path_file_name, model_file_name, selection_probability, path_to_exact_solutions)
    
    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main())
