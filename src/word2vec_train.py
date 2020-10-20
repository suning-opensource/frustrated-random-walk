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

def word2vec_train(pathFileName, modelFileName):
    print "Reading in the file ... , time = " + str(datetime.datetime.now())
    sentences = gensim.models.word2vec.LineSentence(pathFileName)
    print "File reading finished. Training the model ... , time = " + str(datetime.datetime.now())
    model = gensim.models.Word2Vec(sentences, size = 300, window = 5, min_count = 10, sg = 1, workers = multiprocessing.cpu_count())
    print "Model training finished. Saving the model ... , time = " + str(datetime.datetime.now())
    model.save(modelFileName)
    print "Model saved. Done. Time = " + str(datetime.datetime.now())

def readAccts(vertexFileName):
    assert(os.path.exists(vertexFileName))
    result = []
    ifile = open(vertexFileName, "r")
    for (index, string) in enumerate(ifile):
        result.append(string.strip("\n").decode("utf-8"))
    ifile.close()
    return result

def load_model(modelFileName):
    vertexFileName = "vertices.txt"
    from gensim.models import Word2Vec
    model = Word2Vec.load(modelFileName)
    assert(os.path.exists(vertexFileName))
    assert(os.path.exists(modelFileName))
    accts = readAccts(vertexFileName)
    acctSize = len(accts)
    resultFileName = "deepwalk_results.txt"
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

def main():
    if (len(sys.argv) != 3):
        print "pathFileName = sys.argv[1], modelFileName = sys.argv[2]. "
        return -1

    pathFileName = sys.argv[1]
    modelFileName = sys.argv[2]
    word2vec_train(pathFileName, modelFileName)
    print "Loading the model and writing out the results ... "
    load_model(modelFileName)
    print "Done. "
    return 0

if __name__ == '__main__':
    sys.exit(main())
