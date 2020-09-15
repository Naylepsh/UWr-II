from ceasar_cypher import *
from time import time


def longest_ceasar_ish(fname):
    # get longest word's length
    longest = 0
    for word in open(fname, encoding="utf8").read().split():
        word = word.split('|')
        for w in word:
            if len(w) > longest:
                longest = len(w)

    # make files for all words' lengths
    files = []
    for i in range(longest+1):
        file = open("files\\"+str(i)+".txt", "w", encoding="utf8")
        files.append(file)


    # fill the files with words
    for word in open(fname, encoding="utf8").read().split():
        word = word.split('|')
        for w in word:
            files[len(w)].write(w+'\n')

    # close all the files
    for i in range(longest+1):
        files[i].close()

    # search for the longest pairs
    print("beginning searching for pairs")
    pairs = []
    for i in range(longest, 0, -1):
        print(i)
        file = open("files\\"+str(i)+".txt", "r", encoding="utf8")
        offset = 0
        for word1 in file:
            print(word1[:-1])
            for word2 in file:
                if word1 != word2 and are_pair(word1, word2):
                    pairs.append((word1, word2))
            offset += len(word1) + 2
            file.seek(offset)
        file.close()
        if pairs != []:
            break
    return pairs


t = time()
print(longest_ceasar_ish("slowa.txt"))
print(time() - t)
#s1 = "abecadło z pieca spadło."
#s2 = "hjmkhłtz ę żpmkh ążhłtz."
#print(are_pair(s1, s2))