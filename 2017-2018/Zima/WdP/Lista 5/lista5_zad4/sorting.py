from random import randint
from time import time

def main():
    some_list = []
    for i in range(5000000):
        some_list.append(randint(0, 5))

    list1 = some_list[::]
    list2 = some_list[::]

    start3 = time()
    print(v3(list1))
    print(time() - start3)

    start4 = time()
    print(v4(list2))
    print(time() - start4)


def v3(L):
    temp = []
    for element in L:
        if not element in temp:
            temp.append(element)
    return temp


def v4(L):
    L.sort()

    temp = []
    for i in range(len(L) - 1):
        if L[i] != L[i+1]:
            temp.append(L[i])
    return temp


main()
