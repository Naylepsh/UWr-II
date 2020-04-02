from random import randint
from time import time


def only_one(L):
    temp = []
    for element in L:
        if not element in temp:
            temp.append(element)
    return temp


def main():
    some_list = []
    for i in range(5000000):
        some_list.append(randint(0, 5))

    start3 = time()
    print(only_one(some_list))
    print(time() - start3)


main()