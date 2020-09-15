#!/usr/bin/env python3
# coding: utf8


import sys


def square_root(n):
    sum = 0
    i = 0
    while sum <= n:
        i += 1
        sum += 2*i-1
    return i-1


if __name__ == '__main__':
    if len(sys.argv) > 1:
        print(square_root(int(sys.argv[1])))