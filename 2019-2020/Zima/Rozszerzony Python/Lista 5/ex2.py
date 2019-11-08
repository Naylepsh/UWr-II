#!/usr/bin/env python3
# coding: utf8


import re, itertools


def solve(string):
    """
    Solves given cryptarithm by generating all possible combinations and checking if any of them work
    """
    string = string.upper()
    letters = set(string) - set(' +=')
    if 0 < len(letters) <= 10: # if there is more than 10 letters, then cryptarithm is invalid
        equation = re.findall(r"[A-Z]+", string)
        for perm in itertools.permutations(range(10), len(letters)):
            solution = dict(zip(letters, perm))
            if get_value(solution, equation[0], equation[1]) == get_value(solution, equation[2]):
                return solution
    return None


def get_value(vals, *strings):
    """
    Calculates the value of given strings by substituting digits for letters and summing results
    """
    def to_int(string):
        return int(''.join(map(lambda letter: str(vals[letter]), string)))
    
    return sum(map(to_int, strings))

print(solve("SEND + MORE = MONEY"))