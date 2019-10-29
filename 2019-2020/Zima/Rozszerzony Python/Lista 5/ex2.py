import re, itertools


def solve(string):
    string = string.upper()
    letters = set(string) - set(' +=')
    # since we're using decimal system
    # we have to check whether there are 10 or less
    # different letters
    if 0 < len(letters) <= 10:
        # divide into words
        equation = re.findall(r"[A-Z]+", string)
        
        for perm in itertools.permutations(range(10), len(letters)):
            solution = dict(zip(letters, perm))
            if get_value(solution, equation[0], equation[1]) == get_value(solution, equation[2]):
                return solution
    return None


def get_value(vals, *strings):
    value = 0
    for string in strings:
        pt = 1
        for letter in reversed(string):
            value += vals[letter] * pt
            pt *= 10
    return value

print(solve("SEND + MORE = MONEY"))