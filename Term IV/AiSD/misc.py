from random import randint
from random import choice


def generate_array(n):
    return [randint(0, n) for _ in range(n)]


def insert_sort(xs):
    """Complexity O(n^2), best case: O(n) -- when it's already sorted.
    The number of comparisons can be reduced down to nlogn (using binsearch)
    but the number of shifts stays the same"""
    for i in range(1, len(xs)):
        j = i-1
        x = xs[i]
        while j >= 0 and x < xs[j]:
            xs[j+1] = xs[j]
            j -= 1
        xs[j+1] = x
    return xs


def select_sort(xs):
    """Complexity teta(n^2) -- always does O(n^2) comparisons"""
    for i in range(len(xs)-1):
        x = i
        for j in range(i, len(xs)):
            if xs[j] < xs[x]:
                x = j
        t = xs[x]
        xs[x] = xs[i]
        xs[i] = t
    return xs


def partition(xs, b, e, pivot, pi):
    """pi - pivot index"""
    while b < e:
        if xs[b] >= pivot >= xs[e]:
            if b == pi:
                pi = e
            elif e == pi:
                pi = b
            xs[b], xs[e] = xs[e], xs[b]
            b += 1
            e -= 1
        elif xs[b] > pivot:
            e -= 1
        elif xs[e] <= pivot:
            b += 1
        else:
            b += 1
            e -= 1
    return pi


def partition2(xs, b, e, pivot):
    while b < e:
        while xs[b] <= pivot:
            b += 1
        while xs[e] >= pivot:
            e -= 1
        if b < e:
            xs[b], xs[e] = xs[e], xs[b]
        else:
            return e


def quicksort(xs, b, e):
    """avg. complexity teta(nlogn), worst case: O(n^2), best case: O(n)"""
    if e-b < 8:
        xs.sort()
    else:
        i = randint(b+1, e-1)
        pivot = xs[i]
        # i = partition(xs, b, e, pivot, i)
        i = partition2(xs, b, e, pivot)
        quicksort(xs, b, i)
        quicksort(xs, i+1, e)


def counting_sort(xs, min_val, max_val):
    """complexity: teta(n+k) where n = len(xs) and k = max_val - min_val"""
    ys = [0 for _ in range(min_val, max_val+1)]
    zs = [0] * len(xs)
    for i in range(len(xs)):
        ys[xs[i] - min_val] += 1
    for i in range(1, max_val - min_val + 1):
        ys[i] += ys[i-1]
    for i in range(len(xs)-1, -1, -1):
        zs[ys[xs[i]]-1] = xs[i]
        ys[xs[i]] -= 1
    return zs


alphabet = "abcdefghijklmnoprstuvwyz"


def lex_sort(xs):
    """Complexity: O((n+k)*d), where n=len(xs), k = len(alphabet), d = length of strings.
    Normally radix_sort's complexity = O(d * complexity_of_sorting_alg)"""
    k = len(alphabet)+1
    offset = ord('a')
    def custom_radix_sort(ys, index):
        cs = [0 for _ in range(k+1)]
        zs = [0] * len(ys)
        for i in range(len(ys)):
            cs[ord(ys[i][index]) - offset] += 1
        for i in range(1, k + 1):
            cs[i] += cs[i - 1]
        for i in range(len(xs) - 1, -1, -1):
            zs[cs[ord(ys[i][index]) - offset] - 1] = ys[i]
            cs[ord(ys[i][index]) - offset] -= 1
        return zs
    for i in range(len(xs[0])-1, -1, -1):
        xs = custom_radix_sort(xs, i)
    return xs


def generate_string(n):
    string = ""
    for _ in range(n):
        string += choice(alphabet)
    return string


def choose_pivot(xs):
    return randint(0, len(xs))

def kth_elem(xs, k):
    if len(xs) < 10:
        insert_sort(xs)
        return xs[k]
    p = choose_pivot(xs)
    lesser = []
    greater = []
    for elem in xs:
        if elem < p:
            lesser.append(elem)
        else:
            greater.append(elem)
    if k < len(lesser):
        return kth_elem(lesser, k)
    return kth_elem(greater, k - len(lesser))


def min_max(S):
    """Complexity: ceiling(3/2*n - 2)"""
    Smin = []
    Smax = []
    n = len(S)
    for i in range(n//2):
        if S[i] > S[n-i-1]:
            Smin.append(S[n-i-1])
            Smax.append(S[i])
        else:
            Smin.append(S[i])
            Smax.append(S[n-i-1])
    min_val = min(Smin)
    max_val = max(Smax)
    if n // 2 == 0:
        return min_val, max_val
    else:
        return min(min_val, S[n//2]), max(max_val, S[n//2])


def check_min_max():
    for i in range(30):
        xs = generate_array(20)
        x_min, x_max = min_max(xs)
        if x_min != min(xs) or x_max != max(xs):
            return False
    return True


def c(n, k):
    """n choose k, complexity teta(n*k)"""
    vals = []
    for _ in range(n):
        vals.append([1 for _ in range(n)])
    for j in range(k):
        vals[j][j] = 1
        for i in range(j+1, n):
            vals[i][j] = vals[i - 1][j - 1] if j > 0 else vals[i][j]
            vals[i][j] += vals[i-1][j]
    return vals[n-1][k-1]


def lcs(xs, ys):
    """Largest common subsequence.
    Complexity: teta(n*m)"""
    n = len(xs)
    m = len(ys)
    d = []
    for _ in range(n+1):
        d.append([0 for _ in range(m+1)])
    for i in range(1, n+1):
        for j in range(1, m+1):
            if xs[i-1] == ys[j-1]:
                d[i][j] = 1 + d[i-1][j-1]
            else:
                d[i][j] = max([d[i-1][j], d[i][j-1]])
    return d[n][m]


def lcs2(xs, ys):
    """lcs but also returns the sequence"""
    n = len(xs)
    m = len(ys)
    d = []
    for _ in range(n+1):
        d.append([[0, ""] for _ in range(m+1)])
    for i in range(1, n+1):
        for j in range(1, m+1):
            if xs[i-1] == ys[j-1]:
                d[i][j][0] = d[i-1][j-1][0] + 1
                d[i][j][1] = d[i-1][j-1][1] + xs[i-1]
            else:
                if d[i-1][j][0] > d[i][j-1][0]:
                    d[i][j][0] = d[i-1][j][0]
                    d[i][j][1] = d[i-1][j][1]
                else:
                    d[i][j][0] = d[i][j-1][0]
                    d[i][j][1] = d[i][j-1][1]
    return d[n][m]


def bags(weights, values, max_weight):
    """Bag capacity problem without duplicates.
    Complexity: O(nW)"""
    n = len(weights)
    v = []
    for _ in range(n+1):
        v.append([0 for _ in range(max_weight+1)])
    for j in range(1, n+1):
        for w in range(1, max_weight+1):
            if w >= weights[j-1]:
                v[w][j] = max([v[w-weights[j-1]][j-1] + values[j-1], v[w][j-1]])
    return v[n][max_weight]


def bags2(weights, values, max_weight):
    """bag capacity problem without duplicates
    the anwser is [final value, list of indexes whose values give that value]"""
    n = len(weights)
    v = []
    for _ in range(n+1):
        v.append([[0, []] for _ in range(max_weight+1)])
    for j in range(1, n+1):
        for w in range(1, max_weight+1):
            if w >= weights[j-1]:
                if v[w-weights[j-1]][j-1][0] + values[j-1] > v[w][j-1][0]:
                    v[w][j][0] = v[w - weights[j - 1]][j - 1][0]+values[j-1]
                    v[w][j][1] = v[w - weights[j - 1]][j - 1][1]+[j-1]
                else:
                    v[w][j] = v[w][j-1]
    return v[n][max_weight]


class Element:
    def __init__(self, key):
        self.key = key
        self.priority = 0
        self.left = None
        self.right = None
        self.parent = None

    def leafify(self):
        self.key = -1
        self.priority = -1
        self.left = None
        self.right = None

    def is_leaf(self):
        return self.priority == -1

    def is_root(self):
        return self.parent is None

    def __str__(self):
        string = "key: " + str(self.key)
        if not self.is_root():
            string += ", parent: " + str(self.parent.key)
        return string


class Treap:
    def __init__(self):
        self.priorities = []
        self.max_priority = 10000  # arbitrary number
        self.root = None

    def generate_priority(self, elem):
        priority = randint(0, self.max_priority)
        while priority in self.priorities:
            priority = randint(0, self.max_priority)
        elem.priority = priority
        self.priorities.append(priority)

    def rotate(self, elem):
        # using the 'every-priority-is-different' property
        if not elem.is_root() and elem.priority > elem.parent.priority:
            # if elem is the left son
            if elem.parent.left.priority == elem.priority:
                grandparent = elem.parent.parent
                if grandparent is not None and elem.parent.priority != self.root.priority:
                    if grandparent.left.priority == elem.parent.priority:
                        grandparent.left = elem
                    else:
                        grandparent.right = elem
                elem.parent.left = elem.right
                elem.right.parent = elem.parent
                elem.parent.parent = elem
                elem.right = elem.parent
                elem.parent = grandparent
            # if the elem is the right son
            elif elem.parent.right.priority == elem.priority:
                grandparent = elem.parent.parent
                if grandparent is not None and elem.parent.priority != self.root.priority:
                    if grandparent.right.priority == elem.parent.priority:
                        grandparent.right = elem
                    else:
                        grandparent.left = elem
                elem.parent.right = elem.left
                elem.left.parent = elem.parent
                elem.parent.parent = elem
                elem.left = elem.parent
                elem.parent = grandparent
            if elem.parent is None:
                self.root = elem
            self.rotate(elem)

    def find(self, key):
        vertex = self.root
        while not vertex.is_leaf() and vertex.key != key:
            if key < vertex.key:
                vertex = vertex.left
            else:
                vertex = vertex.right
        return vertex

    def insert(self, key):
        elem = Element(key)
        child = Element(-1)
        child.leafify()
        elem.left = child
        elem.right = child
        self.generate_priority(elem)
        if self.root is None:
            self.root = elem
            return
        vertex = self.root
        while True:
            if vertex.key > elem.key and vertex.left.is_leaf():
                vertex.left = elem
                elem.parent = vertex
                break
            elif vertex.key > elem.key:
                vertex = vertex.left
            elif vertex.key <= elem.key and vertex.right.is_leaf():
                vertex.right = elem
                elem.parent = vertex
                break
            else:
                vertex = vertex.right
        self.rotate(elem)

    def delete(self, key):
        vertex = self.find(key)
        if not vertex.is_leaf():
            vertex.priority = 0
            while not vertex.is_leaf():
                if vertex.left.is_leaf() and vertex.right.is_leaf():
                    vertex.leafify()
                elif vertex.left.is_leaf():
                    self.rotate(vertex.right)
                elif vertex.right.is_leaf():
                    self.rotate(vertex.left)
                else:
                    if vertex.left.priority > vertex.right.priority:
                        self.rotate(vertex.left)
                    else:
                        self.rotate(vertex.right)

    def print_treap(self):
        def helper(elem):
            if elem.is_leaf():
                return
            else:
                print(elem)
                helper(elem.left)
                helper(elem.right)
        helper(self.root)


def test_treap():
    treap = Treap()
    for i in range(10):
        treap.insert(i)
    for i in range(10):
        vertex = treap.find(i)
        if vertex.key != i:
            return False
    for i in range(10):
        treap.delete(i)
    return True
