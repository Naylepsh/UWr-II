from random import choice
from random import randint
from random import shuffle

def lazy_select(S, k):
    while True:
        # 1
        R = []
        n = len(S)
        for i in range(int(n**(3/4))):
            R.append(choice(S))
        # 2
        R.sort()
        # 3
        x = k*(n**(-1/4))
        L = R[max(int(x - n**(1/2)), 1) - 1]
        H = R[min(int(x + n**(1/2)), int(n**(3/4))) - 1]
        # 4
        rs = 0
        P = []
        for elem in S:
            # #{y in S | y < L}
            if elem < L:
                rs+=1
            # {y in S | L <= k <= H}
            if L <= elem <= H:
                P.append(elem)
        # 5
        # if L and H gave us a small enough list containing k-th elem, then sort that list and return the element
        if rs <= k <= rs + len(P) and len(P) <= 4*(n**(3/4))+2:
            break
    # 6
    P.sort()
    return P[k-rs-1]


def selection(k, T):
    if len(T) < 10:
        T.sort()
        return T[k]
    p = lazy_select(T.copy(), k)
    U = []
    for elem in T:
        if elem < p:
            U.append(elem)
    if k < len(U):
        return selection(k, U)
    else:
        for elem in U:
            T.remove(elem)
        return selection(k - len(U) - 1, T)



A = list(range(1,randint(1,100)))
shuffle(A)
k = randint(1, len(A))
print("|A|=", len(A), "k =", k)
print(selection(k, A))

