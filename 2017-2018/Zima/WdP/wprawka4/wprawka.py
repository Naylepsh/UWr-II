def num(i):
    def temp(t):
        return i
    return temp


def i_2(i):
    return i + 2


def den_e(i):
    i += 2
    if i % 3 == 0:
        return 2 * (i / 3)
    return 1


def den_odd(i):
    i += 1
    return 2*i + 1


def cont_frac(num, den, k):
    appro = 0
    while k >= 0:
        D = den(k) + appro
        appro = num(k) / D
        k -= 1
    return appro


def tan_cf(x, k):
    return x / (1 + cont_frac(num(-x*x), den_odd, k - 1))


def main():
    print(cont_frac(num(1), i_2, 1))
    print("e =", 2 + cont_frac(num(1), den_e, 10000))
    x = 50
    print("tan"+str(x)+" =", tan_cf(50, 10000))

main()
