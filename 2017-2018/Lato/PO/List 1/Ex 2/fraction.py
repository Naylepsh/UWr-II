# _c - means by copying
# _m - means by modyfing existing argument
# f = [numerator, denominator]

class Fraction:
    numerator = 1
    denominator = 1


def gcd(x, y):
    while y > 0:
        x, y = y, x % y
    return x


def lcm(x, y):
    z = abs(x * y) / gcd(x, y)
    return z


def __correct(f):
    if f[1] < 0:
        f[0] *= -1
        f[1] *= -1

    f[0], f[1] = __shorten(f[0], f[1])
    f[0] = int(f[0])
    f[1] = int(f[1])

    assert(f[1] != 0)


def __shorten(x, y):
    d = gcd(x, y)
    return x / d, y / d


def __get_multipliers(x, y):
    m = lcm(x, y)
    return m, m / x, m / y


def addition_c(f1, f2):
    m, m1, m2 = __get_multipliers(f1[1], f2[1])

    f = [f1[0] * m1 + f2[0] * m2, m]

    __correct(f)
    return f


def addition_m(f1, f2):
    m, m1, m2 = __get_multipliers(f1[1], f2[1])

    f2[0] = f1[0] * m1 + f2[0] * m2
    f2[1] = m

    __correct(f2)


def substraction_c(f1, f2):
    t = [-1 * f2[0], f2[1]]
    return addition_c(f1, t)


def substraction_m(f1, f2):
    f2[0] *= -1
    addition_m(f1, f2)


def multiplication_c(f1, f2):
    # making sure not to divide by 0
    __correct(f2)
    f = [f1[0] * f2[0], f1[1] * f2[1]]
    __correct(f)
    return f


def multiplication_m(f1, f2):
    # making sure not to divide by 0
    __correct(f2)
    f2 = [f1[0] * f2[0], f1[1] * f2[1]]
    __correct(f2)


def division_c(f1, f2):
    t = [f2[1], f2[0]]
    return multiplication_c(f1, t)


def division_m(f1, f2):
    f2[0], f2[1] = f2[1], f2[0]
    multiplication_m(f1, f2)


def to_string(f):
    return str(f[0]) + "/" + str(f[1])
