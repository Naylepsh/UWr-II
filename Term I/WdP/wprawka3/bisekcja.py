def sign(x):
    if x < 0:
        return -1
    else:
        return 1


def bisect(f, x, y):
    e = 2 ** (-32)
    assert (sign(f(x)) != sign(f(y))), "Function has the same sign on both ends of interval"
    while abs(x - y) > e:
        if f(x) == 0:
            return x
        elif f(y) == 0:
            return y

        if sign(f(x)) != sign(f((x + y) / 2)):
            y = (x + y) / 2
        else:
            x = (x + y) / 2
    return (x + y) / 2


def get_p(coeffs):
    def __no_name(x):
        p = 0
        power = len(coeffs) - 1
        for coeff in coeffs:
            p = p + coeff * (x ** power)
            power -= 1
        return p
    return __no_name


def main():
    x = 0
    y = 100
    z = bisect(get_p([1,0,0,-4,-8]), x, y)
    print(z)


main()
