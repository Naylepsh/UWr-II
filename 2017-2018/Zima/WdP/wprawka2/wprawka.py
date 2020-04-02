def f(coeff, x):
    p = 0
    power = len(coeff) - 1
    for c in coeff:
        p = p + c * (x ** power)
        power -= 1
    return p

def surf(p, xs, xf, dx):
    area = 0
    x = xs
    while x < xf:
        area += abs(dx * f(p, x))
        x += dx
    return area

def main():
    print(surf([1, 0, 3], 0, 3, 0.0001))

main()