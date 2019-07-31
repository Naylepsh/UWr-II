from math import sin
from math import pi

def kolko_v1(n):
    x_off = 25
    x_ratio = pi / n
    for i in range(n):
        how_many = int(round(n / 2 * sin(x_ratio * i)))
        if (how_many > 0):
            print((x_off - how_many) * ' ' + 2 * how_many * '*')

def kolko_v2(n):    # this one doesn't work as 'good' as the previous one
    x_off = 25
    for y in range (n):
        how_many = 0
        for x in range (n):
            if ((x - n / 2) ** 2 + (y - n / 2) ** 2 <= (n / 2) ** 2):
                how_many += 1
        print((x_off - how_many) * ' ', 2 * how_many * '*')

kolko_v1(20)
kolko_v1(30)
kolko_v1(40)