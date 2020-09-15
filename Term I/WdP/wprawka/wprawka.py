def policz_p(wspolczynniki, x):
    p = 0
    potega = len(wspolczynniki) - 1
    for wspolczynnik in wspolczynniki:
        p = p + wspolczynnik * (x ** potega)
        potega -= 1
    return p


def policz_wyr(potega, wspolczynniki, x):
    if potega < 0:
        return 0
    else:
        return x * policz_wyr(potega - 1, wspolczynniki, x) + wspolczynniki[potega]


def p_horner(wspolczynniki, x):
    return policz_wyr(len(wspolczynniki) - 1, wspolczynniki, x)


def wez_wsp():
    n = eval(input("Podaj max. potege: "))
    wsp = []
    for i in range(n + 1):
        wsp.append(eval(input("Podaj wspolczynnik: ")))
    return wsp


def main():
    #wsp = wez_wsp()
    wsp = []
    #x = eval(input("Podaj x: "))
    x = 1337
    print(policz_p(wsp, x))
    print(p_horner(wsp, x))


main()


