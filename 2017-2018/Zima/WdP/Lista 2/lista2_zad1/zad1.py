def szachownica(n, k):
    for i in range(n):
        for j in range(k):
            #for l in range(2 * n):
            print(n*(k * ' ' + k * '#'), end='')
            print()
        for j in range(k):
            for l in range(n):
                print(k * '#' + k * ' ', end='')
            print()

szachownica(4,3)