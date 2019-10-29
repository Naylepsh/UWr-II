def square_root(n):
    sum = 0
    i = 0
    while sum <= n:
        i += 1
        sum += 2*i-1
    return i-1


for i in range(37):
    print(i, square_root(i))