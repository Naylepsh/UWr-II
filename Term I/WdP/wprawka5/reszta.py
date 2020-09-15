def change_iter(values, amount):
    s = (amount + 1) * [0]
    s[0] = 1
    for n in range(1, len(values)):
        for i in range(values[n], len(s)):
            s[i] = s[i] + s[i - values[n]]
    return sum(s)


def change_rec(values, amount):
    if amount == 0: return 1
    elif values == [] or amount < 0: return 0
    else: return change_rec(values, amount - values[0]) + change_rec(values[1:], amount)


vals = [1,2,5]
print(change_iter(vals, 1))
print(change_rec(vals, 1))
