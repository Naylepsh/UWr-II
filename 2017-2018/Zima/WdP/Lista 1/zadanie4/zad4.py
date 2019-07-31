from losowanie_fragmentow import losuj_fragment

def losuj_haslo(n):
    password = ""
    while (len(password) != n):
        temp = losuj_fragment()
        while (len(password) + len(temp) == n - 1 or len(password) + len(temp) > n):
            temp = losuj_fragment()
        password += temp
    return password

for i in range(10):
    print(losuj_haslo(8))

for i in range(10):
    print(losuj_haslo(12))