def usun_nawiasy(text):
    temp = list(text)
    while str(temp).find("(") >= 0: # index() returns error if it cannot find an argument, thus we use find()
        start = temp.index("(")
        end = temp.index(")")
        for i in range(end, start - 1, -1):
            temp.pop(i)

    return ''.join(temp)


#print(usun_nawiasy(input('Enter your string: ')))
print(usun_nawiasy('(Ala) ma kota'))
print(usun_nawiasy('Ala (ma) kota'))
print(usun_nawiasy('Ala ma (kota)'))
print(usun_nawiasy('(Ala) ma (kota)'))
print(usun_nawiasy('(Ala ma k)o(ta)'))