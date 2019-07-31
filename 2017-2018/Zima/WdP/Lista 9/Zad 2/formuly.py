def ciagi_binarne(N):
    if N == 0:
        return [[]]
    return [[b] + c for b in [True, False] for c in ciagi_binarne(N - 1)]


# 2 --> [ [False, False], [False, True], [True, False], [True, True] ]


def wartosciowania(zmienne):
    cb = ciagi_binarne(len(zmienne))
    """
    # ZMIANY TUTAJ
    i = 0
    # Dla kazdej 'zmiennej' ktora ma postac T lub F nadajemy jej odpowiednia wartosc
    for zmienna in zmienne:
        if zmienna == 'T':
            # Zmieniamy tylko po jednym wartosciowaniu w kazdej grupie wartosciowan
            for j in range(len(cb)):
                cb[j][i] = True
        elif zmienna == 'F':
            for j in range(len(cb)):
                cb[j][i] = False
        i += 1
    """
    return [dict(zip(zmienne, ciag)) for ciag in cb]


def wartosc_formuly(F, wart):
    F = F.replace('*', ' and ')
    F = F.replace('+', ' or ')
    F = F.replace('-', ' not ')
    ### Niby mozna tak ale wtedy print(F, wart) daje smieszne rzeczy
    # kazde wystapienie stalej zostanie zastapiane odpowiednia dla niej wartoscia
    F = F.replace('T', ' True ')
    F = F.replace('F', ' False')
    print(F, wart)
    return eval(F, wart)


def spelnialna(F):
    # ZMIANY TUTAJ
    # wyrzucamy znaki +*()_
    # jesli uzyjemy zmienne = set(F) - set(+*() )
    # to wszystkie zmienne dluzsze niz 1 litera zostana
    # podzielone na male czesci
    # zatem uzyjemy 'customowej' funkcji
    zmienne = wyrzuc(F,'+*()- ')
    for wart in wartosciowania(zmienne):
        if wartosc_formuly(F, wart) == True:
            return True
    return False


def tautologia(F):
    zmienne = wyrzuc(F,'+*()- ')
    # jesli nie ma takiego wartosciowania
    # dla ktorego F jest falszywe
    # to F jest tautologia
    for wart in wartosciowania(zmienne):
        if wartosc_formuly(F, wart) == False:
            return False
    return True


def wyrzuc(lista, co):
    lista2 = lista.split()
    for i in range(len(lista2)):
        for ch in lista2[i]:
            if ch in co:
                lista2[i] = lista2[i].replace(ch,'')
    # wyrzucamy puste miejsca z tablicy
    return list(filter(None, lista2))


print(spelnialna('(-p) * (-q) * re'))
print(spelnialna('(-p) * T * re'))
print(tautologia('T * F'))
print(tautologia('(-F)'))



"""
filter function yields those items of a list for which function(arg) is true.
"""
