def main():
    string = "  Ala    ma  kota "
    print(podziel(string))


def podziel(s):
    temp = []
    word = ""
    for letter in s:
        if letter.isspace():
            if len(word) > 0:
                temp.append(word)
                word = ""
        else:
            word += letter
    if len(word) > 0:
        temp.append(word)
    return temp


main()