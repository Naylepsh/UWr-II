def find_pairs(infile):
    txt = open(infile, "r", encoding="utf8").read()
    txt = txt.split()

    # for some reason finding a key in dictionary
    # takes way less time than
    # finding a phrase in string
    words = []
    for word in txt:
        words.append(word)

    pairs = {}
    for key in words:
        rev = key[::-1]
        if rev in words and not rev in pairs:
            pairs[key] = rev

    return pairs

def main():
    outfile = open("wynik.txt", "w")
    pairs = find_pairs("slowa.txt")
    for key in pairs:
       #print(key, "<->", pairs[key])
       print(key, "<->", pairs[key], file=outfile)


main()
