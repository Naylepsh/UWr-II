from random import choice


def tlumacz(txt, pol_ang, brown):
    wynik = []
    for p in txt.split():
        if p in pol_ang:
            t = get_most_freq(pol_ang[p], brown)
            wynik.append(t)
        else:
            wynik.append('?' + p)
    return ' '.join(wynik)


def get_most_freq(words, brown):
    # Words have to be already translated
    poss_words = []
    for word in words:
        if word in brown:
            if len(poss_words) == 0:
                poss_words = [word]
            elif brown[word] > brown[poss_words[0]]:
                poss_words = [word]
            elif brown[word] == brown[poss_words[0]]:
                poss_words.append(word)
    return str(choice(poss_words))


def get_brown():
    brown = {}
    for line in open('brown.txt', encoding="utf8"):
        line = line.split()
        for word in line:
            brown[word] = brown.get(word, 0) + 1
    return brown


def get_pol_ang():
    pol_ang = {}  # pusty słownik

    for x in open('pol_ang.txt', encoding="utf8"):
        x = x.strip()
        L = x.split('=')
        if len(L) != 2:
            continue
        pol, ang = L
        if pol in pol_ang:
            pol_ang[pol].append(ang)
        else:
            pol_ang[pol] = [ang]
    return pol_ang


def main():
    pol_ang = get_pol_ang()
    brown = get_brown()
    zdanie = 'dziewczyna spotkać chłopiec i pójść do kino uerthf'
    print(tlumacz(zdanie, pol_ang, brown))


main()
