from time import time


def letter_occ(word):
    letters = {}
    for letter in word:
        #letters[letter] = letters.get(letter, 0) + 1  # .get is slow af
        if letter in letters:
            letters[letter] += 1
        else:
            letters[letter] = 1
    return letters


def is_composed_from(word1, word2):
    freq2 = letter_occ(word2)
    for letter in word1:
        if letter not in word2:
            return False
        if freq2[letter] == 0:
            return False
        else:
            freq2[letter] -= 1
    return True


def turn_into_pair(string):
    """Returns all word pairs with the same amount of letters as argument"""

    # Finds all possible pairs by checking whether
    # what words from 'slowa.txt' file could be
    # composed from string argument
    string = string.lower()
    poss_pairs = []
    with open("slowa.txt", encoding="utf8") as file:
        for word in file.read().split():
            if is_composed_from(word, string):
                poss_pairs.append(word)

    # Finds 'correct' pairs
    # for every word in poss_pairs
    # all other words that happen to be stored further in a list
    # are checked whether if combined with that word
    # would they make a pair that could be composed from string argument
    pairs = []
    for i in range(len(poss_pairs)):
        for j in range(i + 1, len(poss_pairs)):
            pair = poss_pairs[i] + poss_pairs[j]
            if len(pair) == len(string) - 1:  # - 1 because of ' ' character
                if is_composed_from(pair, string) and is_composed_from(string, pair):
                    if not are_in(poss_pairs[i], poss_pairs[j], pairs):  # if the pair isn't already in pairs list
                        pairs.append([poss_pairs[i], poss_pairs[j]])
    return pairs


def are_in(s1, s2, a_list):
    for pair in a_list:
        if [s1, s2] == pair or [s2, s1] == pair:
            return True
    return False


def print_nicely(pairs):
    for pair in pairs:
        print(pair)


#t = time()
print_nicely(turn_into_pair('Sebastian Kondraciuk'))
#print(time() - t)
