def is_composed_from(word1, word2):
    """Checks whether word1 is composed from letters from word2"""

    freq2 = letter_occ(word2)
    # with every loop the number of a letter's occurrences is decreased
    # if there happen to be more instances of some letter in word1
    # than in word2 - loop terminates and returns False
    for letter in word1:
        if letter not in word2:
            return False
        # if there're more letter's occurrences in word1 than in word2
        elif freq2[letter] == 0:
            return False
        else:
            freq2[letter] -= 1
    return True


def letter_occ(word):
    """Returns a dictionary with all word's letter's number of occurrences"""

    letters = {}
    for letter in word:
        letters[letter] = letters.get(letter, 0) + 1
    return letters


def main():
    word1 = "halo"
    word2 = "aloha"
    print(is_composed_from(word1, word2))
    print(is_composed_from(word2, word1))
    word3 = "ayy lmao"
    word4 = "lol"
    print(is_composed_from(word3, word4))
    print(is_composed_from(word4, word3))


main()