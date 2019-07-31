def is_composed_from(word1, word2):
    """Checks whether word1 is composed from letters from word2"""

    freq1 = letter_frequency(word1)
    freq2 = letter_frequency(word2)
    for letter in freq1:
        if letter not in word2 or freq1[letter] > freq2[letter]:
            return False
    return True


def letter_frequency(word):
    """Returns a dictionary with all word's letter's frequency"""

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