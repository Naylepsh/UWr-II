import re


def find_longest(fname):
    # make a regex of words without polish characters
    word_regex = re.compile(r"[^ęóąśłżźćń]+")
    # make a dictionary of polish words
    polish_words = {}
    for wd in open("slowa.txt", encoding="utf8").read().split():
        polish_words[wd] = '1'

    # search for longest text sample without polish chars
    longest = current = ""
    text = open(fname, encoding="utf8").read()
    words = text.split()
    for word in words:
        temp = word_regex.match(word)
        # if a word doesn't have polish chars, strip it of dots, colons, etc
        # and check whether it's a polish word
        # if so, add it to current sample
        # if not, end the same right there and move to the next one
        if temp != None and temp.group(0) == word and is_in(word, polish_words):
            current = current + " " + word
        else:
            if is_longer(current, longest):
                longest = current
            current = ""
    # checking one more time after exitting a loop
    # in case the last word in a loop satisfied if statement
    if is_longer(current, longest):
        longest = current
    return longest


def is_longer(word1, word2):
    w1_len = 0
    for letter in word1:
        if letter.isalpha():
            w1_len += 1
    w2_len = 0
    for letter in word2:
        if letter.isalpha():
            w2_len += 1
    return w1_len > w2_len


def strip_chars(text, chars):
    for element in text:
        if element in chars:
            text.replace(element, '')
    return text


def is_in(word, polish_words):
    word = strip_chars(word, ",.?!")
    if word in polish_words:
        return True
    return False

print(find_longest("lalka-tom-pierwszy.txt"))