from collections import Counter
import re
import operator
from math import log as ln
from typing import List


class AuthorshipClass:
    def __init__(self, symbol, word_count, sentence_length_count):
        self.symbol = symbol
        self.word_count = word_count
        self.total_count = sum([word_count[word] for word in word_count])
        self.sentence_length_count = sentence_length_count
        self.total_length_count = sum(
            [sentence_length_count[length] for length in sentence_length_count])

    def p_of_word(self, word):
        count = self.word_count[word] if word in self.word_count else 0.01
        return count / self.total_count

    def p_of_sentence(self, length):
        count = self.sentence_length_count[length] if length in self.sentence_length_count else 0.01
        return count / self.total_length_count


def naive_bayes(words: List[str], classes: List[AuthorshipClass], sent_length_mult):
    cs = {c.symbol: 1 for c in classes}

    # sentence length trait
    sentence_length = len(words)
    for c in classes:
        mult = sent_length_mult(words)
        cs[c.symbol] += mult * ln(c.p_of_sentence(sentence_length))

    # word trait
    for word in words:
        for c in classes:
            ppb_of_word = c.p_of_word(word)
            cs[c.symbol] += ln(ppb_of_word)
    return max(cs.items(), key=lambda class_ppb: class_ppb[1]), cs
