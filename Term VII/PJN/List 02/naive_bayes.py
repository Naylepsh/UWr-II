from collections import Counter
import re
import operator
from math import log as ln
from typing import List
from file_utils import SentenceLengthRatio


class AuthorshipClass:
    def __init__(self, symbol, word_count, sentence_length_ratio: SentenceLengthRatio):
        self.symbol = symbol
        self.word_count = word_count
        self.total_count = sum([word_count[word] for word in word_count])
        self.sentence_length_ratio = sentence_length_ratio

    def p_of_word(self, word):
        count = self.word_count[word] if word in self.word_count else 0.01
        return count / self.total_count

    def p_of_sentence(self):
        return self.sentence_length_ratio.gte_length_count / self.sentence_length_ratio.all_count


def naive_bayes(words: List[str], classes: List[AuthorshipClass], sentences_len_ratio: SentenceLengthRatio):
    cs = {c.symbol: ln(c.total_count) for c in classes}

    # sentence length trait
    for c in classes:
        # ppb of
        cs[c.symbol] += ln(c.p_of_sentence()) * \
            sentences_len_ratio.gte_length_count
        cs[c.symbol] += ln(1 - c.p_of_sentence()) * \
            (sentences_len_ratio.all_count - sentences_len_ratio.gte_length_count)

    # word trait
    for word in words:
        for c in classes:
            ppb_of_word = c.p_of_word(word)
            cs[c.symbol] += ln(ppb_of_word)
    return max(cs.items(), key=lambda class_ppb: class_ppb[1]), cs
