import re
from collections import Counter, defaultdict


class SentenceLengthRatio:
    def __init__(self, length, gte_length_count, all_count):
        self.length = length
        self.gte_length_count = gte_length_count
        self.all_count = all_count


def count_words_in_file(filename):
    words = get_words_from_file(filename)
    word_count = Counter(words)
    return dict(word_count)


def get_words_from_file(filename):
    def parseText(text):
        regex = re.compile(r'[,\.!?:;\'0-9\*\-“…\(\)„”—»«–––=\[\]’]')
        words = regex.sub('', text.lower()).split()
        return words
    return parse_data_from_file(filename, parseText)


def count_sentence_lengths_in_file(filename):
    def parseText(text):
        lengths = {}
        lengths = defaultdict(lambda: 0, lengths)
        regex = re.compile(r'[,\.!?:;\'0-9\*\-“…\(\)„”—»«–––=\[\]’]')
        sentences = regex.sub('', text.lower()).split('\n')

        for sentence in sentences:
            words = sentence.split()
            lengths[len(words)] += 1

        return lengths
    return parse_data_from_file(filename, parseText)


def get_sentence_length_gte_ratio(filename, length):
    def parseText(text):
        sentences = get_sentences_from_text(text)
        sentences_with_gte_length = list(
            filter(lambda sentence: len(sentence) >= length, sentences))
        return SentenceLengthRatio(length, len(sentences_with_gte_length), len(sentences))
    return parse_data_from_file(filename, parseText)


def get_sentences_from_text(text):
    return list(filter(lambda x: len(x) > 2,
                       re.split(r'\?|\.|\!|\n|…', text)))


def parse_data_from_file(filename, parseText):
    with open(filename, 'r', encoding='utf-8') as file:
        text = file.read()
        data = parseText(text)
    return data
