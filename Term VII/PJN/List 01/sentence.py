from random import choice
import numpy as np


class SentenceGenerator:
    def __init__(self, successors, create_next_word):
        self.successors = successors
        self.create_next_word = create_next_word

    def random_word(self):
        return choice(list(self.successors.keys()))

    def initial_word(self, word):
        return ' '.join(word)

    def random_successor_of(self, word):
        successors = self.successors[word]
        return choice(successors)[1]

    def has_successors(self, word):
        return word in self.successors

    def generate_sentence(self):
        word = self.random_word()
        while not self.has_successors(word):
            word = self.random_word()

        sentence = [self.initial_word(word)]
        while self.has_successors(word):
            successor = self.random_successor_of(word)
            word = self.create_next_word(word, successor)
            sentence.append(successor)

        return ' '.join(sentence)

    def generate_text(self, sentences=1):
        return '\n'.join([self.generate_sentence() for _ in range(sentences)])


class NonUniformSentenceGenerator(SentenceGenerator):
    def random_successor_of(self, word):
        successors = self.successors[word]
        total_occurences = sum(list(map(lambda s: s[0], successors)))
        probabilites = list(map(lambda s: s[0] / total_occurences, successors))
        successors = list(map(lambda s: s[1], successors))

        return np.random.choice(successors, 1, probabilites)[0]


class NGramSG:
    def __init__(self, successors, sentence_generator=SentenceGenerator):
        self.generator = sentence_generator(successors, self.create_next_word)

    def create_next_word(self, current_word, successor):
        pass

    def generate_sentence(self):
        return self.generator.generate_sentence()

    def generate_text(self, sentences):
        return self.generator.generate_text(sentences)


class BigramSG(NGramSG):
    def create_next_word(self, current_word, successor):
        return (successor, )


class TrigramSG(NGramSG):
    def create_next_word(self, current_word, successor):
        # current_word is (word1, word2)
        _, word2 = current_word
        next_word = (word2, successor)
        return next_word
