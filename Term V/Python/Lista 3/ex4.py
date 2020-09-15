#!/usr/bin/env python3
# coding: utf8


from random import sample
from re import sub


def simplify_sentence(text, max_length, num_of_words):
  """ 
  Removes all words of a length greater than max_length from a given text.
  If the number of remaining words exceeds num_of_words, randomly removes necessary amount
  """
  text = list(filter(lambda word: len(word) <= max_length, sub('[.,!?\-;]', '', text).split(' ')))
  to_delete = sample(range(len(text)), len(text)-num_of_words)
  return ' '.join([x for i,x in enumerate(text) if i not in to_delete])


if __name__ == '__main__':
  text = """
Adam Mickiewicz - Pan Tadeusz - Inwokacja

 

Litwo! Ojczyzno moja! ty jesteś jak zdrowie.

Ile cię trzeba cenić, ten tylko się dowie,

Kto cię stracił. Dziś piękność twą w całej ozdobie

Widzę i opisuję, bo tęsknię po tobie.

 

Panno Święta, co Jasnej bronisz Częstochowy

I w Ostrej świecisz Bramie! Ty, co gród zamkowy

Nowogródzki ochraniasz z jego wiernym ludem!

Jak mnie dziecko do zdrowia powróciłaś cudem

(Gdy od płaczącej matki pod Twoję opiekę

Ofiarowany, martwą podniosłem powiekę

I zaraz mogłem pieszo do Twych świątyń progu

Iść za wrócone życie podziękować Bogu),

Tak nas powrócisz cudem na Ojczyzny łono.

Tymczasem przenoś moję duszę utęsknioną

Do tych pagórków leśnych, do tych łąk zielonych,

Szeroko nad błękitnym Niemnem rozciągnionych;

Do tych pól malowanych zbożem rozmaitem,

Wyzłacanych pszenicą, posrebrzanych żytem;

Gdzie bursztynowy świerzop, gryka jak śnieg biała,

Gdzie panieńskim rumieńcem dzięcielina pała,

A wszystko przepasane, jakby wstęgą, miedzą

Zieloną, na niej z rzadka ciche grusze siedzą.
"""
  print(simplify_sentence(text, 6, 10))