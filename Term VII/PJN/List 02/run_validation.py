from naive_bayes_v2 import naive_bayes, AuthorshipClass
from file_utils import count_words_in_file, count_sentence_lengths_in_file, get_sentences_from_text

orzeszkowa_suffix = 'korpus_orzeszkowej.txt'
prus_suffix = 'korpus_prusa.txt'
sienkiewicz_suffix = 'korpus_sienkiewicza.txt'

teaching_set_prefix = './dane/teaching'
orzeszkowa_teaching_file = f'{teaching_set_prefix}_{orzeszkowa_suffix}'
prus_teaching_file = f'{teaching_set_prefix}_{prus_suffix}'
sienkiewicz_teaching_file = f'{teaching_set_prefix}_{sienkiewicz_suffix}'

orzeszkowa_words = count_words_in_file(orzeszkowa_teaching_file)
orzeszkowa_lengths = count_sentence_lengths_in_file(orzeszkowa_teaching_file)
prus_words = count_words_in_file(prus_teaching_file)
prus_lengths = count_sentence_lengths_in_file(prus_teaching_file)
sienkiewicz_words = count_words_in_file(sienkiewicz_teaching_file)
sienkiewicz_lengths = count_sentence_lengths_in_file(sienkiewicz_teaching_file)

orzeszkowa = AuthorshipClass('o', orzeszkowa_words, orzeszkowa_lengths)
prus = AuthorshipClass('p', prus_words, prus_lengths)
sienkiewicz = AuthorshipClass('s', sienkiewicz_words, sienkiewicz_lengths)


def validate(expected_symbol, filename):
    with open(filename, 'r', encoding='utf-8') as f:
        sentences = get_sentences_from_text(f.read())

    total = 0
    correct = 0
    for sentence in sentences:
        words = sentence.split()
        res = naive_bayes(words, [orzeszkowa, prus, sienkiewicz])
        if res[0][0] == expected_symbol:
            correct += 1
        total += 1
    return correct / total


def validate_orzeszkowa():
    return validate(orzeszkowa.symbol, f'./dane/validation_{orzeszkowa_suffix}')


def validate_prus():
    return validate(prus.symbol, f'./dane/validation_{prus_suffix}')


def validate_sienkiewicz():
    return validate(sienkiewicz.symbol, f'./dane/validation_{sienkiewicz_suffix}')


print('orzeszkowa')
print(validate_orzeszkowa())

print('prus')
print(validate_prus())

print('sienkiewicz')
print(validate_sienkiewicz())
