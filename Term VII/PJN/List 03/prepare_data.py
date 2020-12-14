from collections import defaultdict


def create_set(src, dest, skip, take):
    with open(dest, 'w', encoding='utf-8') as outfile:
        with open(src, 'r', encoding='utf-8') as infile:
            for line in infile:
                if skip > 0:
                    skip -= 1
                    continue

                if take == 0:
                    break

                take -= 1
                line = line.replace(' !', '.').replace(' ?', '.').replace(
                    ' (', '').replace(' )', '').replace(' [', '').replace(' ]', '').replace(' ,', '')
                outfile.write(line)


def create_teaching_set(src, dest):
    skip, take = 0, 1000000
    create_set(src, dest, skip, take)


def create_test_set(src, dest):
    skip, take = 1000000, 200000
    create_set(src, dest, skip, take)


src = './data/polish_corpora.txt'
teaching_file = './data/teaching_set.txt'
test_file = './data/test_set.txt'

create_teaching_set(src, teaching_file)
create_test_set(src, test_file)


def bigram_to_unigram(bigram):
    unigram = defaultdict(lambda: 0, {})
    for word in bigram:
        for successor in bigram[word]:
            count = bigram[word][successor]
            unigram[word] += count
            unigram[successor] += count
    return unigram
