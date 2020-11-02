from naive_bayes import naive_bayes, AuthorshipClass, SentenceLengthRatio
from file_utils import count_words_in_file, get_words_from_file, get_sentence_length_gte_ratio

prus_file = './data/korpus_prusa.txt'
orzeszkowa_file = './data/korpus_orzeszkowej.txt'
sienkiewicz_file = './data/korpus_sienkiewicza.txt'

prus_words = count_words_in_file(prus_file)
orzeszkowa_words = count_words_in_file(orzeszkowa_file)
sienkiewicz_words = count_words_in_file(sienkiewicz_file)


sentence_gte_length = 20
prus_sentence_stats = get_sentence_length_gte_ratio(
    prus_file, sentence_gte_length)
orzeszkowa_sentence_stats = get_sentence_length_gte_ratio(
    orzeszkowa_file, sentence_gte_length)
sienkiewicz_sentence_stats = get_sentence_length_gte_ratio(
    sienkiewicz_file, sentence_gte_length)

prus = AuthorshipClass('p', prus_words, prus_sentence_stats)
orzeszkowa = AuthorshipClass('o', orzeszkowa_words, orzeszkowa_sentence_stats)
sienkiewicz = AuthorshipClass(
    's', sienkiewicz_words, sienkiewicz_sentence_stats)


def run_authorship_test(filenames, expected_symbol):
    passed = 0
    failed = 0
    for filename in filenames:
        try:
            test_words = get_words_from_file(filename)
            test_stats = get_sentence_length_gte_ratio(
                filename, sentence_gte_length)

            res, full_result = naive_bayes(
                test_words, [prus, orzeszkowa, sienkiewicz], test_stats)
            symbol = res[0]
            if (symbol == expected_symbol):
                passed += 1
            else:
                print(f'failed for {filename}, results: {full_result}')
                failed += 1
        except:
            pass
    print(f'passed {passed} / {passed + failed}')


def test_orzeszkowa():
    filenames = [
        f'./data/testy1/test_orzeszkowej{i}.txt' for i in range(1, 22)]
    run_authorship_test(filenames, orzeszkowa.symbol)


def test_prus():
    filenames = [f'./data/testy1/test_prusa{i}.txt' for i in range(1, 40)]
    run_authorship_test(filenames, prus.symbol)


def test_sienkiewicz():
    filenames = [
        f'./data/testy1/test_sienkiewicza{i}.txt' for i in range(1, 40)]
    run_authorship_test(filenames, sienkiewicz.symbol)


def print_test_msg(title):
    print(10*'=', title, 10*'=')


def run_tests():
    print_test_msg('ORZESZKOWA')
    test_orzeszkowa()
    print_test_msg('PRUS')
    test_prus()
    print_test_msg('SIENKIEWICZ')
    test_sienkiewicz()


run_tests()
