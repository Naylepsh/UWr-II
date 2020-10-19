from logger import FileProcessLogger, file_line_count


def add_successor(successors, n, word, successor):
    successor = (n, successor)
    if word in successors:
        successors[word].append(successor)
    else:
        successors[word] = [successor]


def gather_successors(filename, process_line, K=1):
    logger = FileProcessLogger(filename)

    successors = {}
    with open(filename, 'r', encoding='utf-8') as file:
        for line in file:
            logger.update()

            line = line.rstrip('\n')
            res = process_line(line)
            if len(res) != 3:
                continue
            n, keyword, successor = res
            if n >= K:
                add_successor(successors, n, keyword, successor)

    return successors


def gather_successors_from_bigrams(filename, K=1):
    def process_line(line):
        line = line.rstrip('\n').split(' ')
        if (len(line) != 3):
            return

        n, word1, word2 = line
        return int(n), (word1, ), word2

    return gather_successors(filename, process_line, K)


def gather_successors_from_trigrams(filename, K=1):
    def process_line(line):
        line = line.rstrip('\n').split(' ')
        if (len(line) != 4):
            return []

        n, word1, word2, word3 = line
        return int(n), (word1, word2), word3

    return gather_successors(filename, process_line, K)


def gather_words_from_file(filename, lines_to_read):
    words = {}
    logger = FileProcessLogger(filename)
    logger.total_lines = min(logger.total_lines, lines_to_read)
    with open(filename, encoding='utf-8') as f:
        for _ in range(lines_to_read):
            logger.update()
            words_in_line = next(f).rstrip('\n').split(' ')
            for word in words_in_line:
                words[word.lower()] = True
    return words
