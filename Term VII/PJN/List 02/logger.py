class FileProcessLogger:
    def __init__(self, filename, percent_delta=10):
        self.total_lines = file_line_count(filename)
        self.lines_read = 0
        self.percent = 0
        self.percent_delta = percent_delta

    def update(self, lines_read=1):
        self.lines_read += lines_read
        x = self.lines_read / self.total_lines * 100
        if x > self.percent:
            self.percent = max(x, self.percent + self.percent_delta)
            self.log()

    def log(self):
        print(f'Processed {round(self.percent)}% of the file contents...')


def file_line_count(filename):
    lines = 0
    with open(filename, 'r', encoding='utf-8') as file:
        for _ in file:
            lines += 1
    return lines
