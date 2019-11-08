#!/usr/bin/env python3
# coding: utf8


import itertools
import sys


def compress(text):
    """
    Text compression using Run-length encoding
    """
    groups = [list(group) for key, group in itertools.groupby(text)]
    return ''.join([f'{len(group)}{group[0]}' if len(group) > 1 else group[0] for group in groups])


def decompress(compressed):
    """
    Decompression of text encoded with Run-length encoding
    """
    decompressed = ''
    num = 0
    for char in compressed:
        if char.isdigit():
            num = num*10 + int(char)
        else:
            decompressed += max(num,1) * char
            num = 0
    return decompressed

if __name__ == '__main__':
    if len(sys.argv) > 1:
        text = sys.argv[1]
        compressed = compress(text)
        print('compressed: ', compressed)
        print('decompressed: ', decompress(compressed))
