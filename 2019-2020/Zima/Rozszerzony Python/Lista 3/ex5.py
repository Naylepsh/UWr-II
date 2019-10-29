import itertools


def compress(text):
    groups = [list(group) for key, group in itertools.groupby(text)]
    return ''.join([f'{len(group)}{group[0]}' if len(group) > 1 else group[0] for group in groups])


def decompress(compressed):
    decompressed = ''
    num = 0
    for char in compressed:
        if char.isdigit():
            num = num*10 + int(char)
        else:
            decompressed += max(num,1) * char
            num = 0
    return decompressed

print(compress('suuuuper'))
print(decompress(compress('suuuuper')))
