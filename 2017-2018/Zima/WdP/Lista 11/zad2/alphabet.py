PA_SIZE = len("aąbcćdeęfghijklłmnńoóprsśtuwyzźż")


def polish_alphabet():
    alphabet = {}
    value = 0
    for letter in "aąbcćdeęfghijklłmnńoóprsśtuwyzźż":
        alphabet[letter] = value
        value += 1
    return alphabet
