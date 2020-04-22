import alphabet


def cypher(string, key):
    all_letters = alphabet.polish_alphabet()
    encrypted = ""

    for char in string:
        # if it's not a letter, then don't encrypt it
        if char not in all_letters:
            encrypted += char
            continue
        new_value = (all_letters[char] + key) % alphabet.PA_SIZE
        for k in all_letters:
            if all_letters[k] == new_value:
                encrypted += k
                break
    return encrypted


def are_pair(string1, string2):
    s1_encryptions = []
    s2_encryptions = []

    # Assuming that shift in letter is different than 0
    for i in range(1, alphabet.PA_SIZE):
        s1_encryptions.append(cypher(string1, i))
        s2_encryptions.append(cypher(string2, i))

    for encr1 in s1_encryptions:
        if encr1 in s2_encryptions:
            return True
    return False
