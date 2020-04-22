def cipher(string):
    val = 1
    values = {}
    for letter in string:
        if letter not in values:
            values[letter] = str(val)
            val += 1
    temp = ""
    for i in range(len(string)):
        if i == 0:
            temp += values[string[i]]
        else:
            temp += '-' + values[string[i]]
    return temp

print(cipher("tata"))
print(cipher("indianin"))