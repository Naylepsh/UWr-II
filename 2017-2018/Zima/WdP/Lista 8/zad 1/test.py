# Korpus Browna
chars = ',.?!'
brown = {}
for line in open('brown.txt', encoding="utf8"):
    line = line.split()
    for word in line:
        brown[word] = brown.get(word, 0) + 1

print(brown)