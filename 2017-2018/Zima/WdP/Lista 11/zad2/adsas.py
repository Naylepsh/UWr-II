print('zrestrukturyzowalibyście' == 'zrestrukturyzowalibyście')


file = open(r"C:\Users\naylepsh\Documents\Code\WdP\Lista 11\zad2\files\29.txt", encoding="utf8")
t = 0
for word in file:
    t += len(word) + 2
    for word2 in file:
        pass
    print(word[:-1])
    file.seek(t)
