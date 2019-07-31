from duze_cyfry import dajCyfre

def print_DLC(number):
    list = []
    for i in range(len(number)):
        list.append(dajCyfre(int(number[i])))

    for i in range(len(list[0])):   # or just 5 instead of len(list[0])
        for j in range(len(list)):
            print(list[j][i], end=' ')
        print()

number = input('Enter your number: ')
print_DLC(number)
