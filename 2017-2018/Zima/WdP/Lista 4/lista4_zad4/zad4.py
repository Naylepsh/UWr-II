from random import choice


def randperm(n):
    temp = []
    for i in range(n):
        temp.append(i)

    randomized = []
    for i in range(n):
        number = choice(temp)
        randomized.append(number)
        temp.remove(number)

    return randomized


def main():
    for i in range(3, 10):
        print(randperm(i))


main()
