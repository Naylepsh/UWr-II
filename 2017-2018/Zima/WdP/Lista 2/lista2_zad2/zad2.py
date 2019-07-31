from random import randint

#5 consecutive numbers, each bigger than previous one
def win_cond_1(list = []):
    l_size = len(list)
    size = 5 # size of a sequence
    if (l_size >= size):
        counter = 0
        for i in range(size - 1):
            if list[l_size - size + i + 1] > list[l_size - size + i]:
                counter += 1
        if counter == size - 1:
            return True
    return False

#6 consecutive numbers each not lesser than previous one
def win_cond_2(list = []):
    l_size = len(list)
    size = 6 # size of a sequence
    if (l_size >= 6):
        counter = 0
        for i in range(size - 1):
            if list[l_size - size + i + 1] >= list[l_size - size + i]:
                counter += 1
        if counter == size - 1:
            return True
    return False

print("Choose win condition:\n1) - 5 consecutive numbers each bigger than previous one")
print("2) - 6 consecutive numbers each not lesser than previous one")
option = int(input())
if (option != 1 and option != 2):
    print("Can you even read? :/")
    exit()

win_counter = 0
roll_counter = 0
ROLLS_PER_LOOP = 100
rolled = []
while (roll_counter < 100000 / ROLLS_PER_LOOP):
    if option == 1:
        for i in range(ROLLS_PER_LOOP):
            rolled.append(randint(1,6))
            if win_cond_1(rolled):
                win_counter += 1
                break
    else:
        for i in range(ROLLS_PER_LOOP):
            rolled.append(randint(1, 6))
            if win_cond_2(rolled):
                win_counter += 1
                break
    roll_counter += 1
    rolled.clear()  # or rolled[:] = []

print('Rolled ', ROLLS_PER_LOOP * roll_counter, ' times')
print('Probability of a winning roll: ', win_counter / roll_counter)