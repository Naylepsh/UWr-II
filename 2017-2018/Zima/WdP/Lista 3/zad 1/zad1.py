from math import sqrt
def is_prime(n):
    for i in range(2, int(sqrt(n))):
        if (n % i) == 0:
            return False
    return True

counter = 0
for i in range(1, 100000):
    if (is_prime(i)):
        if (str(i).find('777') >= 0):
            counter += 1
            print(i)
print('There are ', counter, ' such numbers')