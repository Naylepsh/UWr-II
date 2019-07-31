from math import sqrt
import time

def is_prime(n):
    for i in range(2, int(sqrt(n))):
        if n % i == 0:
            return False
    return True

def is_lucky(n, ln, la):
    if str(n).find(la * ln) >= 0:
        return True
    return False

start = time.time()
digits = 10
lucky_num = '7'
lucky_amount = 7
for i in range(10 ** digits - 1, 10 ** (digits - 1) - 1, -1):
    # 10 ** digits - 1 is the highest number with 'digits' digits
    # 10 ** (digits - 1) - 1 is the number lesser than lowest number with 'digits' digits by 1
    if is_lucky(i, lucky_num, lucky_amount):
        if is_prime(i):
            print(i, ' is the highest hyperlucky number in this interval')
            break;
end = time.time()
print(end - start)