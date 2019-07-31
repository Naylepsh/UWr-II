import math


def factorial(n):
    if (n <= 0):
        print("Błąd.", n, "jest mniejsze od 1.")
        return

    result = 1
    for i in range(1,n + 1):
        result *= i
    digits = int(math.log(result,10) + 1)

    if (digits == 1):
        text = " cyfrę."
    elif (1 < digits < 5 or (digits > 20 and 1 < digits % 10 < 5)):
        text = " cyfry."
    else:
        text = " cyfr."

    print(n, "! ma ", digits, text, sep="")

for i in range(4,101):
    factorial(i)