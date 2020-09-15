def main():
    number = eval(input("Enter a number: "))
    p_factors = find_p_factors(number)
    print(p_factors)


def find_p_factors(num):
    temp = []
    while num > 1:
        for i in range(2, num + 1):
            if num % i == 0:
                if str(temp).find(str(i)) < 0:
                    temp.append(i)
                num = int(num / i)
                break
    return temp


main()