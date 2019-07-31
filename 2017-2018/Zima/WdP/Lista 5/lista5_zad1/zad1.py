from statistics import median

def F(n):
    if n % 2 == 0:
        return n / 2
    else:
        return 3 * n + 1


def getEnergy(n):
    energy = 0
    while n != 1:
        energy += 1
        n = int(F(n))
    return energy


def analizaCollatza(a, b):
    energies = []
    for n in range(a, b + 1):
        energies.append(getEnergy(n))
    print(energies)
    print("Collatz analisys:")
    print("Average = {0:>5.2f}".format(sum(energies) / len(energies)))
    print("Median {0:>2} {1}".format("=", int(median(energies))))
    print("Max {0:>5} {1}".format("=", max(energies)))
    print("Min {0:>5} {1}".format("=", min(energies)))


def main():
    a, b = eval(input("Enter 2 values seperated by a comma: "))
    analizaCollatza(a, b)


if __name__ == "__main__":
    main()