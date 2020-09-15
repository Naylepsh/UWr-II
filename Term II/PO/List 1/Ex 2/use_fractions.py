from fraction import *


def main():
    f1 = [1, 2]
    f2 = [2, 3]
    print("f1 now:", to_string(f1), "###", "f2 now:", to_string(f2))

    print("### First, all operations done with copying ###")

    f = addition_c(f1, f2)
    print(to_string(f1), "+", to_string(f2), "=", to_string(f))

    f = substraction_c(f1, f2)
    print(to_string(f1), "-", to_string(f2), "=", to_string(f))

    f = multiplication_c(f1, f2)
    print(to_string(f1), "*", to_string(f2), "=", to_string(f))

    f = division_c(f1, f2)
    print(to_string(f1), "/", to_string(f2), "=", to_string(f))


    print("### Second, all operations done with 2nd argument changes ###")

    print(to_string(f1), "+", to_string(f2), "= ", end="")
    addition_m(f1, f2)
    print(to_string(f2))

    print(to_string(f1), "-", to_string(f2), "= ", end="")
    substraction_m(f1, f2)
    print(to_string(f2))

    print(to_string(f1), "*", to_string(f2), "= ", end="")
    multiplication_m(f1, f2)
    print(to_string(f2))

    print(to_string(f1), "/", to_string(f2), "= ", end="")
    division_m(f1, f2)
    print(to_string(f2))

    # division by 0
    # f = [0, 1]
    # division_m(f1, f)


if __name__ == "__main__":
    main()
