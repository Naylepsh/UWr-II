from duze_cyfry import dajCyfre
from turtle import *
from random import random
### CONSTANTS ###
sq_size = 10


def get_dlc(number):
    t_list = []
    for i in range(len(number)):
        t_list.append(dajCyfre(int(number[i])))
    return t_list


def draw_dlc(t_list, tur):
    for number in t_list:
        r = random()
        b = random()
        g = random()
        tur.fillcolor(r, b, g)
        for line in number:
            for char in line:
                if char == "#":
                    tur.begin_fill()
                    draw_square(tur)
                    tur.end_fill()
                    offset_x(tur, sq_size)
                else:
                    offset_x(tur, sq_size)
            correct_pos(tur, -5 * sq_size, sq_size)
        correct_pos(tur, 6 * sq_size, -5 * sq_size)


def draw_square(tur):
    for i in range(4):
        tur.forward(sq_size)
        tur.right(90)


def offset_x(tur, x):
    tur.penup()
    tur.forward(x)
    tur.pendown()


def offset_y(tur, y):
    tur.right(90)
    offset_x(tur, y)


def correct_pos(tur, x, y):
    offset_x(tur, x)
    offset_y(tur, y)
    tur.left(90)


def main():
    number = "1234567890"
    tur = Turtle()
    tur.speed("fastest")
    correct_pos(tur, -300, 0)
    draw_dlc(get_dlc(number), tur)
    mainloop()


main()