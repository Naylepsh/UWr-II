from turtle import *
from random import choice

wn = Screen()
tur = Turtle()

def draw_square(size):
    tur.begin_fill()
    for i in range(4):
        tur.forward(size)
        tur.right(90)
    tur.end_fill()


def offset_x(x):
    tur.penup()
    tur.forward(x)
    tur.pendown()


def offset_y(y):
    tur.penup()
    tur.right(90)
    tur.forward(y)
    tur.left(90)
    tur.pendown()


def offset(x, y):
    tur.penup()
    tur.goto(x, y)
    tur.pendown()


def new_line(n, size):
    offset_x(-n * size)
    offset_y(size)


def main():
    bs_size = 10    # big square size
    ss_size = 30    # small square size
    tur.speed("fastest")
    offset(-100, 100)
    colors = ['red', 'blue', 'purple', 'yellow', 'black', 'pink', 'white', 'green', 'gray', 'magenta', 'cyan', 'orange',
              'indigo']

    for y in range(bs_size):
        for x in range(bs_size):
            tur.fillcolor(choice(colors))
            draw_square(ss_size)
            offset_x(ss_size)
        new_line(bs_size, ss_size)

    mainloop()


main()