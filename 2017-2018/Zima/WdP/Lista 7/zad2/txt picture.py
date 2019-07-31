from turtle import *
# CONSTANTS
size = 10


def main():
    tur, x, y = turtle_init()
    fname = "obrazek tekstowy.txt"
    infile = open(fname, "r")
    for line in infile:
        draw_col(line.split(), tur)
        x += size
        newline(tur, x, y)
    mainloop()


def turtle_init():
    tur = Turtle()
    colormode(255)
    tur._tracer(0, 1)
    tur.penup()
    x, y = -300, 250
    tur.goto(x, y)
    tur.pendown()
    return tur, x, y


def draw_col(colors, tur):
    for pixel_color in colors:
        draw_square(pixel_color, tur)
        offset_y(tur)


def draw_square(color, tur):
    color = eval(color)
    tur.fillcolor(color[0], color[1], color[2])
    tur.begin_fill()
    for i in range(4):
        tur.forward(size)
        tur.right(90)
    tur.end_fill()


def offset_y(tur):
    tur.penup()
    tur.right(90)
    tur.forward(size)
    tur.left(90)
    tur.pendown()


def newline(tur, x, y):
    tur.penup()
    tur.goto(x, y)
    tur.pendown()


main()
