import turtle


def draw_carpet(tur, size, lvl, x, y):
    if lvl >= 0:
        for i in [-1,0,1]:
            for j in [-1,0,1]:
                if not (i == 0 and j == 0):
                    # move to new position
                    x1 = x + 3 * i * size
                    y1 = y + 3 * j * size
                    tur.penup()
                    tur.goto(x1, y1)
                    tur.pendown()
                    # draw new set of squares
                    draw_square(tur, size, x1, y1)
                    draw_carpet(tur, size / 3, lvl - 1, x1, y1)


def draw_square(tur, size, x, y):
    # move to starting position
    tur.penup()
    tur.goto(x + size / 2, y - size / 2)
    tur.pendown()

    # start drawing
    tur.fillcolor('black')
    tur.begin_fill()
    tur.goto(x - size / 2, y - size / 2)
    tur.goto(x - size / 2, y + size / 2)
    tur.goto(x + size / 2, y + size / 2)
    tur.goto(x + size / 2, y - size / 2)
    tur.end_fill()


def main():
    tur = turtle.Turtle()
    turtle.tracer(0,1)
    size = 180
    draw_square(tur, size, 0, 0)
    draw_carpet(tur, size / 3, 3, 0, 0)
    turtle.mainloop()


main()
