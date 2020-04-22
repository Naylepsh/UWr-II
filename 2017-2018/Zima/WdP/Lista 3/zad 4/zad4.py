import turtle
from random import randint
wn = turtle.Screen()
tur = turtle.Turtle()

def offset_x(x):
    tur.penup()
    tur.forward(x)
    tur.pendown()

def rectangle(a, b):
    for i in range(2):
        tur.forward(a)
        tur.left(90)
        tur.forward(b)
        tur.left(90)
    offset_x(a + a / 5)

tur.speed(10)
offset_x(-100)
a = 10      # horizontal length
b = 5 * a   # vertical length
n = 25      # number of rectangles
for i in range(n):
    b += randint(0,5)
    rectangle(a, b)

turtle.mainloop()
