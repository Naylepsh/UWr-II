import tkinter
from time import time

size = 15
sq = 30


def main():
    # make a graphic interface window
    win = tkinter.Canvas(tkinter.Tk(), width=size*2*sq, height=size*sq)
    win.pack()
    # initialize map with cells of both teams
    cells = init_cells("czerwony.txt", "niebieski.txt")
    history = []
    # simulate a game while it's state is 'unique'
    while cells not in history:
        run(cells, history, win)
        win.update()
    print("Score (Red , Blue) :", get_score(cells))
    tkinter.mainloop()


def init_cells(fname1, fname2):
    reds = open(fname1)
    blues = open(fname2)
    cells = []
    for y in range(size):
        red_part = []
        for cell in reds.readline().replace('#', 'r').strip('\n'):
            red_part.append(cell)
        blue_part = []
        for cell in blues.readline().replace('#', 'b').strip('\n'):
            blue_part.append(cell)
        cells.append(red_part+blue_part)
    return cells


def run(cells, history, win):
    # clean canvas
    win.delete(tkinter.ALL)
    copy = make_copy(cells)
    for y in range(len(cells)):
        for x in range(len(cells[y])):
            # check its neighbours
            red, blue = get_neighbours(y, x, cells)
            nb = red + blue
            # change its status if necessery
            # if not enough or too many neighbours for alive cell
            if (copy[y][x] == 'r' or copy[y][x] == 'b') and not (2 <= nb <= 3):
                cells[y][x] = '.'
            elif copy[y][x] == 'r':
                win.create_rectangle(x * sq, y * sq, (x + 1) * sq, (y + 1) * sq, fill="red")
            elif copy[y][x] == 'b':
                win.create_rectangle(x * sq, y * sq, (x + 1) * sq, (y + 1) * sq, fill="blue")
            # if cell is dead but has 3 neighbours
            elif copy[y][x] == '.' and nb == 3:
                if red > blue:
                    cells[y][x] = 'r'
                    win.create_rectangle(x * sq, y * sq, (x + 1) * sq, (y + 1) * sq, fill="red")
                else:
                    cells[y][x] = 'b'
                    win.create_rectangle(x * sq, y * sq, (x + 1) * sq, (y + 1) * sq, fill="blue")
    # add to history
    history.append(copy)
    #show(cells)
    t = time()
    while time() - t < 0.05:
        continue


def make_copy(container):
    copy = []
    for col in container:
        temp = []
        for cell in col:
            temp.append(cell)
        copy.append(temp)
    return copy


def get_neighbours(y, x, cells):
    red = blue = 0
    for off_y in [-1,0,1]:
        for off_x in [-1,0,1]:
            if not (off_y == 0 and off_x == 0):
                ny = (y + off_y) % size
                nx = (x + off_x) % (2 * size)
                if cells[ny][nx] == 'r':
                    red += 1
                elif cells[ny][nx] == 'b':
                    blue += 1
    return red, blue


def show(cells):
    for row in cells:
        for cell in row:
            print(cell, end='')
        print()
    print()


def get_score(cells):
    score_r = score_b = 0
    for row in cells:
        for cell in row:
            if cell == 'r':
                score_r += 1
            elif cell == 'b':
                score_b += 1
    return score_r, score_b


if __name__ == "__main__":
    main()
