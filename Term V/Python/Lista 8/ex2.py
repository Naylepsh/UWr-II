import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from random import random


class GameOfLife():

  def __init__(self, board_size):
    self.board_size = board_size
    self._ALIVE = 1
    self._DEAD = 0
    self._STATES = [self._ALIVE, self._DEAD]
    self._LIFE_CHANCE = 0.3
    self.board = np.random.choice(self._STATES, self.board_size ** 2, 
      p=[self._LIFE_CHANCE, 1 - self._LIFE_CHANCE]).reshape(self.board_size, self.board_size)
  
  def _alive_neighbours_count(self, x, y):
    alive = 0
    for x_off in [-1,0,1]:
      for y_off in [-1,0,1]:
        if x_off != 0 and x_off != 0:
          alive += self.board[(y+y_off)%self.board_size][(x+x_off)%self.board_size]
    return alive
  
  def simulate_turn(self, data):
    newBoard = self.board.copy()
    for y in range(self.board_size):
      for x in range(self.board_size):
        alive_neighbours = self._alive_neighbours_count(x, y)
        if self.board[y][x]:
          if (alive_neighbours < 2) or (alive_neighbours > 3):
            newBoard[y][x] = self._DEAD
        else:
          if alive_neighbours == 3:
            newBoard[y][x] = self._ALIVE
    mat.set_data(newBoard)
    self.board = newBoard


if __name__ == '__main__':
  game = GameOfLife(10)
  fig, ax = plt.subplots()
  mat = ax.matshow(game.board)
  ani = animation.FuncAnimation(fig, game.simulate_turn, interval=100)
  plt.show()