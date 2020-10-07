import sys
import math
from random import randint
from time import time


class TicTacToeSimulator:
    PLAYER = 'p'
    OPPONENT = 'o'
    WIN = 1
    DRAW = 0
    LOSE = -1

    def __init__(self, player_positions, opponent_positions, free_positions):
        self.free_positions = free_positions
        self.positions = [['' for _ in range(3)] for _ in range(3)]

        for row, col in player_positions:
            self.positions[row][col] = self.OPPONENT

        for row, col in opponent_positions:
            self.positions[row][col] = self.PLAYER

    def simulate(self):
        while len(self.free_positions) > 0:
            self.simulate_player_turn()
            if (self.get_winner()):
                return self.WIN

            if (len(self.free_positions) == 0):
                return self.DRAW

            self.simulate_opponent_turn()
            if (self.get_winner()):
                return self.LOSE

        return self.DRAW

    def simulate_player_turn(self):
        row, col = self.choose_next_position()
        self.positions[row][col] = self.OPPONENT

    def simulate_opponent_turn(self):
        row, col = self.choose_next_position()
        self.positions[row][col] = self.PLAYER

    def choose_next_position(self):
        i = randint(0, len(self.free_positions) - 1)
        row, col = self.free_positions[i]
        self.free_positions.pop(i)
        return row, col

    def get_winner(self):
        winner_checks = [self.get_horizontal_winner,
                         self.get_vertical_winner,
                         self.get_diagonal_winner]
        for winner_check in winner_checks:
            winner = winner_check()
            if winner:
                return winner

    def get_horizontal_winner(self):
        for row in self.positions:
            cell1, cell2, cell3 = row
            if cell1 == cell2 == cell3 and cell1 is not None:
                return cell1
        return None

    def get_vertical_winner(self):
        for i in range(3):
            cell1, cell2, cell3 = map(lambda row: row[i], self.positions)
            if cell1 == cell2 == cell3 and cell1 is not None:
                return cell1
        return None

    def get_diagonal_winner(self):
        cell1, cell2, cell3 = [self.positions[i][i] for i in range(3)]
        if cell1 == cell2 == cell3 and cell1 is not None:
            return cell1

        cell1 = self.positions[0][2]
        cell2 = self.positions[1][1]
        cell3 = self.positions[2][0]
        if cell1 == cell2 == cell3 and cell1 is not None:
            return cell1

        return None


class State:
    def __init__(self, x, y):
        self.visit_count = 0
        self.win_score = 0
        self.x = x
        self.y = y

    def get_score(self):
        return self.win_score / self.visit_count if self.visit_count > 0 else 0


class FlatMonteCarlo:
    def __init__(self, states, available_positions):
        self.states = states
        self.available_positions = available_positions

    def run(self, start_time):
        # counter = 0
        while 1000*(time() - start_time) < 95:
            i = randint(0, len(self.available_positions) - 1)
            row, col = self.available_positions[i]

            updated_player_positions = player_positions + \
                [self.available_positions[i]]
            updated_available_positions = list(
                filter(lambda pos: pos[0] != row or pos[1] != col, self.available_positions))

            simulator = TicTacToeSimulator(
                updated_player_positions, opponent_positions, updated_available_positions)
            res = simulator.simulate()
            self.states[i].visit_count += 1
            self.states[i].win_score += res
            # counter += 1

        # print(counter, file=sys.stderr, flush=True)

        currentBest = self.states[0]
        for state in self.states:
            if state.get_score() > currentBest.get_score():
                currentBest = state

        return currentBest


opponent_positions = []
player_positions = []

# game loop
while True:
    start = time()
    opponent_row, opponent_col = [int(i) for i in input().split()]
    opponent_positions.append((opponent_row, opponent_col))

    valid_action_count = int(input())
    available_positions = []
    for i in range(valid_action_count):
        row, col = [int(j) for j in input().split()]
        available_positions.append((row, col))

    states = [State(x, y) for x, y in available_positions]
    monte_carlo = FlatMonteCarlo(states, available_positions)
    best_action = monte_carlo.run(start)

    print(best_action.x, best_action.y)
