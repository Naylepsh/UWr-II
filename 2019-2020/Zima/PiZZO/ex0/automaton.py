import json
import sys


class State:

	def __init__(self, name, is_accepting):
		self.name = name
		self.is_accepting = is_accepting
		self.transitions = {}

	def add_transition(self, letter, next_state):
		self.transitions[letter] = next_state

	def run(self, letter):
		return self.transitions[letter]

	# helper -- delete before sending the file
	def __str__(self):
		s = f'is_accepting?: {self.is_accepting}\ntransitions:\n'
		for letter in self.transitions:
			s += f'\t{letter}:{self.transitions[letter].name}\n'
		return s


class Automaton:

	def __init__(self, filename):
		self.states = {}
		with open(filename, 'r') as file:
			data = json.load(file)
			self.load_states(data['states'], data['accepting'])
			self.set_transitions(data['transitions'])
			self.initial_state = self.states[data['initial']]

	def load_states(self, states, accepting_states):
		for state_name in states:
			self.states[state_name] = State(state_name, state_name in accepting_states)


	def set_transitions(self, transitions):
		for transition in transitions:
			previous_state_name = transition['from']
			next_state = self.states[transition['to']]
			letter = transition['letter']
			self.states[previous_state_name].add_transition(letter, next_state)

	# helper -- delete before sending the file
	def __str__(self):
		return '\n\n'.join(f'name: {name}\n{self.states[name]}' for name in self.states)


if __name__ == '__main__':
	filename = sys.stdin.readline()[:-1]
	automata = Automaton(filename)
	state = automata.initial_state
	while True:
		char = sys.stdin.read(1)
		if not char:
			break
		if char == '\n' or char == '\r':
			if state.is_accepting:
				print('yes')
			else:
				print('no')
			state = automata.initial_state
		else:
			state = state.run(char)