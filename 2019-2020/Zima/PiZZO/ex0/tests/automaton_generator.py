import json
from random import randint, choice


FULL_ALPHABET = 'qwertyuiopasdfghjklzxcvbnm'


class AutomatonGenerator:
	
	def __init__(self, alphabet_size, num_of_states, num_of_accepting):
		self.alphabet = self.init_alphabet(alphabet_size)
		self.states = self.init_states(num_of_states)
		self.initial = self.states[0]
		self.accepting = self.init_accepting(num_of_accepting)
		self.transitions = self.init_transitions()

	def init_alphabet(self, size):
		assert size <= len(FULL_ALPHABET), 'alphabet size too big'
		return FULL_ALPHABET[:size]

	def init_states(self, n):
		assert n > 0, 'invalid number of states (below 1)'
		return ['q'+str(i) for i in range(n)]

	def init_accepting(self, n):
		# what will happen to implemented automata if none are accepting?
		assert n > -1, 'number of accepting states is too low'
		assert len(self.states) >= n, f'number of accepting({n}) is too big'
		states = self.states.copy()
		accepting = []
		while n > 0:
			state = choice(states)
			states.remove(state)
			accepting.append(state)
			n -= 1
		return accepting

	def init_transitions(self):
		transitions = []
		for state in self.states:
			for letter in self.alphabet:
				next_state = choice(self.states)
				transitions.append({'letter':letter, 'from':state, 'to':next_state})
		return transitions

	def generate_accepted_word(self, min_length):
		word = ''
		if len(self.accepting) > 0:
			state = choice(self.accepting)
			while not (len(word) >= min_length and state == self.initial):
				possible_routes = []
				for transition in self.transitions:
					if transition['to'] == state:
						possible_routes.append(transition)
				route = choice(possible_routes)
				word = route['letter'] + word
				state = route['from']
		return word

	def generate_rejected_word(self, min_length):
		word = ''
		not_accepted = [state for state in self.states if state not in self.accepting]
		if len(not_accepted) > 0:
			state = choice(not_accepted)
			while not (len(word) >= min_length and state == self.initial):
				possible_routes = []
				for transition in self.transitions:
					if transition['to'] == state:
						possible_routes.append(transition)
				route = choice(possible_routes)
				word = route['letter'] + word
				state = route['from']
		return word

	def generate_accepted_words(self, n, min_length, max_length):
		assert min_length <= max_length, 'max length is lesser than min length'
		return [self.generate_accepted_word(randint(min_length, max_length+1)) for _ in range(n)]

	def generate_rejected_words(self, n, min_length, max_length):
		assert min_length <= max_length, 'max length is lesser than min length'
		return [self.generate_rejected_word(randint(min_length, max_length+1)) for _ in range(n)]
