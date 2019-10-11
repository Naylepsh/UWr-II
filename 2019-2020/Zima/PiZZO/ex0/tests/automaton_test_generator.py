from automaton_generator import *
import sys


def generate_tests(automata, tests_filename, automata_filename, n_accepted, n_rejected, max_length):
	with open(tests_filename, 'w') as file:
		file.write(automata_filename+'\n')
		for word in automata.generate_accepted_words(n_accepted, 1, max_length):
			file.write(word+'\n')
		for word in automata.generate_rejected_words(n_rejected, 1, max_length):
			file.write(word+'\n')
		for _ in range(10**5):
			file.write('q')
		file.write('\n')

# init automata
alphabet_size = randint(1, len(FULL_ALPHABET))
num_of_states = randint(2, 10)
num_of_accepting = randint(1, num_of_states-1)
random_automata = AutomatonGenerator(alphabet_size, num_of_states, num_of_accepting)

# save automata to a file
automata_filename = 'automata.json'
with open(automata_filename, 'w') as file:
	file.write(json.dumps(random_automata.__dict__))

# generate tests
tests_filename = sys.argv[1]
accepted = int(sys.argv[2])
rejected = int(sys.argv[3])
generate_tests(random_automata, tests_filename, automata_filename, accepted, rejected, 100)
