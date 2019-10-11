import sys

filename = sys.argv[1]
accepted = int(sys.argv[2])
rejected = int(sys.argv[3])

with open(filename, 'r') as file:
	invalid_answers = 0
	for i in range(accepted):
		answer = file.readline()[:-1]
		if answer == 'no':
			print('fail at line', i)
			invalid_answers += 1
	for i in range(rejected):
		answer = file.readline()[:-1]
		if answer == 'yes':
			print('fail at line', i)
			invalid_answers += 1
if invalid_answers == 0:
	print('Succesfully passed the tests') 