# coding: utf8


def primes(up_to):
	"""
	generates primes from 2 to a given integer
	"""
	x = 2
	while x < up_to:
		for y in range(2, x):
			if x % y == 0:
				x += 1
				break
		else:
			x += 1
			yield x - 1

