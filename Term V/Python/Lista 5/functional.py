# coding: utf8


def primes(n):
	"""
	returns a list of prime numbers from 2 to a given integer
	"""
	return list(filter(lambda x: 0 not in map(lambda y: x % y, range(2, x)), range(2, n)))


def perfects(n):
	"""
	returns a list of perfect numbers from 2 to a given integer
	"""
	return list(filter(lambda x: x == sum(filter(lambda y: x % y == 0, range(1, x))), range(2, n)))
