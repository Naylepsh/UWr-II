# coding: utf8


def primes(n):
	"""
	returns a list of prime numbers from 2 to a given integer
	"""
	return [x for x in range(2, n) if not 0 in (x % y for y in range(2,x))]


def perfects(n):
	"""
	returns a list of perfect numbers from 2 to a given integer
	"""
	return [x for x in range(2,n) if sum([y for y in range(1, x) if x % y == 0]) == x]
