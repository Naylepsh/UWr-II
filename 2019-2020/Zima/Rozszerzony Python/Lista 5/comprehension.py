def primes(n):
	return [x for x in range(2, n) if not 0 in [x % y for y in range(2,x)]]


def perfects(n):
	return [x for x in range(2,n) if sum([y for y in range(1, x) if x % y == 0]) == x]
