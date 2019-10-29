def primes(n):
	xs = []
	for x in range(2, n+1):
		for y in range(2, x // 2+1):
			if x % y == 0:
				break
		else:
			xs.append(x)
	return xs


def perfects(n):
	xs = []
	for x in range(2, n+1):
		acc = 0
		for y in range(1, x // 2+1):
			if x % y == 0:
				acc += y
		if x == acc:
			xs.append(x)
	return xs
	