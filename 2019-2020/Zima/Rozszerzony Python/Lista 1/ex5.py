def factorize(n):
	assert n != 0, '???'
	n = abs(n)
	if n == 1:
		return [(1,1)]
	factors = []
	factor = 2
	i = 0
	while n > 1:
		if n % factor == 0:
			n /= factor
			i += 1
		else:
			if i > 0:
				factors.append((factor, i))
				i = 0
			factor += 1
	factors.append((factor, i))
	return factors

if __name__ == '__main__':
	print(factorize(756))
	print(factorize(27))
	print(factorize(1))
	print(factorize(-10))