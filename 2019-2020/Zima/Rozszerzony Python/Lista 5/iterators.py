class Primes:
	def __init__(self, up_to):
		self.up_to = up_to
		self.x = 2

	def __iter__(self):
		return self


	def __next__(self):
		while True:
			if self.x > self.up_to:
				raise StopIteration
			for y in range(2, self.x):
				if self.x % y == 0:
					self.x += 1
					break
			else:
				self.x += 1
				return self.x - 1


def primes(up_to):
	x = 2
	while x < up_to:
		for y in range(2, x):
			if x % y == 0:
				x += 1
				break
		else:
			x += 1
			yield x - 1

