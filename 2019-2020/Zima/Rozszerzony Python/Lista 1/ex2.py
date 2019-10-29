def foo(x, coins):
	"""Asssumes that coins make a canonical coin system"""
	if x < 0 or not coins:
		return []
	n = x // coins[-1]
	return [(coins[-1], n)] + foo(x-coins[-1]*n, coins[:-1])

coins = [1,2,5,10,20]
print(foo(123, coins))