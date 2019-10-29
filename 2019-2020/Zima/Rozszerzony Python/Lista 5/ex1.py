import imperative, functional, comprehension, iterators
import sys, timeit

if len(sys.argv) < 2:
	print('Invalid number of arguments')
	exit()

ns = list(map(lambda n: int(n), sys.argv[1:]))

impr = []
func = []
comp = []
itr = []

for n in ns:
	impr.append(round(timeit.timeit(f'imperative.primes({n})', setup='import imperative', number=1), 4))
	comp.append(round(timeit.timeit(f'comprehension.primes({n})', setup='import comprehension', number=1), 4))
	itr.append(round(timeit.timeit(f'[prime for prime in iterators.primes({n})]', setup='import iterators', number=1), 4))
	func.append(round(timeit.timeit(f'functional.primes({n})', setup='import functional', number=1), 4))

top_row = ['', 'imperative', 'functional', 'comprehension', 'iterators']
items = (
	sys.argv[1:] + 
	list(map(lambda item: str(item), [impr[-1], func[-1], comp[-1], itr[-1]])) + 
	top_row)
col_len = len(max(items, key=len)) + 1

print('|'.join(map(lambda item: (col_len-len(item))*' ' + item, top_row)))

for i in range(len(ns)):
	row = list(map(lambda xs: str(xs[i]), [ns, impr, func, comp, itr]))
	print('|'.join(map(lambda item: (col_len-len(item))*' ' + item, row)))

