#!/usr/bin/env python3
# coding: utf8
"""
Tests imperative vs comprehension vs functional modules for given integers on input
"""

import sys
import timeit


if __name__ == '__main__':
	if len(sys.argv) > 1:
		ns = list(map(lambda x: int(x), sys.argv[1:]))

	for n in ns:
		print(f'imperative.primes({n}) in:',
					round(timeit.timeit(f'imperative.primes({n})', setup='import imperative', number=1), 4))
		print(f'comprehension.primes({n}) in:',
					round(timeit.timeit(f'comprehension.primes({n})', setup='import comprehension', number=1), 4))
		print(f'functional.primes({n}) in:',
					round(timeit.timeit(f'functional.primes({n})', setup='import functional', number=1), 4))
		print()
		
	print(10*'#'+'\n')

	for n in ns:
		print(f'imperative.primes({n}) in:',
					round(timeit.timeit(f'imperative.perfects({n})', setup='import imperative', number=1), 4))
		print(f'comprehension.primes({n}) in:',
					round(timeit.timeit(f'comprehension.perfects({n})', setup='import comprehension', number=1), 4))
		print(f'functional.primes({n}) in:',
					round(timeit.timeit(f'functional.perfects({n})', setup='import functional', number=1), 4))
		print()
