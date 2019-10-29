import comprehension
import functional
import imperative
from time import time

def timer(f):
	def wrapped(*args):
		start = time()
		f(*args)
		return time() - start
	return wrapped

time_primes_comp = timer(comprehension.primes)
time_primes_func = timer(functional.primes)
time_primes_imper = timer(imperative.primes)
time_perfects_comp = timer(comprehension.perfects)
time_perfects_func = timer(functional.perfects)
time_perfects_imper = timer(imperative.perfects)

n = 10000
print(f'n = {n}')
print('Primes compr time:', time_primes_comp(n))
print('Primes func time:', time_primes_func(n))
print('Primes imer time:', time_primes_imper(n))

n = 1000
print(f'n = {n}')
print('Perfects compr time:', time_perfects_comp(n))
print('Perfects func time:', time_perfects_func(n))
print('Perfects imper time:', time_perfects_imper(n))
