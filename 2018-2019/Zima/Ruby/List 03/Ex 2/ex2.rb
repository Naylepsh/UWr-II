# a)
def primes_to(n)
  primes = []
  # for each element x from 2 to n, if it's NOT divisible by any
  # prime from 2 to x-1, then push it to tab
  (2..n).each {|x| primes.push(x) unless primes.any? {|divisor| x%divisor==0}}
  primes
end

# b)
def pow_of_2(n)
  2 ** n
end

def prime?(n)
  (2..n-1).each {|x| return false if n % x == 0}
  true
end

def turn_to_perfect(n)
  p = pow_of_2(n-1)
  (2*p - 1) * p
end

def perfect_numbers(n)
  # https://pl.wikipedia.org/wiki/Liczba_doskona%C5%82a
  perfect = []
  num_limit = (Math.log2(n)+2).floor
  primes_to(num_limit).each {|p| perfect.push(turn_to_perfect(p)) if prime?(pow_of_2(p) - 1)}
  perfect
end

#print primes_to(100)
print perfect_numbers(1000)