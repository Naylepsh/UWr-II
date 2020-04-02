def decomposition(n)
  factors = []
  divisor = 2
  while n > 1
    # if divisor is a factor
    if n % divisor == 0
      n /= divisor
      if factors.empty? or factors[-1][0] != divisor
        # add new prime factor
        factors.push([divisor, 1])
      else
        # increase the number of occurrences of currently used factor
        factors[-1][1] += 1
      end
    else
      divisor += 1
    end
  end
  factors
end

def factors_of(n)
  factors = []
  (1..(n/2).to_int+1).each {|x| factors.push(x) if n % x == 0}
  factors
end

def amicable_numbers(n)
  amics = []
  (1..n).each do |p|
    q = factors_of(p).sum
    amics.push([p,q]) if p == factors_of(q).sum and p != q and not amics.include?([q,p])
  end
  amics
end

print(decomposition(756))
puts
print(amicable_numbers(1300))
puts


