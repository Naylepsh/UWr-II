def dividers(x)
  # returns an array of dividers of x NOT including 1
  x = x.abs
  divs = []
  div = 2
  while x > 1
    if x % div == 0
      x /= div
      divs.push(div) unless divs.include?(div)
    else; div += 1
    end
  end
  divs
end

print(dividers(42))
puts
print(dividers(1025))
puts
print(dividers(2))
puts
print(dividers(1))
puts
print(dividers(-10))
puts