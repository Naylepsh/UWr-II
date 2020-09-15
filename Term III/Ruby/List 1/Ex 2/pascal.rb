def pascal(n)
  for i in 0...n
    pascal_row(i)
  end
end

def pascal_row(n)
  for i in 0...n+1
    print(newton(n, i), " ")
  end
  puts
end

def newton(a, b)
  factorial(a) / factorial(b) / factorial(a-b)
end

def factorial(n)
  if n == 0 || n == 1
    return 1
  end
  n*factorial(n-1)
end


pascal(3)
puts
pascal(4)
puts
pascal(5)
puts
pascal(7)