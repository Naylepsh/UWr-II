def integral(a, b, &f)
  epsilon = 0.00001
  sum = 0
  while a < b
    sum += f.call(a) * epsilon
    a += epsilon
  end
  sum
end

def graph(a, b, &f)
  def divide_x(a, b, n)
    x_args = [a]
    dx = (b.to_f-a)/n
    (1..n).each {|m| x_args.push(a + m*dx)}
    x_args
  end

  def divide_y(vals, n)
    max_val = vals.max_by{|_key, value| value}
    min_val = vals.min_by{|_key, value| value}
    (max_val.to_f-min_val)/n
  end

  vals = {}
  divide(a,b,10).each {|x| vals[x] = f.call(x)}
  vals = vals.sort_by{ |_key, value| value}.reverse
  y_axis_sep = divide_y(vals, 10)

  until vals.empty?
    dx = (b.to_f-a)/n

  end

end

#puts integral(0.0,1.0) {|x| 2*x}
#graph(0,1) {|x| 2*x}

def rysuj(a,b,h,s)
  for i in (0..2*h).step s
    print (h-i).my_to_s
    for j in (a..b).step s
      if value(j) >= h-i and (value(j) < h-i+s or value(j-s) < h-i or value(j+s) < h-i)
        print "#"
      else
        print " "
      end
    end
    puts
    if i == h
      print "      "
      podzial(a,b,s)
      puts
    end
  end
end