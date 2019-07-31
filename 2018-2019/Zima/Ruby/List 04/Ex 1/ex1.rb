#require_relative "arith"
require "./arith"

x = Add.new(Constant.new(5), Constant.new(6))
puts x
x = Add.new(Multiply.new(Constant.new(5), Variable.new('x')),
            Constant.new(0))
puts x
print "simplified: "
puts x.simplify
print "with x == 6: "
evals = {'x' => 6, 'y' => 7}
puts x.eval(evals)
x = Multiply.new(Constant.new(0), Variable.new('y')).simplify
puts x
x = Multiply.new(Constant.new(5), Divide.new(Variable.new('y'), Constant.new(5)))
puts x
print "simplified: "
puts x.simplify
x = Divide.new(Constant.new(5), Divide.new(Variable.new('y'), Constant.new(5)))
puts x
puts x.simplify