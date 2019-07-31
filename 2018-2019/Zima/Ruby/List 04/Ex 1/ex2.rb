require_relative "my_lang"

puts Evaluator.new(Assign.new('x', Constant.new(5))).eval
puts Evaluator.new(Block.new(Assign.new('y', Constant.new(6)),
                             Assign.new('y', Constant.new(6)))).eval
puts Evaluator.new(Block.new(Assign.new('x', Constant.new(5)),
                             If.new(
                                 Subtract.new(
                                     Variable.new('x'),
                                     Constant.new(5)),
                                 Constant.new(1),
                                 Constant.new(0)))).eval
prog = Evaluator.new(Block.new(Assign.new('x', Constant.new(1)),
                             Assign.new('i', Constant.new(1)),
                             While.new(Subtract.new(Constant.new(6), # if we wanna know factor(n), here has to be n+1
                                                           Variable.new('i')),
                                       Block.new(Assign.new('x', Multiply.new(Variable.new('x'),
                                                                              Variable.new('i'))),
                                                 Assign.new('i', Add.new(Variable.new('i'),
                                                                         Constant.new(1)))))))
puts "factor(5)'s code':\n" + prog.to_s
puts "factor(5) = " + prog.eval.to_s