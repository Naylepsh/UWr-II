class Constant
  def initialize(value)
    @val = value
  end

  def to_s
    @val.to_s
  end

  def eval(evaluations)
    @val
  end

  def simplify
    self
  end
end

class Variable
  def initialize(symbol)
    @symbol = symbol
  end

  def to_s
    @symbol
  end

  def eval(evaluations)
    evaluations[@symbol]
  end

  def simplify
    self
  end
end

class Operator
  attr_reader :left
  attr_reader :right
  def initialize(left, right)
    @left = left
    @right = right
  end
end

class Add < Operator
  def to_s
    "(#{@left.to_s} + #{@right.to_s})"
  end

  def eval(evaluations)
    @left.eval(evaluations) + @right.eval(evaluations)
  end

  def simplify
    simple_left = @left.simplify
    simple_right = @right.simplify
    if '0' == simple_left.to_s
      simple_expr = simple_right
    elsif '0' == simple_right.to_s
      simple_expr = simple_left
    else
      simple_expr = Add.new(simple_left, simple_right)
    end
    simple_expr
  end
end

class Subtract < Operator
  def to_s
    "(#{@left.to_s} - #{@right.to_s})"
  end

  def eval(evaluations)
    @left.eval(evaluations) - @right.eval(evaluations)
  end

  def simplify
    simple_left = @left.simplify
    simple_right = @right.simplify
    if '0' == simple_left.to_s
      simple_expr = simple_right
    elsif '0' == simple_right.to_s
      simple_expr = simple_left
    else
      simple_expr = Subtract.new(simple_left, simple_right)
    end
    simple_expr
  end
end

class Multiply < Operator
  def to_s
    "(#{@left.to_s} * #{@right.to_s})"
  end

  def eval(evaluations)
    @left.eval(evaluations) * @right.eval(evaluations)
  end

  def simplify
    simple_left = @left.simplify
    simple_right = @right.simplify
    if '0' == simple_left.to_s or '0' == simple_right.to_s
      simple_expr = Constant.new(0)
    elsif simple_right.instance_of? Divide and simple_left.to_s == simple_right.right.to_s
      simple_expr = simple_right.left
    elsif simple_left.instance_of? Divide and simple_right.to_s == simple_left.right.to_s
      simple_expr = simple_left
    else
      simple_expr = Multiply.new(simple_left, simple_right)
    end
    simple_expr
  end
end

class Divide < Operator
  def to_s
    "(#{@left.to_s} / #{@right.to_s})"
  end

  def eval(evaluations)
    @left.eval(evaluations) / @right.eval(evaluations)
  end

  def simplify
    simple_left = @left.simplify
    simple_right = @right.simplify
    if '0' == simple_left
      simple_expr = Constant.new(0)
    elsif simple_right.instance_of? Divide
      simple_expr = Divide.new(Multiply.new(simple_left, simple_right.right), simple_right.left).simplify
    elsif simple_left.instance_of? Divide
      simple_expr = Divide.new(simple_left.left, Multiply.new(simple_left.right, simple_right))
    else
      simple_expr = Divide.new(simple_left, simple_right)
    end
    simple_expr
  end
end

