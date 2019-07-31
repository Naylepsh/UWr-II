require_relative "arith"

class Assign
  def initialize(symbol, val)
    @symbol = symbol
    @val = val
  end

  def eval(memory)
    memory[@symbol] = @val.eval(memory)
    memory
  end

  def to_s
    "#{@symbol} = #{@val}"
  end
end

class If
  def initialize(cond, if_true, if_false)
    @cond = cond
    @if_true = if_true
    @if_false = if_false
  end

  def eval(memory)
    if @cond.eval(memory) != 0
      @if_true.eval(memory)
    else
      @if_false.eval(memory)
    end
  end

  def to_s
    "if (#{@cond} != 0){\n(#{@if_true})\n(#{@if_false})}"
  end
end

class While
  def initialize(cond, body)
    @cond = cond
    @body = body
  end

  def eval(memory)
    while @cond.eval(memory) != 0
      memory = @body.eval(memory)
    end
    memory
  end

  def to_s
    "while (#{@cond})\n#{@body}"
  end
end

class Block
  def initialize(*instructions)
    @instructions = instructions
  end

  def eval(memory)
    @instructions.each do |instruction|
      memory = instruction.eval(memory)
    end
    memory
  end

  def to_s
    str = "{\n"
    @instructions.each do |instruction|
      str += instruction.to_s + "\n"
    end
    str + "}"
  end
end

class Evaluator
  def initialize(block)
    @block = block
  end

  def eval
    @block.eval({})
  end

  def to_s
    @block.to_s
  end
end

