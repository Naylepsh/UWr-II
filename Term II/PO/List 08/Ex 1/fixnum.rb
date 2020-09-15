class Fixnum
  # class variable
  @@verbal_nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

  def factors
    facts = []
    for i in 1..self
      #facts.push(i) if self % i == 0
      facts << i if self % i == 0
    end
    facts
  end

  def ack(m)
    return m + 1 if self == 0
    return (self-1).ack(1) if m == 0
    (self-1).ack(self.ack(m-1))
  end

  def perfect?
    self == self.factors.sum - self # sum of all factors minus that number itself
  end

  def verbal
    if self == 0
      return @@verbal_nums[0]
    end
    @@verbal_nums[self % 10] + (if self / 10 == 0
                                ""
                              else
                                " " + (self / 10).verbal
                              end)
  end
end

puts "factors of 6 = #{6.factors}"
puts "factors of 12 = #{12.factors}"
puts "ack(2,1) = #{2.ack(1)}"
puts "is 6 perfect?: #{6.perfect?}"
puts "is 28 perfect?: #{28.perfect?}"
puts "is 42 perfect?: #{42.perfect?}"
puts "6 = #{6.verbal}"
puts "42 = #{42.verbal}"
puts "0 = #{0.verbal}"
puts "307 = #{307.verbal}"
