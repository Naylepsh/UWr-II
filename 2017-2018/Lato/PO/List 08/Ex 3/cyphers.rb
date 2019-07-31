class Cypher
  def initialize(string)
    @string = string
  end

  def cypher(key)
    cyphered = ""
    for letter in @string.split('')
      if key.include? letter
        cyphered += key[letter]
      else
        cyphered += letter
      end
    end
    Decypher.new(cyphered)
  end

  def to_s
    @string
  end
end

class Decypher
  def initialize(string)
    @string = string
  end

  def decypher(key)
    decyphered = ""
    inverted = key.invert
    for letter in @string.split('')
      if inverted.include? letter
        decyphered += inverted[letter]
      else
        decyphered += letter
      end
    end
    Cypher.new(decyphered)
  end

  def to_s
    @string
  end
end


some_key = {"a"=>"c", "b"=>"d", "c"=>"e", "d"=>"f", "e"=>"g", "f"=>"h", "g"=>"i", "h"=>"j", "i"=>"k",
    "j"=>"l", "k"=>"m", "l"=>"n", "m"=>"o", "n"=>"p", "o"=>"r", "p"=>"s", "r"=>"t", "s"=>"u", "t"=>"v",
    "u"=>"w", "v"=>"x", "w"=>"y", "x"=>"z", "y"=>"a", "z"=>"b"}
cyph = Cypher.new("hello world!").cypher(some_key)
puts cyph
decyph = cyph.decypher(some_key)
puts decyph