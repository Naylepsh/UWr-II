def word_stats(filename)
  words = Hash.new(0.0)
  word_counter = 0.0
  file = File.open(filename, 'r')
  #text = text.gsub!(/[^A-Za-z ąęóżźćńł]/, '')
  all_words = file.read().gsub!(/[,.;'"!?\/()]/, '').downcase.split

  # sum all words' appearances
  all_words.each {| word | words[word] += 1; word_counter += 1 }

  # turn each word's appearance number into it's frequency in percentages
  words_freq = Hash.new(0)
  words.keys.each {| word | words_freq[word] = words[word] * 100.0 / word_counter}

  words_freq.sort_by {|_key, value| value}
  #words
end

def print_array(arr)
  for elem in arr
    puts(elem[0] + " " + elem[1].to_s)
  end
end

fn = gets.chomp
#word_stats(fn)
#print_dict(word_stats(fn))
print_array(word_stats(fn))