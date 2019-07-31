require './search'

def put_indexed(indexed)
  indexed.each do |url, indexes|
    puts url
    indexes.each do |pair|
      puts "\t" + pair[0] + " - " + pair[1].to_s
    end
  end
end

sites = %w(https://www.ii.uni.wroc.pl/~marcinm/dyd/ruby/
         https://zapisy.ii.uni.wroc.pl/
         http://pwr.edu.pl/
         https://ultima.pl/
         https://www.ppe.pl/)

test = Search::Searcher.new
x = test.index(sites[0], 1)
put_indexed x
puts test.search(/ruby/)
#x = test.index('https://en.wikipedia.org/wiki/Data_URI_scheme', 0)
#put_indexed x