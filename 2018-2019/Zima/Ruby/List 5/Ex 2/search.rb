module Search
  require 'open-uri'
  require 'uri'
  require 'nokogiri'

  class Searcher
    def initialize
    end

    def count_words(site)
      words = {}

      text = Nokogiri::HTML.parse(open(URI.encode(site))).text || '' rescue '' # returns just the text -- no html code

      text.split("\n").each do |line|
        line.gsub(/[\n,.;=!?'{}()\[\]"\-\/\\:]/, '').split(' ').each do |word|
          next if word.to_i.to_s == word
          words[word] = 0 unless words[word]
          words[word]+= 1
        end
      end
      words.sort_by { |word, count| -count }
    end

    def find_urls(url, depth)
      if depth > 0
        html = get_html(url)
        refs = find_refs(html)
        refs.each do |ref|
          unless @urls.include? ref
            @urls.push(ref)
            find_urls(ref, depth - 1)
          end
        end
      end
    end

    def get_html(url)
      open(url){|f|f.read}#  || '' rescue ''
    end

    def find_refs(html)
      refs = html.scan(/href="https?:\/\/[a-zA-Z0-9\-_.\/]+"/)
      for i in 0..refs.length-1
        refs[i] = refs[i][6..refs[i].length-2]
      end
      refs
    end

    def index(start_page, depth)
      # gets all urls
      @urls = [start_page]
      find_urls(start_page, depth)
      # for each url index its words
      indexed = []
      @urls.each do |url|
        puts url
        indexed.push([url, count_words(url)])
      end
      indexed
    end

    def search(re)
      urls = []
      @urls.each do |site|
        if open(site).read =~ re
          urls.push(site)
        end
      end
      urls
    end
  end
end