require 'open-uri'
require 'uri'
require 'nokogiri'

class Searcher
  def initialize(list_of_sites)
    @list_of_sites = list_of_sites
  end

  def count_words(site)
    words = {}

    text = Nokogiri::HTML.parse(open(URI.encode(site))).text || '' rescue '' # returns just the text -- no html code
    #puts text

    text.split("\n").each do |line|
      line.gsub(/[\n,.!?'{}()\[\]"\-\/\\:]/, '').split(' ').each do |word|
        next if word.to_i.to_s == word
        words[word] = 0 unless words[word]
        words[word]+= 1
      end
    end
    words.sort_by { |word, count| -count }
  end

  def link(site)
    Nokogiri::HTML.parse(open(URI.encode(site))).xpath("//a").map do |tag|
      puts tag
      URI.join(site, tag['href']).to_s
    end rescue nil
  end

  def index(start_page, depth)
    result = [[start_page]]
    depth.times do |level|
      result.push(result.last.map{|site| link(site)}.flatten.compact )
    end

    result = result.map{|level| level.join(", ") + '- ' + level.map{|site| count_words(site).join(': ')}.join(', ')}.join("\n\n\n")
    puts result
  end

  def search(reg_exp)
    tab = []
    @list_of_sites.each do |site|
      if open(site).read =~ reg_exp
        tab << site
      end
    end
    puts tab
  end
end