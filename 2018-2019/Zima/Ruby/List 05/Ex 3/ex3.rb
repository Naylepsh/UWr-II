require "open-uri"

class Dist
  @page_b = ""
  @start_time = 0

  def initialize
    @pages_visited = []
  end

  def distance(page_a, page_b)
    @page_b = page_b
    @start_time = DateTime.now
    @max_time_limit = 5 # == 1 second
    dist(page_a, 0, @max_time_limit)
  end

  def dist(current_page, n, time_limit)
    level_start_time = DateTime.now
    if current_page == @page_b
      n
    else
      begin
        @pages_visited.push(current_page)
        puts current_page
        html = get_html(current_page) # prone to errors, hence why 'begin' and 'rescue'
        refs = find_refs(html)
        refs.each do |ref|
          now = DateTime.now
          if ((now - level_start_time) * 24 * 60 * 60 > time_limit) ||
              ((now - @start_time) * 24 * 60 * 60 > @max_time_limit)
            # subtracting two DateTimes returns the elapsed time in days hence why 24 * ...
            return -1
          end
          unless @pages_visited.include? ref
            @pages_visited.push(ref)
            x = dist(ref, n+1, time_limit / refs.length)
            return x if x >= 0 # if x is greater than -1 then we've found the dist, so let's stop here
            end
          end
        -1
      rescue OpenURI::HTTPError => e
        # due to pretty much an unlimited number of possible errors when web scrapping
        # im gonna just return nil every time.
        # not like we're gonna find desired page through a page that keeps spamming errors, no?
        #if e.message == '404 Not Found'
        #  return -1
        #elsif e.message == '403 Forbidden'
        #  return -1
        -1
      rescue OpenURI::HTTPRedirect => redirect
        -1
        end
    end
  end

  def get_html(url)
    open(url){|f|f.read}
  end

  def find_refs(html)
    refs = html.scan(/href="https?:\/\/[a-zA-Z0-9\-_.\/]+"/)
    for i in 0..refs.length-1
      refs[i] = refs[i][6..refs[i].length-2]
    end
    refs
  end
end


link1 = "https://en.wikipedia.org/wiki/Data_URI_scheme"
link2 = "http://upload.wikimedia.org/wikipedia/commons/d/d3/35_mm_angle_of_view_vs_focal_length.svg"
link3 = "https://www.google.com/"
x = Dist.new
puts x.distance(link1, link2)