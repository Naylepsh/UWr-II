require "dbm"

class Album
	def initialize(name, tracks, artist, lent_to, days_away)
		@name = name
		@id = rand(100)
		@tracks = tracks
		@artist = artist
		@lent_to = lent_to
		@days_away = days_away
		@max_days_away = 14
	end

	def to_s
		s = "Album: " + @name + "\n"
		s += "Id: " + @id.to_s + "\n"
		s += "Track list: " + @tracks.to_s + "\n"
		s += "Author: " + @artist + "\n"
		s += "Lent to: " + @lent_to + "\n"
		if @max_days_away - @days_away > 0
			s += "Days left: " + (@max_days_away - @days_away).to_s + "\n"
		else
			s += "Album taken away for too long\n"
		end
		s
	end
end

class AlbumHolder
	def initialize
		@albums = []
	end

	def add_album(album)
		@albums.push(album)
	end

	def save(filename)
		x = 0
		DBM.open(filename) do |db|
			@albums.each {|album| db[x] = album; x += 1}
		end
	end

	def load(filename)
		@albums = []
		DBM.open(filename) do |db|
			db.each {|_, v| @albums.push(v)}
		end
	end

	def to_s
		@albums.each {|album| puts album + "\n"}
	end
end


album_1 = Album.new("Album1", ["Track1", "Track2", "Track3"], "Artist1",
	"Friend1", 15)

album_2 = Album.new("GachiBASS",
                    ["Leave the Gachimuchi on",
                     "Super Gachi Sayian",
                    "It's so it's so it's so",
                    "HandsUP"], "Billy Herrington",
	"Van Darkholme", 5)

filename1 = "holder1.dbm"
holder1 = AlbumHolder.new
holder1.add_album(album_1)
holder1.add_album(album_2)
holder1.save(filename1)

#puts holder1

holder2 = AlbumHolder.new
holder2.load(filename1)

puts holder2


