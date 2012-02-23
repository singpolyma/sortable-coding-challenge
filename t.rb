require 'json'

open('listings.txt') do |fh|
	while (line = fh.gets)
		line.chomp!
		p JSON.parse(line)
	end
end
