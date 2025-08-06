#!/usr/bin/env ruby

# Check arguments
if ARGV.length != 2
  puts "Usage: #{$PROGRAM_NAME} DIRECTORY_NAME SEASON_NUMBER"
  exit 1
end

directory_name = ARGV[0]
season_number = ARGV[1].to_i

# Create base directory if it doesn't exist
Dir.mkdir(directory_name) unless Dir.exist?(directory_name)

# Create season subdirectories
(1..season_number).each do |i|
  season_dir = File.join(directory_name, format("Season %02d", i))
  Dir.mkdir(season_dir) unless Dir.exist?(season_dir)
end

puts "Created #{season_number} season directories in '#{directory_name}'"
