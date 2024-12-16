#!/usr/bin/env ruby

require "json"

# LC_ALL=en_US.UTF-8
# Function to list available subtitle tracks for a given MKV file
def list_subtitles(mkv_file)
  # Using mkvmerge to fetch information about the MKV file
  ENV['LC_ALL'] = 'en_US.UTF-8'
  command = "mkvmerge -F json -i '#{mkv_file}'"
  output = `#{command}`


  if $?.success?
    # Extract subtitle track information from the output
    # subtitle_tracks = output.scan(/Track ID (\d+): subtitle/)
    json_result = JSON.parse(output)

    subtitles = json_result["tracks"].select {|t| t["type"] == "subtitles" }

    if subtitles.empty?
      puts "No subtitles found in #{mkv_file}"
    else
      subtitles_langs = subtitles.map {|sub| sub["properties"]["language_ietf"] || sub["properties"]["language"] }
      puts "Subtitles #{subtitles_langs.join(", ")} found in #{mkv_file}"
      # subtitle_tracks.each do |track_id|
      #   puts "Subtitle Track ID: #{track_id[0]} - #{mkv_file}"
      # end
    end
  else
    puts "Error fetching information for file: #{mkv_file}"
  end
end

# Function to iterate over all MKV files in a given directory
def process_directory(directory)
  # Check if the directory exists
  unless Dir.exist?(directory)
    puts "Directory does not exist: #{directory}"
    exit 1
  end

  # Find all MKV files in the directory
  mkv_files = Dir.glob("#{directory}/**/*.mkv")

  if mkv_files.empty?
    puts "No MKV files found in #{directory}"
  else
    mkv_files.each do |file|
      list_subtitles(file)
    end
  end
end

# Check if the user provided a directory argument
if ARGV.empty?
  puts "Usage: ruby list_subtitles.rb /path/to/directory"
  exit 1
end

# Get the directory path from command-line arguments
directory = ARGV[0]

# Process the directory
process_directory(directory)

