#!/usr/bin/env ruby

require "json"

# Function to check if required tracks exist in an MKV file
def check_tracks(mkv_file, required_audio_langs, required_subtitle_langs)
  # Using mkvmerge to fetch information about the MKV file
  ENV['LC_ALL'] = 'en_US.UTF-8'
  command = "mkvmerge -F json -i '#{mkv_file}'"
  output = `#{command}`

  unless $?.success?
    puts "ERROR: Could not analyze file: #{mkv_file}"
    return
  end

  json_result = JSON.parse(output)
  subtitles = json_result["tracks"].select { |t| t["type"] == "subtitles" }
  audio_tracks = json_result["tracks"].select { |t| t["type"] == "audio" }

  # Extract languages from tracks
  subtitle_langs = subtitles.map { |sub| sub["properties"]["language_ietf"] || sub["properties"]["language"] || "und" }
  audio_langs = audio_tracks.map { |aud| aud["properties"]["language_ietf"] || aud["properties"]["language"] || "und" }

  puts "Checking: #{mkv_file}"
  
  # Check audio tracks
  missing_audio = []
  required_audio_langs.each do |required_lang|
    found = audio_langs.any? { |lang| lang.include?(required_lang) }
    missing_audio << required_lang unless found
  end

  # Check subtitle tracks
  missing_subtitles = []
  required_subtitle_langs.each do |required_lang|
    found = subtitle_langs.any? { |lang| lang.include?(required_lang) }
    missing_subtitles << required_lang unless found
  end

  # Report results
  if missing_audio.empty? && missing_subtitles.empty?
    puts "  ✓ All required tracks present"
  else
    puts "  ⚠ MISSING TRACKS:"
    unless missing_audio.empty?
      puts "    Missing audio: #{missing_audio.join(', ')}"
    end
    unless missing_subtitles.empty?
      puts "    Missing subtitles: #{missing_subtitles.join(', ')}"
    end
  end
  
  puts "    Available audio: #{audio_langs.join(', ')}" if audio_langs.any?
  puts "    Available subtitles: #{subtitle_langs.join(', ')}" if subtitle_langs.any?
  puts
end

# Function to iterate over all MKV files in a given directory
def process_directory(directory, required_audio_langs, required_subtitle_langs)
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
      check_tracks(file, required_audio_langs, required_subtitle_langs)
    end
  end
end

# Check if the user provided the required arguments
if ARGV.length < 3
  puts "Usage: ruby check_subtitles_and_audio.rb AUDIO_LANGS SUBTITLE_LANGS /path/to/directory"
  puts "Example: ruby check_subtitles_and_audio.rb pl,en pl,en /path/to/videos"
  exit 1
end

# Parse command-line arguments
audio_langs_str = ARGV[0]
subtitle_langs_str = ARGV[1]
directory = ARGV[2]

# Split comma-separated language lists
required_audio_langs = audio_langs_str.split(',').map(&:strip)
required_subtitle_langs = subtitle_langs_str.split(',').map(&:strip)

puts "Checking for audio languages: #{required_audio_langs.join(', ')}"
puts "Checking for subtitle languages: #{required_subtitle_langs.join(', ')}"
puts "Directory: #{directory}"
puts "=" * 50

# Process the directory
process_directory(directory, required_audio_langs, required_subtitle_langs)