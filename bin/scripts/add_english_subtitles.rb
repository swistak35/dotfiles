#!/usr/bin/env ruby

require 'fileutils'
require 'open3'
require 'json'
require 'optparse'

# Command-line option parsing
options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: ruby add_subtitles.rb [options] DIRECTORY"

  opts.on("-d", "--dry-run", "Perform a dry run without modifying files") do
    options[:dry_run] = true
  end
end.parse!

# Default value for DRY_RUN (false if not specified in the CLI)
DRY_RUN = options[:dry_run] || false

def add_subtitles(video_file, subtitle_file)
  output_file = File.join(File.dirname(video_file), "#{File.basename(video_file, File.extname(video_file))}_with_subs.mkv")

  command = [
    'mkvmerge',
    '-o', output_file,
    video_file,
    '--language', '0:eng',
    '--track-name', '0:English',
    subtitle_file
  ]

  if DRY_RUN
    puts "[DRY RUN] Adding subtitles #{File.basename(subtitle_file)} to #{File.basename(video_file)}"
  else
    # Run the mkvmerge command to add the subtitle
    stdout, stderr, status = Open3.capture3(*command)

    if status.success?
      puts "Subtitles #{File.basename(subtitle_file)} added to #{File.basename(video_file)} successfully."
    else
      puts "Error adding subtitles #{File.basename(subtitle_file)} to #{File.basename(video_file)}: #{stderr}"
    end
  end
end

def video_has_english_subtitle(video_file)
  # Check if the video already has English subtitles
  command = "mkvmerge -J '#{video_file}'"
  output = `#{command}`
  json_result = JSON.parse(output)
  subtitles = json_result["tracks"].select {|t| t["type"] == "subtitles" }
  subtitles_langs = subtitles.map {|sub| sub["properties"]["language_ietf"] || sub["properties"]["language"] }

  !(subtitles_langs & ["en", "eng", "en-US"]).empty?
end

def find_subtitle_file(video_file, subtitle_files)
  # Extract season and episode (e.g., s02e03)
  season_episode = video_file.match(/s(\d{2})e(\d{2})/i)

  if season_episode
    season_episode_str = season_episode[0].downcase

    subtitle_files.each do |subtitle_file|
      return subtitle_file if subtitle_file.downcase.include?(season_episode_str)
    end
  end
  nil
end

def process_videos(directory)
  # List video and subtitle files
  video_files = Dir.entries(directory).select { |f| f.match?(/\.(mkv|mp4|avi)$/i) }
  subtitle_files = Dir.entries(directory).select { |f| f.match?(/\.(srt|sub)$/i) }

  video_files.each do |video_file|
    video_path = File.join(directory, video_file)

    # Skip if video already has English subtitles
    if video_has_english_subtitle(video_path)
      puts "Warning: #{video_file} already has English subtitles. Skipping."
      next
    end

    # Find corresponding subtitle file
    subtitle_file = find_subtitle_file(video_file, subtitle_files)

    if subtitle_file
      subtitle_path = File.join(directory, subtitle_file)
      add_subtitles(video_path, subtitle_path)
    else
      puts "No subtitle found for #{video_file}."
    end
  end
end

# Set the directory you want to process
directory = ARGV[0]
if directory.nil? || !Dir.exist?(directory)
  puts "Please provide a valid directory."
  exit 1
end

process_videos(directory)
