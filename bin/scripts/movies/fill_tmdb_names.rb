#!/usr/bin/env ruby

require 'optparse'
require 'find'

class TmdbNameFiller
  def initialize
    @options = {}
    @dry_run = false
    @mode = nil
    @directory = nil
  end

  def run
    parse_arguments
    validate_arguments

    case @mode
    when :movies
      process_movies_mode
    when :shows
      puts "Shows mode not implemented yet"
    end
  end

  private

  def parse_arguments
    OptionParser.new do |opts|
      opts.banner = "Usage: #{$0} [options] DIRECTORY"

      opts.on("--dry-run", "Show what would be done without making changes") do
        @dry_run = true
      end

      opts.on("--movies-mode", "Process movies") do
        if @mode
          puts "Error: Cannot specify both --movies-mode and --shows-mode"
          exit 1
        end
        @mode = :movies
      end

      opts.on("--shows-mode", "Process TV shows") do
        if @mode
          puts "Error: Cannot specify both --movies-mode and --shows-mode"
          exit 1
        end
        @mode = :shows
      end

      opts.on("-h", "--help", "Show this help message") do
        puts opts
        exit
      end
    end.parse!

    @directory = ARGV.first
  end

  def validate_arguments
    unless @mode
      puts "Error: Must specify either --movies-mode or --shows-mode"
      exit 1
    end

    unless @directory
      puts "Error: Directory argument required"
      exit 1
    end

    unless Dir.exist?(@directory)
      puts "Error: Directory '#{@directory}' does not exist"
      exit 1
    end
  end

  def process_movies_mode
    video_files = find_video_files(@directory)

    video_files.each do |file_path|
      filename = File.basename(file_path)

      if has_tmdb_or_tvdb_tag?(filename)
        puts "Skipping #{filename} (already has tag)" if @dry_run
        next
      end

      puts "Processing: #{filename}"

      tmdb_id = find_movie_in_tmdb(filename)

      if tmdb_id
        new_filename = add_tmdb_tag(filename, tmdb_id)
        new_path = File.join(File.dirname(file_path), new_filename)

        if confirm_rename(filename, new_filename)
          if @dry_run
            puts "DRY RUN: Would rename '#{filename}' to '#{new_filename}'"
          else
            # File.rename(file_path, new_path)
            puts "Renamed: #{filename} -> #{new_filename}"
          end
        else
          puts "Skipped: #{filename}"
        end
      else
        puts "No TMDB match found for: #{filename}"
      end
    end
  end

  def find_video_files(directory)
    video_extensions = ['.mkv', '.mp4']
    video_files = []

    Find.find(directory) do |path|
      if File.file?(path)
        ext = File.extname(path).downcase
        video_files << path if video_extensions.include?(ext)
      end
    end

    video_files
  end

  def has_tmdb_or_tvdb_tag?(filename)
    filename.match?(/\{tmdb-\d+\}/) || filename.match?(/\{tvdb-\d+\}/)
  end

  def find_movie_in_tmdb(filename)
    # TODO: Implement TMDB API search
    # For now, return nil (no match found)
    nil
  end

  def add_tmdb_tag(filename, tmdb_id)
    name, ext = filename.split('.', 2)
    "#{name} {tmdb-#{tmdb_id}}.#{ext}"
  end

  def confirm_rename(old_name, new_name)
    print "Rename '#{old_name}' to '#{new_name}'? (y/n): "
    response = gets.chomp.downcase
    response == 'y'
  end
end

if __FILE__ == $0
  TmdbNameFiller.new.run
end
