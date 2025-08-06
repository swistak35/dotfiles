#!/usr/bin/env ruby

require 'optparse'
require 'find'

class TmdbNameFiller
  def initialize
    @options = {}
    @dry_run = false
    @only_check = false
    @directory = nil
  end

  def run
    parse_arguments
    validate_arguments
    process_movies_mode
  end

  private

  def parse_arguments
    OptionParser.new do |opts|
      opts.banner = "Usage: #{$0} [options] DIRECTORY"

      opts.on("--dry-run", "Show what would be done without making changes") do
        @dry_run = true
      end

      opts.on("--only-check", "Only check files for missing tags, don't process") do
        @only_check = true
      end


      opts.on("-h", "--help", "Show this help message") do
        puts opts
        exit
      end
    end.parse!

    @directory = ARGV.first
  end

  def validate_arguments
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

    if @only_check
      check_files_for_tags(video_files)
      return
    end

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

  def check_files_for_tags(video_files)
    items_needing_tags = find_items_needing_tags(video_files)
    items_without_tags = filter_items_without_tags(items_needing_tags)

    if items_without_tags.empty?
      puts "All items have tags!"
    else
      puts "Items without tags:"
      items_without_tags.each do |item_path|
        puts "  #{item_path}"
      end
    end
  end

  def find_items_needing_tags(video_files)
    items_needing_tags = []
    
    video_files.each do |file_path|
      parent_dir = File.dirname(file_path)
      parent_dir_name = File.basename(parent_dir)
      
      if parent_dir_name.start_with?("Season ")
        # It's a TV show - add the show directory (parent of parent)
        show_dir = File.dirname(parent_dir)
        items_needing_tags << show_dir unless items_needing_tags.include?(show_dir)
      else
        # It's a movie - add the video file itself
        items_needing_tags << file_path
      end
    end
    
    items_needing_tags
  end

  def filter_items_without_tags(items)
    items.reject do |item_path|
      name = File.basename(item_path)
      has_tmdb_or_tvdb_tag?(name)
    end
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
