#!/usr/bin/env ruby

# Inline bundle to install dependencies
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'
  gem 'themoviedb'
end

require 'optparse'
require 'find'
require 'themoviedb'
require 'date'

LJUST_SIZE = 20

class TmdbNameFiller
  def initialize
    @options = {}
    @dry_run = false
    @only_check = false
    @no_confirm = false
    @directory = nil

    # Configure TMDB API
    api_key = ENV['TMDB_API_KEY'].strip
    unless api_key
      puts "Error: TMDB_API_KEY environment variable is required"
      exit 1
    end
    Tmdb::Api.key(api_key)
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

      opts.on("--no-confirm", "Skip confirmation and automatically rename files") do
        @no_confirm = true
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
        puts "\nSkipping #{filename} (already has tag)" if @dry_run
        next
      end

      puts "\n#{"Processing:".ljust(LJUST_SIZE)} #{filename}"

      tmdb_results = find_movie_in_tmdb(filename)

      if tmdb_results
        selected_tmdb_id = select_movie_result(filename, tmdb_results)

        if selected_tmdb_id
          new_filename = add_tmdb_tag(filename, selected_tmdb_id)
          new_path = File.join(File.dirname(file_path), new_filename)

          if @dry_run
            puts "#{"DRY RUN Would rename:".ljust(LJUST_SIZE)} '#{filename}' to '#{new_filename}'"
          else
            File.rename(file_path, new_path)
            puts "#{"Renamed:".ljust(LJUST_SIZE)} '#{filename}' to '#{new_filename}'"
          end
        else
          puts "Skipped."
        end
      else
        puts "#{"No TMDB match found for:".ljust(LJUST_SIZE)} #{filename}"
      end
    end
  end

  def year_from_release_date(release_date)
    return nil if release_date.nil?
    Date.parse(release_date).year
  rescue Date::Error
    nil
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
    obfuscated_title = obfuscate_title(filename)
    puts "#{"Searching TMDB for:".ljust(LJUST_SIZE)} #{obfuscated_title}"

    begin
      results = Tmdb::Movie.search(obfuscated_title)

      # binding.irb
      if results && results.any?
        top_results = results.first(3)

        puts "#{"Found results:".ljust(LJUST_SIZE)}"
        top_results.each_with_index do |result, index|
          puts format_result(result, index + 1)
        end

        return top_results
      else
        return nil
      end
    rescue => e
      binding.irb
      puts "Error searching TMDB: #{e.message}"
      return nil
    end
  end

  def format_result(result, option_number = nil)
    prefix = option_number ? "#{option_number}:" : "Selected:"
    "#{prefix.ljust(LJUST_SIZE)} #{result.title} (#{year_from_release_date(result.release_date)}) #{result.original_title != result.title ? "/ #{result.original_title} " : ""}-- #{result.overview}"
  end

  def obfuscate_title(filename)
    # Remove file extension
    title = File.basename(filename, File.extname(filename))

    # Remove common movie/tv show patterns
    title = title.gsub(/\b\d{4}\b/, '') # Remove years
    title = title.gsub(/\b(720p|1080p|1080i|1080pl|2160p|4k|BluRay|WEBRip|DVDRip|BDRip|BRRip|HDTV|WEB-DL)\b/i, '') # Remove quality tags
    title = title.gsub(/\b(x264|x263-drp|x265|HEVC|H264|H265|h\.264|mpeg-2|mpeg2|NF|MA)\b/i, '') # Remove codec info
    title = title.gsub(/\b(AAC|2\.0|AC3|6CH|DTS|HD|DDP|MP3|DTS-HD|atmos|DD-1\.0|DD1\.0|DD2\.0|DD5\.1|DD-5\.1|AVC|VC-1|DTS-HD|DDP2\.0|DDP5\.1|6\.1|AAC5\.1|DSNP|TrueHD|5\.1)\b/i, '') # Remove audio codec info
    title = title.gsub(/\b(remastered|remux|multi|PLSUB|EXTENDED|theatrical|amzn|NF|PL|POLISH|PLDUB|repack|V2)\b/i, '') # Remove other common keywords
    title = title.gsub(/\b(dsite|shaanig|drp|smurf|veto|lts|denda|apex|rarbg|maryjane|kiko|kit|etrg|vppv|yify|ozw|dream|ltn|tpx|rexsio|MR|EMiS|Ralf|B89|K12|playSD|RBG|\[YTS\.MX\])\b/i, '') # Remove ripper
    title = title.gsub(/[-._\(\)]/, ' ') # Replace separators with spaces
    title = title.gsub(/\s+/, ' ') # Collapse multiple spaces
    title = title.strip # Remove leading/trailing whitespace

    title
  end

  def add_tmdb_tag(filename, tmdb_id)
    ext = File.extname(filename)
    name = File.basename(filename, ext)
    "#{name} {tmdb-#{tmdb_id}}#{ext}"
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

  def select_movie_result(filename, results)
    if @no_confirm
      # Auto-select first result when no-confirm is enabled
      return results.first.id
    end

    print "#{"Select option:".ljust(LJUST_SIZE)} "
    valid_options = (1..results.length).map(&:to_s) + ['n']
    print "(#{valid_options.join('/')}) or 'n' to skip: "

    response = STDIN.gets.chomp.downcase

    if response == 'n'
      return nil
    elsif response.match?(/^[1-3]$/) && response.to_i <= results.length
      selected_index = response.to_i - 1
      selected_result = results[selected_index]
      puts format_result(selected_result)
      return selected_result.id
    else
      puts "Invalid option. Skipping."
      return nil
    end
  end
end

if __FILE__ == $0
  TmdbNameFiller.new.run
end
