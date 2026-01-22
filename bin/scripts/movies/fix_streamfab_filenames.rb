#!/usr/bin/env ruby

require 'optparse'
require 'find'

class StreamFabFilenameFixer
  LJUST_SIZE = 30

  def initialize
    @dry_run = false
    @path = nil
  end

  def run
    parse_arguments
    validate_arguments
    process_path(@path)
  end

  private

  def parse_arguments
    OptionParser.new do |opts|
      opts.banner = "Usage: #{$0} [options] PATH"

      opts.on("--dry-run", "Show what would be done without making changes") do
        @dry_run = true
      end

      opts.on("-h", "--help", "Show this help message") do
        puts opts
        exit
      end
    end.parse!

    @path = ARGV.first
  end

  def validate_arguments
    unless @path
      puts "Error: PATH argument is required"
      puts "Usage: #{$0} [options] PATH"
      exit 1
    end

    unless File.exist?(@path)
      puts "Error: Path '#{@path}' does not exist"
      exit 1
    end
  end

  def process_path(path)
    if File.file?(path)
      process_file(path)
    elsif File.directory?(path)
      process_directory(path)
    else
      puts "Error: Path '#{path}' is neither a file nor a directory"
      exit 1
    end
  end

  def process_file(file_path)
    old_filename = File.basename(file_path)

    # Skip hidden files
    if old_filename.start_with?('.')
      return
    end

    # Transform the filename
    new_filename = transform_filename(old_filename)

    # Handle empty filename after transformation
    if new_filename.nil?
      puts "#{"Skipping:".ljust(LJUST_SIZE)} '#{old_filename}' (empty after transformation)"
      return
    end

    # Skip if no changes needed
    if old_filename == new_filename
      return
    end

    # Build new path
    new_path = File.join(File.dirname(file_path), new_filename)

    # Check if target already exists
    if File.exist?(new_path)
      puts "#{"Skipping:".ljust(LJUST_SIZE)} '#{old_filename}' (target '#{new_filename}' already exists)"
      return
    end

    # Perform the rename or show dry-run message
    if @dry_run
      puts "#{"[DRY RUN] Would rename:".ljust(LJUST_SIZE)} '#{old_filename}' -> '#{new_filename}'"
    else
      begin
        File.rename(file_path, new_path)
        puts "#{"Renamed:".ljust(LJUST_SIZE)} '#{old_filename}' -> '#{new_filename}'"
      rescue => e
        puts "#{"Error:".ljust(LJUST_SIZE)} Failed to rename '#{old_filename}': #{e.message}"
      end
    end
  end

  def process_directory(directory_path)
    Find.find(directory_path) do |path|
      next unless File.file?(path)
      process_file(path)
    end
  end

  def transform_filename(filename)
    # Step 1: Split into basename and extension
    ext = File.extname(filename)
    basename = File.basename(filename, ext)

    # Step 2: Replace spaces with dots in basename
    transformed = basename.gsub(' ', '.')
    transformed = transformed.gsub('_', '.')

    # Step 3: Replace Polish characters with their non-accented counterparts
    polish_chars = {
      'ą' => 'a', 'Ą' => 'A',
      'ć' => 'c', 'Ć' => 'C',
      'ę' => 'e', 'Ę' => 'E',
      'ł' => 'l', 'Ł' => 'L',
      'ń' => 'n', 'Ń' => 'N',
      'ó' => 'o', 'Ó' => 'O',
      'ś' => 's', 'Ś' => 'S',
      'ź' => 'z', 'Ź' => 'Z',
      'ż' => 'z', 'Ż' => 'Z'
    }
    polish_chars.each do |accented, plain|
      transformed = transformed.gsub(accented, plain)
    end

    # Step 4: Remove non-allowed characters from basename
    # Keep only: a-z, A-Z, 0-9, dots, dashes
    transformed = transformed.gsub(/[^a-zA-Z0-9.\-]/, '')

    # Step 5: Collapse multiple consecutive dots into single dot
    transformed = transformed.gsub(/\.+/, '.')

    # Step 6: Remove leading/trailing dots from basename
    transformed = transformed.gsub(/^\.+|\.+$/, '')

    # Step 7: Handle empty filename case
    return nil if transformed.empty?

    # Step 8: Rejoin with extension (preserving original extension)
    return transformed + ext
  end
end

# Run the script if executed directly
if __FILE__ == $0
  StreamFabFilenameFixer.new.run
end
