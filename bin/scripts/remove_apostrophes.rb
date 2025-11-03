#!/usr/bin/env ruby

require 'optparse'
require 'find'

class ApostropheRemover
  def initialize
    @dry_run = false
    @directory = nil
  end

  def run
    parse_arguments
    validate_arguments
    process_files
  end

  private

  def parse_arguments
    OptionParser.new do |opts|
      opts.banner = "Usage: #{$0} [options] DIRECTORY"

      opts.on("--dry-run", "Show what would be done without making changes") do
        @dry_run = true
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

  def process_files
    files_to_rename = find_files_with_apostrophes(@directory)

    if files_to_rename.empty?
      puts "No files with apostrophes found."
      return
    end

    puts "Found #{files_to_rename.length} file(s) with apostrophes:"
    puts

    files_to_rename.each do |file_path|
      old_name = File.basename(file_path)
      new_name = old_name.gsub("'", "")
      new_path = File.join(File.dirname(file_path), new_name)

      if @dry_run
        puts "DRY RUN: Would rename '#{old_name}' to '#{new_name}'"
      else
        File.rename(file_path, new_path)
        puts "Renamed: '#{old_name}' to '#{new_name}'"
      end
    end
  end

  def find_files_with_apostrophes(directory)
    files_with_apostrophes = []

    Find.find(directory) do |path|
      if File.file?(path) && File.basename(path).include?("'")
        files_with_apostrophes << path
      end
    end

    files_with_apostrophes
  end
end

if __FILE__ == $0
  ApostropheRemover.new.run
end