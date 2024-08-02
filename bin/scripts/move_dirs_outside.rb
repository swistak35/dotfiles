#!/usr/bin/env ruby

require 'fileutils'

if ARGV.length != 1
  puts "Usage: #{$0} target_directory"
  exit 1
end

target_directory = File.expand_path(ARGV[0])

unless Dir.exist?(target_directory)
  puts "Error: Directory '#{target_directory}' does not exist."
  exit 1
end

Dir.foreach(target_directory) do |entry|
  next if entry == '.' || entry == '..'

  subdirectory = File.join(target_directory, entry)
  next unless File.directory?(subdirectory)

  Dir.foreach(subdirectory) do |file|
    next if file == '.' || file == '..'

    file_path = File.join(subdirectory, file)
    if File.file?(file_path)
      puts "Moving #{file_path} to #{target_directory}"
      FileUtils.mv(file_path, target_directory)
    end
  end

  # Attempt to remove the subdirectory if it is empty
  begin
    Dir.rmdir(subdirectory)
    puts "Removed empty directory #{subdirectory}"
  rescue SystemCallError
    puts "Directory #{subdirectory} is not empty or could not be removed"
  end
end

puts "All files moved successfully."
