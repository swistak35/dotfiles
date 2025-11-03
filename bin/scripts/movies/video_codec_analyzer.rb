#!/usr/bin/env ruby

require 'find'
require 'json'

def get_video_codec(file_path)
  cmd = "ffprobe -v quiet -print_format json -show_streams \"#{file_path}\" 2>/dev/null"
  result = `#{cmd}`
  return nil if $?.exitstatus != 0
  
  begin
    data = JSON.parse(result)
    video_stream = data['streams']&.find { |stream| stream['codec_type'] == 'video' }
    video_stream&.dig('codec_name')
  rescue JSON::ParserError
    nil
  end
end

def is_video_file?(file_path)
  video_extensions = %w[.mp4 .mkv .avi .mov .wmv .flv .webm .m4v .mpg .mpeg .3gp .ogv .asf .rm .rmvb .vob .ts .mts .m2ts]
  ext = File.extname(file_path).downcase
  video_extensions.include?(ext)
end

def main
  if ARGV.length < 1 || ARGV.length > 2
    puts "Usage: #{$0} <directory> [codec]"
    puts "  If codec is specified, lists all files with that codec"
    puts "  If codec is not specified, shows summary of all codecs"
    exit 1
  end

  directory = ARGV[0]
  target_codec = ARGV[1]

  unless File.directory?(directory)
    puts "Error: #{directory} is not a directory"
    exit 1
  end

  codec_files = Hash.new { |h, k| h[k] = [] }
  total_files = 0

  Find.find(directory) do |path|
    next unless File.file?(path) && is_video_file?(path)
    
    total_files += 1
    codec = get_video_codec(path)
    codec = 'unknown' if codec.nil?
    codec_files[codec] << path
  end

  if target_codec
    if codec_files.key?(target_codec)
      puts "Files with codec '#{target_codec}':"
      codec_files[target_codec].each { |file| puts file }
      puts "\nTotal: #{codec_files[target_codec].length} files"
    else
      puts "No files found with codec '#{target_codec}'"
    end
  else
    puts "Codec summary for directory: #{directory}"
    puts "=" * 50
    
    codec_files.sort_by { |codec, files| -files.length }.each do |codec, files|
      puts "#{codec.ljust(20)}: #{files.length} files"
    end
    
    puts "=" * 50
    puts "Total video files analyzed: #{total_files}"
  end
end

main if __FILE__ == $0