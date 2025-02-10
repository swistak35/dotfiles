require 'fileutils'

def rename_files_in_directory(directory_path, subtract_value)
  # Define a pattern to match the filenames with a constant prefix, number, and text
  pattern = /^(Muminki )(\d+)( .+\.mp4)$/

  # Iterate through each file in the directory
  Dir.foreach(directory_path) do |filename|
    # Skip '.' and '..'
    next if filename == '.' || filename == '..'

    # Match the filename with the regex pattern
    if match = pattern.match(filename)
      prefix, number_str, suffix = match.captures

      # Convert the number part to an integer and subtract the specified value
      new_number = number_str.to_i - subtract_value
      # Zero-pad the new number to match the original format
      new_filename = "#{prefix}s02e#{"%02d" % new_number}#{suffix}"

      # Create full file paths
      old_filepath = File.join(directory_path, filename)
      new_filepath = File.join(directory_path, new_filename)

      # Rename the file
      FileUtils.mv(old_filepath, new_filepath)
      puts "Renamed: '#{filename}' to '#{new_filename}'"
    end
  end
end

# Usage
directory = "/media/plex/Wideo/Seriale/Muminki/Season 2"
subtract_value = 78
rename_files_in_directory(directory, subtract_value)
