#!/usr/bin/env ruby

MUON_DIR = "/home/swistak35/Projekty/opensource/muon/bin/muon2"

Dir.chdir("/home/swistak35/projs/time")
result = `#{MUON_DIR} rawstatus`

tracking_info, day_info, week_info, month_info = result.split("\n")


_, curr_duration, curr_percentage = tracking_info.split(" ")
_, day_duration, day_percentage = day_info.split(" ")
_, week_duration, week_percentage = week_info.split(" ")
_, month_duration, month_percentage = month_info.split(" ")

def formatter(seconds)
  minutes = seconds / 60
  seconds -= minutes * 60
  hours = minutes / 60
  minutes -= hours * 60
  str = ""
  if hours > 0
    str += "#{hours}h, "
  end
  if minutes > 0
    str += "#{minutes}m, "
  end
  if seconds > 0
    str += "#{seconds}s"
  end
  if str.empty?
    "brak"
  else
    str
  end
end

puts "${goto 32}teraz: ${alignr}${font Ubuntu:style=Bold}#{formatter(curr_duration.to_i)}${font} " if curr_duration.to_i > 0
puts "${goto 32}dzien: ${alignr}${font Ubuntu:style=Bold}#{formatter(day_duration.to_i)} (#{day_percentage}%)${font} " if day_duration.to_i > 0
puts "${goto 32}tydzien: ${alignr}${font Ubuntu:style=Bold}#{formatter(week_duration.to_i)} (#{week_percentage}%)${font} "
puts "${goto 32}miesiac: ${alignr}${font Ubuntu:style=Bold}#{formatter(month_duration.to_i)} (#{month_percentage}%)${font} "
