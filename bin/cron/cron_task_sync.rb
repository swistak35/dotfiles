#!/usr/bin/env ruby
#
# */10 *  * * * /home/swistak35/.bin/cron/cron_task_sync.rb

LOG_PATH = File.join(ENV['HOME'], ".cron_task_sync.log")

result = `task sync 2>&1`
result.gsub!(/\s+/, ' ')

File.open(LOG_PATH, "a") do |f|
  f.puts "[#{Time.now}] '#{result}'"
end

