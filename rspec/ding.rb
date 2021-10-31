RSpec.configure do |config|
  config.after(:suite) do
    system("paplay /usr/share/sounds/freedesktop/stereo/bell.oga")
  end
end
