# Windows version
# Get-ChildItem spec -Recurse -File -Filter *_spec.rb | Foreach-Object { echo "Processing $($_.FullName)"; bundle exec rspec $_.FullName }
