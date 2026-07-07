urlencode() { ruby -rcgi -e 'puts CGI.escape(ARGV[0])' "$1" }

urldecode() { ruby -rcgi -e 'puts CGI.unescape(ARGV[0])' "$1" }

rot13() { echo $1 | tr "A-Za-z" "N-ZA-Mn-za-m" }

ppjson() { ruby -rjson -e 'puts JSON.pretty_generate(JSON.parse(STDIN.read))' }

mcd() { mkdir -p "$1" && cd "$1"; }

last_modified() { ls -t $* 2> /dev/null | head -n 3 }

rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

repeatit() {
  while true
  do
    $@ && return
  done
}

repeatit05() {
  while true
  do
    sleep 0.5
    $@
  done
}

be_rspec_with_notification() {
  bundle exec rspec $*
  tput bel
  notify-send "Tests are done." "Back to Coding!"
}

# Plaintext HTTP sniffers. Interface defaults to the default-route one,
# pass another as $1 to override.
sniff() {
  local iface=${1:-$(ip route | awk '/^default/ {print $5; exit}')}
  sudo tcpdump -i $iface -nn -l -A -s0 'tcp port 80' | grep -a --line-buffered -E '^(GET|POST) '
}

httpdump() {
  local iface=${1:-$(ip route | awk '/^default/ {print $5; exit}')}
  sudo tcpdump -i $iface -n -s 0 -w - | grep -a -o -E 'Host: .*|GET /.*'
}
