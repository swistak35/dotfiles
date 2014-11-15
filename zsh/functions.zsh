urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }

urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }

rot13() { echo $1 | tr "A-Za-z" "N-ZA-Mn-za-m" }

ppjson() { python -mjson.tool }

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

tdaw() {
  td a "$1 @wideo"
}
