urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
rot13() { echo $1 | tr "A-Za-z" "N-ZA-Mn-za-m" }
ppjson() { python -mjson.tool }
mcd() { mkdir -p "$1" && cd "$1"; }
last_modified() { ls -t $* 2> /dev/null | head -n 3 }
