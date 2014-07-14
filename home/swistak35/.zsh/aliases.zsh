# Funny aliases
alias wtf='dmesg'
alias rtfm='man'
alias moar='more'

# Sudo Aliases
alias pacman='sudo pacman'
alias mount='sudo mount'

# App aliases
if [[ $(uname -o) = "GNU/Linux" ]]
then
	alias ls='ls --color=auto --human-readable --group-directories-first --classify'
else
  	alias ls='ls -G'
fi
alias l1='ls -A1'
alias ll='ls -Al'
alias emu='emulator -avd'
alias bakemeup='sudo rdiff-backup --include-filelist /home/swistak35/.rdiff-backup-filelist / /media/backup/rdiffbackup'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias sgrep='grep -R -n -H -C 5 --exclude-dir={.git,.svn,CVS} '
alias diff='colordiff'
alias rubysrv='ruby -run -e httpd . -p5000'
alias du='du -sh'
alias mkdir='mkdir -p'
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias netdebug='mtr google.com'
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i wlp3s0 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""	
alias map="xargs -n1"
alias reload!='. ~/.zshrc'
alias pubkey="more ~/.ssh/id_rsa.pub | cb | echo '=> Public key copied to pasteboard.'"
alias todo="/media/magazyn/Dropbox/Private/todo/todo.sh"
alias t='todo'
alias tw='todo -@ ls @wideo'
alias ti='todo -@ ls @ii'
alias tz='todo -@ ls @rozne'
alias to='todo ls | grep -v @'
alias dly='todo -d /home/swistak35/.daily/todo.cfg'
alias k9='kill -9'
alias localip="ifconfig -a | grep inet | awk '{print \$2}'"
alias leavejobs="disown && exit"
alias iplocation="curl ipinfo.io"
alias checkport="nc -zv localhost"
alias pstree_="pstree -ap"
alias clearcache="pacman -Sc"
alias recentpacs="yaourt -Q --date"
alias iitunel="ssh -C2qTnN -D 8979 i258338@tryglaw.ii.uni.wroc.pl"
alias prb="pry -r '/home/swistak35/.rozne/myhomelib.rb' -r 'active_support/all'"
alias :q="exit"
alias adb_cleandb="adb shell pm clear"
alias kcrsa="eval \$(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)"
alias fd='find . -type d -name'
alias ff='find . -type f -name'
alias largest_packages="pacman -Qi | awk '/Nazwa/ { name=\$3 } /Rozmiar po instalacji/ { printf \"%.3fMB\t%s\n\", \$4/1024, name }' | sort -rh | head -n 20"

# Suffix aliases
alias -g T='| tail'
alias -g H='| head'
alias -g L="| less"
alias -g C='| wc -l'
alias -g G='| grep -i'
alias -g UQ='| uniq'

# GIT aliases
alias g='git'
alias gc='git commit'
alias gcm='git commit -m'
alias ga='git add'
alias gp='git push'
alias gpom='git push origin master'
alias gu='git pull'
alias gs='git status'
alias go='git checkout'
alias gob='git checkout -b'
alias gd='git diff'
alias gb='git branch'
alias gbd='git branch -d'
alias gf='git fetch'
alias gm='git merge'
