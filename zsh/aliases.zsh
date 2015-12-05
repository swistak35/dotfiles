# Funny aliases
alias wtf='dmesg'
alias rtfm='man'
alias moar='more'

# Sudo Aliases
alias sudo='sudo '
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
alias mapx="xargs -n1"
alias reload!='. ~/.zshrc'
alias pubkey="more ~/.ssh/id_rsa.pub | cb | echo '=> Public key copied to pasteboard.'"
alias k9='kill -9'
alias localip="ifconfig -a | grep inet | awk '{print \$2}'"
alias leavejobs="disown && exit"
alias iplocation="curl ipinfo.io"
alias checkport="nc -zv localhost"
alias pstree_="pstree -ap"
alias clearcache="pacman -Sc"
alias recentpacs="yaourt -Q --date"
alias iitunel="ssh -C2qTnN -D 8979 i258338@tryglaw.ii.uni.wroc.pl"
alias prb="pry -r '/home/swistak35/.rozne/myhomelib.rb'"
alias :q="exit"
alias adb_cleandb="adb shell pm clear"
alias kcrsa="eval \$(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)"
alias fd='find . -type d -name'
alias ff='find . -type f -name'
alias largest_packages="pacman -Qi | awk '/Nazwa/ { name=\$3 } /Rozmiar po instalacji/ { printf \"%.3fMB\t%s\n\", \$4/1024, name }' | sort -rh | head -n 20"
alias muo="/home/swistak35/projs/oss/muon/bin/muon2"
alias fsum="paste -sd+ | bc"
alias lst="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/ /' -e 's/-/|/'"
alias be='bundle exec'
alias jsonify='ruby -rjson -e "puts JSON.pretty_generate(JSON.parse(STDIN.read))"'
alias fix_sudo_display='xhost + > /dev/null'
alias idris="/opt/idris/.cabal-sandbox/bin/idris"
alias rm='rm -I'
alias gen-uuid='ruby -rsecurerandom -e "print SecureRandom.uuid"'

# Todo
alias todo="/home/swistak35/Dropbox/Private/todo/todo.sh"
alias td='todo'
alias tdw='td ls "+productivity\|+pierdoly"'
alias tda='td ls +arkency'
alias tdo='td ls +long'
alias tdl='td ls +learning +idefix'
alias tdr='td ls -arkency -daily -long -learning -idefix -productivity -pierdoly'
alias tdd='td ls +daily'

# Tmux
alias tmk="tmux kill-session -t"
alias tms="mux start"
alias tmn="tmux -2 new-session -s"
alias tml="tmux list-sessions"
alias tma="tmux attach-session -t"


# Suffix aliases
alias -g T='| tail'
alias -g H='| head'
alias -g L="| less"
alias -g C='| wc -l'
alias -g G='| grep -i'
alias -g S='| sort'
alias -g ED='| sed -e'
alias -g SN='| sort -n'
alias -g UQ='| uniq'
alias -g UT="| cut"
alias -g E="| tee"
alias -g R="| ruby -p -e"

# git
alias g='git'
alias gc='git commit -v'
alias gcm='git commit -vm'
alias gca='git commit --amend --reuse-message=HEAD'
alias gcup='git commit -am"up"'
alias gch='git cherry-pick'
alias gchn='git cherry-pick -n'
alias ga='git add'
alias gp='git push'
alias gpom='git push origin master'
alias gu='git pull'
alias gs='git status'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gd='git diff -w'
alias gb='git branch'
alias gbd='git branch -d'
alias gf='git fetch'
alias gm='git merge'
alias glg='git log -p'
alias gls='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias gll='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
alias gldr='git log --pretty=format:"%C(yellow)%h\\ %ad%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --date=relative'
alias glds='git log --pretty=format:"%C(yellow)%h\\ %ad%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --date=short'
alias gss='git stash save'
alias gsp='git stash pop'
alias gds='git diff --staged'
alias go='git show'
alias goh='git show HEAD'
