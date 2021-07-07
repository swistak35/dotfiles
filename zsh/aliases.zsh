# Funny aliases
alias wtf='dmesg'
alias rtfm='man'
alias moar='more'

# Sudo Aliases
alias sudo='sudo '
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
alias kcrsa="eval \$(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)"
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
alias prb="pry -r '/home/swistak35/.rozne/myhomelib.rb'"
alias :q="exit"
alias adb_cleandb="adb shell pm clear"
alias fd='find . -type d -name'
alias ff='find . -type f -name'
alias muo="/home/swistak35/projs/oss/muon/bin/muon2"
alias fsum="paste -sd+ | bc"
alias lst="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/ /' -e 's/-/|/'"
alias be='bundle exec'
alias jsonify='ruby -rjson -e "puts JSON.pretty_generate(JSON.parse(STDIN.read))"'
alias rm='rm -I'
alias gen-uuid='ruby -rsecurerandom -e "print SecureRandom.uuid"'

# Ruby
alias rake="noglob rake" # allows square brackts for rake task invocation
alias brake='noglob bundle exec rake' # execute the bundled rake gem
alias bern=be_rspec_with_notification

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
alias tms="tmuxinator start"
alias tmn="tmux -2 new-session -s"
alias tml="tmux list-sessions"
alias tma="tmux -2 attach-session -t"


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
alias -g Rb="| ruby -p -e"
alias -g Fst='| awk "{ print \$1 }"'
alias -g Snd='| awk "{ print \$2 }"'
alias -g Last='| awk "{ print \$NF }"'

# git
alias g='git'
alias gc='git commit -S -v'
alias gcm='git commit -S -vm'
alias gca='git commit -S --amend --reuse-message=HEAD'
alias gcup='git commit -S -am"up"'
alias gch='git cherry-pick'
alias gchn='git cherry-pick -n'
alias ga='git add'
alias gp='git push'
alias gpom='git push origin master'
alias gu='git pull'
alias gs='git status'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gd='git diff -w --patience --word-diff=color'
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
alias gds='gd --staged'
alias go='git show'
alias goh='git show HEAD'
