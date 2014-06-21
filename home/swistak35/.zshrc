# Wtf?
bindkey -e
# zstyle :compinstall filename '/home/swistak35/.zshrc'

# ========================
# === KEY SHORTCUTS SETUP
# ========================
	# create a zkbd compatible hash;
	# to add other keys to this hash, see: man 5 terminfo
	typeset -A key

	key[Home]=${terminfo[khome]}
	key[End]=${terminfo[kend]}
	key[Insert]=${terminfo[kich1]}
	key[Delete]=${terminfo[kdch1]}
	key[Up]=${terminfo[kcuu1]}
	key[Down]=${terminfo[kcud1]}
	key[Left]=${terminfo[kcub1]}
	key[Right]=${terminfo[kcuf1]}
	# key[PageUp]=${terminfo[kpp]}
	# key[PageDown]=${terminfo[knp]}

	[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
	[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
	[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
	[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
	[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
	[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
	[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
	[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
	bindkey "^[[1;5D" backward-word
	bindkey "^[[1;5C" forward-word
    	bindkey "^D" backward-kill-word
	bindkey "^[[1;5A" up-line-or-search
	bindkey "^[[1;5B" down-line-or-search

	# Finally, make sure the terminal is in application mode, when zle is
	# active. Only then are the values from $terminfo valid.
	if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
	    function zle-line-init () {
	        printf '%s' "${terminfo[smkx]}"
	    }
	    function zle-line-finish () {
	        printf '%s' "${terminfo[rmkx]}"
	    }
	    zle -N zle-line-init
	    zle -N zle-line-finish
	fi
# ========================
# === END
# ========================




# ========================
# === Non-shell features
# ========================
	# Aliases for ..., ...., ....., etc.
		rationalise-dot() {
		  if [[ $LBUFFER = *.. ]]; then
		    LBUFFER+=/..
		  else
		    LBUFFER+=.
		  fi
		}
		zle -N rationalise-dot
		bindkey . rationalise-dot
	# End
# ========================
# == END
# ========================




# ========================
# === ZSH Modules
# ========================
	zmodload zsh/mathfunc
# ========================
# END
# ========================




# ========================
# === Various ZSH features
# ========================
	autoload -U colors && colors
	# Prompt!
	# PROMPT="%~%# "
	if [[ -a ~/.promptcolor ]]; then
		PROMPTCOLOR=`cat ~/.promptcolor`
	else
	  	PROMPTCOLOR=blue
	fi
	PROMPT="%{$fg_bold[$PROMPTCOLOR]%}%~%# %{$reset_color%}"
	# PROMPT="%{$fg_bold[white]%}%~%# %{$reset_color%}"
	
	# Autocorrect commands
	# setopt correctall
	
	# Auto cd in directories
	setopt autocd
	
	# Extended Globbing
	# ex. `cp ^*.(tar|bz2|gz)`
	setopt extendedglob
# ========================
# === END
# ========================




# ========================
# ZMV - ZSh Massive Rename
# ========================
	autoload zmv
# ========================
# === END
# ========================




# ========================
# === ZSH Completion
# ========================
	# Init
	autoload -Uz compinit
	setopt completealiases
	compinit
	
	# Cache
	zstyle ':completion:*' use-cache on
	zstyle ':completion:*' cache-path ~/.zsh/cache	

	# `cd` not select parent dir
	zstyle ':completion:*:cd:*' ignore-parents parent pwd

	# Tab completion for PIDs
	zstyle ':completion:*:*:kill:*' menu yes select
	zstyle ':completion:*:kill:*' force-list always
	zstyle ':completion::*:kill:*:*' command 'ps xf -U $USER -o pid,%cpu,cmd'
	zstyle ':completion::*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'	

	# Partial colouring
	zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34=34}:${(s.:.)LS_COLORS}")';

	# Allow approximate
	zstyle ':completion:*' completer _complete _match _approximate
	zstyle ':completion:*:match:*' original only
	zstyle ':completion:*:approximate:*' max-errors 1 numeric
	
	# Sudo completion
	zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
# ========================
# === END
# ========================

# ========================
# === ZSH History settings
# ========================
	# Init
	HISTFILE=~/.zshhistory
	HISTSIZE=50000
	export SAVEHIST=$HISTSIZE
	
	# Ignoring dups in ZSH history
	setopt hist_ignore_all_dups
	
	# Ignoring commands with pre-space in ZSH history
	setopt hist_ignore_space
	
	# Share history
	setopt append_history
	setopt inc_append_history
	setopt share_history
# ========================
# === END
# ========================



# ========================
# === Aliases
# ========================
	# Funny aliases
	alias wtf='dmesg'
	alias rtfm='man'
	alias moar='more'
	alias rubysrv='ruby -run -e httpd . -p5000'
	
	# Sudo Aliases
	alias pacman='sudo pacman'
	alias mount='sudo mount'
	
	# App aliases
	alias ls='ls --color=auto --human-readable --group-directories-first --classify'
	alias l1='ls -A1'
	alias ll='ls -Al'
	alias emu='emulator -avd'
	alias bakemeup='sudo rdiff-backup --include-filelist /home/swistak35/.rdiff-backup-filelist / /media/backup/rdiffbackup'
	alias grep='grep --colour=auto'
	alias egrep='egrep --colour=auto'
	alias diff='colordiff'
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
	alias ga='git add'
	alias gp='git push'
	alias gu='git pull'
	alias gs='git status'
	alias gco='git checkout'
	alias gb='git branch'
	alias gpom='git push origin master'
# ========================
# === End
# ========================




# ========================
# === Environment variables
# ========================
	export LESS='-FRSX -i-P%f (%i/%m) Line %lt/%L'
	export EDITOR="nano"
	export BROWSER="firefox"
	export XTERM="konsole"
	export FACEBOOK_SECRET=5b04b2bd4277aaca57da23447f791ef5
	export FACEBOOK_KEY=297537827012809
	export GITHUB_SECRET=fd74e299b442056dfd23d822638cd3b918bc248f
	export GITHUB_KEY=6bfa34fa6519c4a10695
	export ANDROID_HOME=/home/swistak35/android-sdk-linux
	# export LANG=pl_PL.utf8
	# export LC_ALL=pl_PL.utf8
	export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
	export PATH=$PATH:$HOME/.bin # My custom commands
# ========================
# === End
# ========================




# ========================
# === Plugins
# ========================
	# Awesome highlighting
	ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor root)
	source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	ZSH_HIGHLIGHT_STYLES[path]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[globbing]='fg=magenta,bold'
	
	# Git right prompt
	source ~/.zsh/plugins/zsh-nice-ps/ps.zsh
	
	# Marking dirs
	source ~/.zsh/plugins/zsh-marks/zsh-marks.sh

	# Arch's command not found
	source /usr/share/doc/pkgfile/command-not-found.zsh

	# Cabal comp
	source ~/.zsh/plugins/cabal/cabal.plugin.zsh

	# Sudo "Esc Esc" keybind
	source ~/.zsh/plugins/sudo/sudo.zsh

	# Spectrum of colors
	source ~/.zsh/plugins/spectrum/spectrum.zsh

	# Colored man pages
	source ~/.zsh/plugins/colored_man/colored_man.zsh
	
	# Extract function
	source ~/.zsh/plugins/extract/extract.zsh
	
	# Brackets
	source ~/.zsh/plugins/brackets/brackets.zsh
# ========================
# === End
# ========================




# ========================
# === Other features
# ========================
	# RVM
	# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
	source $HOME/.rvm/scripts/rvm

	# Opam
	/home/swistak35/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

	# SSH-agent
	eval $(ssh-agent)
# ========================
# === End
# ========================
