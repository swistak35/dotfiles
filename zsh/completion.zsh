# Completion configuration: fpath and styles only. compinit itself runs later,
# via zinit's zicompinit (turbo mode) in zshrc — do NOT call compinit here,
# and keep this file sourced before the first prompt so fpath is ready.

# Custom completion functions (_bw, _muon, ...)
fpath=(~/.zsh/completions $fpath)

zmodload -i zsh/complist

##############
# Behaviour  #
##############

# Cache slow completion sources (apt, dpkg, ...)
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh
[[ -d ~/.cache/zsh ]] || mkdir -p ~/.cache/zsh

# Arrow-key navigable menu; Shift-Tab cycles backwards
zstyle ':completion:*' menu select
bindkey -M menuselect '^[[Z' reverse-menu-complete

# Completers: normal, then pattern match, then typo correction
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Try exact first, then case-insensitive, then partial-word (f.b -> foo.bar),
# then substring anywhere
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Pick up newly installed commands without restarting the shell
zstyle ':completion:*' rehash true

# Paths: don't offer the dir you're already in; allow ../<Tab>; collapse //;
# don't stat every dir on exact path components (faster on slow/network fs)
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' accept-exact-dirs true

################
# Presentation #
################

# Group matches by type, with a header per group
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}— %d —%f'
zstyle ':completion:*:messages' format '%F{magenta}%d%f'
zstyle ':completion:*:warnings' format '%F{red}no matches:%f %d'
zstyle ':completion:*' verbose yes
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*:manuals' separate-sections true

# Colour matches like ls; highlight the prefix typed so far
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34=34}:${(s.:.)LS_COLORS}")'

####################
# Command-specific #
####################

# kill: menu with own processes, PIDs highlighted
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion::*:kill:*:*' command 'ps xf -U $USER -o pid,%cpu,cmd'
zstyle ':completion::*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'

# sudo: also complete commands from sbin dirs
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# ssh & friends: Host lines from ~/.ssh/config (skipping wildcard patterns)
# plus known_hosts entries (skipping hashed "|1|..." and bracketed ones)
zstyle -e ':completion:*:(ssh|scp|sftp|rsync):*' hosts 'reply=(
  ${${${(@M)${(f)"$(cat ~/.ssh/config /dev/null 2>/dev/null)"}:#Host *}#Host }:#*[*?]*}
  ${${${(f)"$(cat ~/.ssh/known_hosts /etc/ssh/ssh_known_hosts /dev/null 2>/dev/null)"}:#[\|\[]*}%%[ ,]*}
)'
