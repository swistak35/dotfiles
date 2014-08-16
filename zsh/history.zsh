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