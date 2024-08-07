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
	bindkey "^[[1;5A" up-line-or-search
	bindkey "^[[1;5B" down-line-or-search

	# bindkey "^[[D" backward-word
	# bindkey "^[[C" forward-word
    	bindkey "^D" backward-kill-word
	# bindkey "^[[A" up-line-or-search
	# bindkey "^[[5B" down-line-or-search

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
