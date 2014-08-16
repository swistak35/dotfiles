autoload -U colors && colors
# Prompt!
# PROMPT="%~%# "
if [[ -a ~/.promptcolor ]]; then
	PROMPTCOLOR=`cat ~/.promptcolor`
else
  	PROMPTCOLOR=blue
fi
PROMPT="%{$fg_bold[$PROMPTCOLOR]%}%~%# %{$reset_color%}"