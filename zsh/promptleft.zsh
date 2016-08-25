autoload -U colors && colors
# Prompt!
# PROMPT="%~%# "
if [[ -a ~/.promptcolor ]]; then
	PROMPTCOLOR=`cat ~/.promptcolor`
else
  	PROMPTCOLOR=blue
	PROMPTCOLOR_STATUSCODE=red
fi
PROMPT="%(?..%{$fg_bold[$PROMPTCOLOR_STATUSCODE]%}[%?]%{$reset_color%})%{$fg_bold[$PROMPTCOLOR]%}%~%# %{$reset_color%}"
