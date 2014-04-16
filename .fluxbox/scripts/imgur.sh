#!/bin/bash
scrot 'temp.png' -e 'mv $f ~/.fluxbox/scripts/'
~/.fluxbox/scripts/upload.sh ~/.fluxbox/scripts/temp.png | xclip -selection clipboard

