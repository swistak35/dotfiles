#!/bin/zsh

switch-taskwarrior-theme-day() {
    sed -e '/solarized-dark/ s/^#*/#/' -i ~/.taskrc
    sed -e '/solarized-light/ s/#*//' -i ~/.taskrc
}

switch-taskwarrior-theme-night() {
    sed -e '/solarized-light/ s/^#*/#/' -i ~/.taskrc
    sed -e '/solarized-dark/ s/#*//' -i ~/.taskrc
}


if [[ ($(date +%H) -gt 20) || ($(date +%H) -lt 6) ]]
then
    switch-taskwarrior-theme-night
else
    switch-taskwarrior-theme-day
fi
