#!/bin/bash
if zenity --question --title="Zrestartować komputer?" --text="Czy na pewno zrestartować komputer?"; then
sudo shutdown -r now
fi
