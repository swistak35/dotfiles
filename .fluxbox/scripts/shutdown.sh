#!/bin/bash
if zenity --question --title="Wyłączyć komputer?" --text="Czy na pewno wyłączyć komputer?"; then
sudo shutdown -h now
fi
