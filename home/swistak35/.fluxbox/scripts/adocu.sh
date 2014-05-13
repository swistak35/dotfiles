#!/bin/bash
message=$(zenity --entry --text="Zmiana opisu na:" --title="Adocu");
curl -u swistak35:swistak1 -d msg="$message" http://adocu.com/api/status.xml
