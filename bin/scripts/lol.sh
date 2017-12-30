#!/bin/sh
echo $0
pkill LeagueClientUx.
pkill LeagueClient.ex
/opt/wine-staging/bin/wine /media/magazyn/Program\ Files/Riot\ Games/League\ of\ Legends/LeagueClient.exe &> /dev/null
