# set -x

if [[ -n $1 ]]
then
  if [[ -a $1 ]]
  then
    file=$1
    pygmentize -f rtf $1 > ${file%.*}.rtf
  else
    echo "Taki plik nie istnieje..."
  fi
else
  echo "Daj argument"
fi

