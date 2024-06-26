#!/bin/zsh
ifile=$1
ofile="${ifile:r}.gif"

EXTENSION=png

TEMPDIR=$(mktemp -d)

echo '# Determine input WxH and FPS' 
eval "$(ffmpeg -i "$ifile" 2>&1 |sed -nr 's/.*Stream.*Video.* ([0-9]+x[0-9]+).*[^[0-9.]([0-9.]+) fps.*tbr,.*/WxH=\1;FPS=\2/p')"

echo "# Output multiple images to temporary directory $TEMPDIR from the input video $ifile"
ffmpeg -i "$ifile" -r $FPS -s $WxH -f image2 -vframes 25 -y $TEMPDIR/test-%03d.$EXTENSION 2> /dev/null

echo "# use ImageMagic "convert" to generate the animated .gif into $ofile" 
convert -delay 10 $TEMPDIR/test-[0-9][0-9][0-9].$EXTENSION "$ofile"

echo '# remove temp image files'    
rm -rf $TMPDIR/*.$EXTENSION

echo 'Done!'
