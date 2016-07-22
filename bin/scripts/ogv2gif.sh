ifile=$1

# echo $ifile
EXTENSION=png

TEMPDIR=$(mktemp -d)

echo '# Determine input WxH and FPS' 
eval "$(ffmpeg -i "$ifile" 2>&1 |sed -nr 's/.*Stream.*Video.* ([0-9]+x[0-9]+).*[^[0-9.]([0-9.]+) fps.*tbr,.*/WxH=\1;FPS=\2/p')"

echo '# Output multiple images from the input video'
ffmpeg -i "$ifile" -r $FPS -s $WxH -f image2 -vframes 100 -y $TEMPDIR/test-%03d.$EXTENSION 2> /dev/null

echo '# use ImageMagic "convert" to generate the animated .gif' 
convert -delay 20 $TEMPDIR/test-[0-9][0-9][0-9].$EXTENSION ~/test.gif 

# echo '# remove temp image files'    
# rm -rf $TMPDIR/*.$EXTENSION

echo 'Done!'
