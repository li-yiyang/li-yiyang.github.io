#!/bin/sh

FORMAT=png

if [ $# -eq 1 ]
then
  FULL_FILENAME=$1
  FILENAME="${FULL_FILENAME%%.*}"
  OUTPUT="$FILENAME.$FORMAT"
  convert $1 -colorspace Gray -ordered-dither o4x4 $OUTPUT  
  echo "$FULL_FILENAME -> $OUTPUT"
else 
  if [ $# -eq 2 ] 
  then
    FULL_FILENAME=$1
    FILENAME="${FULL_FILENAME%%.*}"
    OUTPUT="$FILENAME.$FORMAT"
    convert $1 -colorspace Gray -ordered-dither o$2 $OUTPUT 
    echo "$FULL_FILENAME -> $OUTPUT"
  else
    echo "./diter.sh [file] [2x2]"
  fi
fi
