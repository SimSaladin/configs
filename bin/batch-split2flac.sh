#!/bin/bash
# Requirements:
#     * split2flac.sh
#
# a little wrapper for split2flac.sh to go through many directories.

CHARSET=MS932
UPDIR=$PWD
mkdir "$UPDIR/processed" &>/dev/null
valid_image_exts=('ape' 'flac' 'tak' 'tta' 'wav')

shopt -s nullglob

echo ">>> going through directories in directory $UPDIR"

find_images(){
   IMGS=()
   for X in ${valid_image_exts[@]}; do
      for img in *.$X; do
         IMGS[${#IMGS[@]}]="$img"
      done
   done
   echo ">>> found ${#IMGS[@]} image(s):"
   for img in "${IMGS[@]}"; do
      echo "      $img"
   done
   return 0
}

find_cue(){
   FILE="$@"
   echo ">>> Looking for cue to match $FILE"
   CUE=
   for cue in *.cue; do
      CUE="$cue"
      if [[ $(grep "\"$FILE\"" "$cue") != "" ]]; then
         return 0
      fi
   done
   for c in *.cue; do
      echo "  +++ $c"
   done
   echo -n ">>> no cue file for this image! force <$FILE> to cue <$CUE> ? [Y/n]"
   read doing
   if [[ $doing == n ]]; then
      echo ">>> please enter a cue file: "
      read CUE
      [ -f "$CUE" ] || return 1
   else
      sed "s/FILE \".*\"/FILE \"$CUE\"/" "$CUE" > "$CUE".temp.cue
      CUE="$CUE".temp.cue
   fi
   return 1
}
for a in "$@"; do
   cd "$UPDIR"
   echo ">>> in dir: $a"
   cd "$a"

   # images to process
   find_images
   if [[ ${#IMGS[@]} -eq 0 ]]; then
      echo -n ">>> WARNING: no image(s) found in $a"
      # read do_continue
      # [[ "$do_continue" == y ]] || exit 
   fi
   for image in "${IMGS[@]}"; do
      echo "$image"
      find_cue "$image"
      echo ">>> using .cue: $CUE"
      split2flac.sh -cuecharset "$CHARSET" -cue "$CUE" "$image"
   done
done
exit 0
