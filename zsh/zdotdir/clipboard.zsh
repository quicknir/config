function cutbuffer() {
  zle .$WIDGET
  echo $CUTBUFFER | xclip -selection clipboard 2> /dev/null
}

zle_cut_widgets=(
  vi-backward-delete-char
  vi-change
  vi-change-eol
  vi-change-whole-line
  vi-delete
  vi-delete-char
  vi-kill-eol
  vi-substitute
  vi-yank
  vi-yank-eol
)

for widget in $zle_cut_widgets
do
  zle -N $widget cutbuffer
done


function putbuffer() {
  local p rc
  p="$(xclip -o -selection clipboard 2> /dev/null)"
  rc=$?
  if [[ $rc == 0 ]]; then
    zle copy-region-as-kill "$p"
  fi
  zle .$WIDGET
}

zle_put_widgets=(
  vi-put-after
  vi-put-before
)

for widget in $zle_put_widgets
do
  zle -N $widget putbuffer
done
