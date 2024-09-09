path[1,0]="${ZDOTDIR:h}/fzf/bin"  # just for fzf-tmux
# just for ctrl-r which we use unmodified; we have our own versions
# of ctrl-t and alt-c (and the latter is bound to ctrl-j)
. "${ZDOTDIR:h}/fzf/shell/key-bindings.zsh"

# Use the = style for arguments rather than a space. fzf-tab gets confused otherwise
export FZF_DEFAULT_COLORS="--color=16,fg:11,bg:-1,hl:1:regular,hl+:1,bg+:7,fg+:-1:regular:underline --color=prompt:4,pointer:13,marker:13,spinner:3,info:3"

# -e is for exact matching within a group
export FZF_DEFAULT_OPTS="-e ${FZF_DEFAULT_COLORS} --bind 'ctrl-l:accept' --ansi --layout default"

export FZF_TMUX_OPTS="-p -w 62% -h 38%"
FZF_TC_COMMON_OPTS="--preview-window hidden --bind 'ctrl-h:toggle-preview'"
export FZF_CTRL_T_OPTS="${FZF_TC_COMMON_OPTS} --preview '__fzf_ls_bat_preview {}' --bind 'ctrl-i:unbind(ctrl-i)+reload(fd -u --color always)' --bind 'ctrl-space:toggle'"
export FZF_ALT_C_OPTS="${FZF_TC_COMMON_OPTS} --preview '__fzf_ls_preview {}' --bind 'ctrl-i:unbind(ctrl-i)+reload(__dir_entries -u)'"

export FZF_TMUX=1

__fzfcmd() {
  [ -n "${TMUX_PANE-}" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "${FZF_TMUX_OPTS-}" ]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}

export FZF_ALT_C_COMMAND="fd --type d"
# Replace the fzf cd widget. Our widget doesn't print the line.
# Also, this cd widget includes all of the directories from our history.
fzf-cd-widget() {
  local cmd1="cdr -l | tr -s ' ' | cut -d ' ' -f 2-"
  local cmd="${FZF_ALT_C_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs'     -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type d -print 2> /dev/null | cut -b3-"}"
  setopt localoptions pipefail no_aliases 2> /dev/null
  local dir="$(eval "{ $cmd1 & $cmd }" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} ${FZF_ALT_C_OPTS-}" $(__fzfcmd) +m)"
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  dir=${~dir}
  builtin cd -q "${(q)dir}" && my-redraw-prompt;
  local ret=$?
  return $ret
}
zle -N fzf-cd-widget

export FZF_CTRL_T_COMMAND="fd --color always"

__fsel() {
  local search_dir="."
  local cut_width="3"

  if [[ ${LBUFFER[-1]} != ' ' ]]; then
      tokens=(${(z)LBUFFER})
      if [[ -d ${~tokens[-1]} ]]; then
          search_dir=${~tokens[-1]}
          cut_width="2"
      fi
  fi
  builtin cd -q $search_dir
  # TODO: currently the command find... bit does nothing. Should instead have it fall back to find when
  # fd is unavailable
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b${cut_width}-"}"
  setopt localoptions pipefail no_aliases 2> /dev/null
  local item
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} ${FZF_CTRL_T_OPTS-}" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  echo
  return $ret
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  local ret=$?
  zle reset-prompt
  return $ret
}

zle -N fzf-file-widget

__hist_sel() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local item
  local cmd="tac ${HISTFILE}.color"
  eval "$cmd" | awk '!visited[$0]++' | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --scheme history --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-}" $(__fzfcmd) "$@" | while read item; do
    echo -n "${item} "
  done
  local ret=$?
  echo
  return $ret
}

fzf-history-widget() {
  LBUFFER="${LBUFFER}$(__hist_sel)"
  local ret=$?
  zle reset-prompt
  return $ret
}

# Not currently used; come back to this one day
# fzf-rg-widget() {
#   RG_PREFIX="rg --line-number --no-heading --color=always --smart-case "
#   fzf --ansi \
#       --disabled --query "$INITIAL_QUERY" \
#       --bind "change:reload:sleep 0.1; eval $RG_PREFIX {q} || true" \
#       --delimiter : \
#       --preview 'bat --color=always {1} --highlight-line {2}' \
#       --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
# }
# zle -N fzf-rg-widget
# bindkey '^S' fzf-rg-widget