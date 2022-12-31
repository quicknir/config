# Enable Powerlevel10k instant prompt. Should stay close to the top of /spare/ssd_local/nir/zsh/home/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set terminal
export TERM="xterm-256color"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Our zshenv has handled setting zdotdir to the path within our repo, so now
# we can use zdotdir to locate the rest of our config
termdir="${ZDOTDIR:h}"

# Better ls coloring when using solarized terminal theme
eval $(eval "dircolors ${termdir}/dircolors-solarized/dircolors.ansi-light")
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

alias vi=vim
# For better vi usability, reduce key delay/timeout                                                                       KEYTIMEOUT=1

# Vim-like movement bindings!
zmodload zsh/complist  # Necessary so that menuselect keymap gets loaded; otherwise gets lazy loaded on first use
bindkey -M menuselect '^J' down-line-or-history
bindkey -M menuselect '^K' up-line-or-history
bindkey -M menuselect '^H' backward-char
bindkey -M menuselect '^L' forward-char

# fzf setup
fzfdir="$termdir/fzf"
export PATH="$PATH:$fzfdir/bin"

source "$fzfdir/shell/key-bindings.zsh"

# To generate paths, use default find-based command for dirs,
# and ag to find files more quickly
_fzf_compgen_path() {
  { _fzf_compgen_dir $1 & ag --hidden -g "" "$1" } 2> /dev/null
}

# Exact matching similar to helm
export FZF_DEFAULT_OPTS="-e \
   --color 16,fg:11,bg:-1,hl:1,hl+:1,bg+:7,fg+:11 \
   --color prompt:4,pointer:13,marker:13,spinner:3,info:3"

# CTRL-E - word based history search
__hist_word_sel() {

  local cmd='for line in $(fc -l 0 | cut -d '' '' -f 3-); do for word in $line; do echo $word; done; done | sort --unique'
  setopt localoptions pipefail no_aliases 2> /dev/null
  local item
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse --bind=ctrl-z:ignore ${FZF_DEFAULT_OPTS-} ${FZF_CTRL_T_OPTS-}" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  return $ret
}

__fzfcmd() {
  [ -n "${TMUX_PANE-}" ] && { [ "${FZF_TMUX:-0}" != 0 ] || [ -n "${FZF_TMUX_OPTS-}" ]; } &&
    echo "fzf-tmux ${FZF_TMUX_OPTS:--d${FZF_TMUX_HEIGHT:-40%}} -- " || echo "fzf"
}

fzf-history-word-widget() {
  LBUFFER="${LBUFFER}$(__hist_word_sel)"
  local ret=$?
  zle reset-prompt
  return $ret
}
zle -N fzf-history-word-widget
bindkey '^E' fzf-history-word-widget

# Useful aliases

# This one
alias hist-dur='history -iD 0 | fzf'

# Suffixes!
alias -s txt=vim

# Support for GUI clipboard
source $ZDOTDIR/clipboard.zsh

# A separate file that gets sourced; convenient for putting things you may not want to upstream
source $ZDOTDIR/more.zsh

# https://www.reddit.com/r/zsh/comments/ka4sae/navigate_folder_history_like_in_fish/
function my-redraw-prompt() {
  {
    builtin echoti civis
    builtin local f
    for f in chpwd "${chpwd_functions[@]}" precmd "${precmd_functions[@]}"; do
      (( ! ${+functions[$f]} )) || "$f" &>/dev/null || builtin true
    done
    builtin zle reset-prompt
  } always {
    builtin echoti cnorm
  }
}

function my-cd-rotate() {
  () {
    builtin emulate -L zsh
    while (( $#dirstack )) && ! builtin pushd -q $1 &>/dev/null; do
      builtin popd -q $1
    done
    (( $#dirstack ))
  } "$@" && my-redraw-prompt
}

function my-cd-up()      { builtin cd -q .. && my-redraw-prompt; }
function my-cd-back()    { my-cd-rotate +1; }
function my-cd-forward() { my-cd-rotate -0; }

builtin zle -N my-cd-up
builtin zle -N my-cd-back
builtin zle -N my-cd-forward

bindkey -v '^K' my-cd-up
bindkey -v '^H' my-cd-back
bindkey -v '^L' my-cd-forward
bindkey -v '^J' fzf-cd-widget

# To customize prompt, run `p10k configure` or edit $ZDOTDIR/.p10k.zsh.
[[ ! -f "${ZDOTDIR}/.p10k.zsh" ]] || source "${ZDOTDIR}/.p10k.zsh"
