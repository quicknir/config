# To get beam shaped cursor on instant prompt startup
echo -ne '\e[5 q'

# Enable Powerlevel10k instant prompt. Should stay close to the top of /spare/ssd_local/nir/zsh/home/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Source powerlevel10k
. "${ZDOTDIR:h}/powerlevel10k/powerlevel10k.zsh-theme"

alias ll='exa -l --icons --group-directories-first --git'
alias less='bat --paging always'
alias cat='bat'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"


# Handy reference, courtesy of https://github.com/seebi/dircolors-solarized
# SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      sRGB        HSB
# --------- ------- ---- -------  ----------- ---------- ----------- -----------
# base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
# base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
# base01    #586e75 10/7 brgreen  240 #4e4e4e 45 -07 -07  88 110 117 194  25  46
# base00    #657b83 11/7 bryellow 241 #585858 50 -07 -07 101 123 131 195  23  51
# base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
# base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
# base2     #eee8d5  7/7 white    254 #d7d7af 92 -00  10 238 232 213  44  11  93
# base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
# yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
# orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
# red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
# magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
# violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
# blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
# cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
# green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60

# Usage: palette
palette() {
    local -a colors
    for i in {000..16}; do
        colors+=("%F{$i}hello: $i%f")
    done
    print -cP $colors
}

# Usage: printc COLOR_CODE
printc() {
    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}

alias vi=vim
alias vim=XDG_CONFIG_HOME='$ZDOTDIR/.. nvim'
# For better vi usability, reduce key delay/timeout
KEYTIMEOUT=1

# Edit command in full blown vim; bound to normal mode C-e
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd "^E" edit-command-line

# rg/fzf interactive search widget
rg_fzf_search_widget() { "${ZDOTDIR}/search.sh" }
zle -N rg_fzf_search_widget
bindkey -M vicmd "s" rg_fzf_search_widget

# fzf setup
export PATH="$PATH:${ZDOTDIR:h}/fzf/bin"
# just for ctrl-r which we use unmodified; we have our own versions
# of ctrl-t and alt-c (and the latter is bound to ctrl-j)
. "${ZDOTDIR:h}/fzf/shell/key-bindings.zsh"

# Exact matching similar to helm
export FZF_DEFAULT_COLORS="\
   --color 16,fg:11,bg:-1,hl:1,hl+:1,bg+:7,fg+:-1:regular:underline \
   --color prompt:4,pointer:13,marker:13,spinner:3,info:3"

export FZF_DEFAULT_OPTS="-e ${FZF_DEFAULT_COLORS}"

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


export FZF_TMUX_OPTS="-p -w 62% -h 38%"
export FZF_CTRL_T_OPTS="--preview-window hidden --ansi --layout reverse-list --preview '__fzf_ls_bat_preview {}' --bind 'ctrl-p:toggle-preview' --bind 'ctrl-h:reload(fd -H --color always)'"
export FZF_ALT_C_OPTS="--preview-window hidden --layout reverse-list --preview '__fzf_ls_preview {}' --bind 'ctrl-p:toggle-preview'"
export FZF_TMUX=1

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
# utility function; open script files in vim if not executable, otherwise execute
__exec_or_vim() {
    if [[ -x $1 ]]; then
        $1
    else
        vim $1
    fi
}
alias -s {sh,zsh,py}=__exec_or_vim
alias -s {txt,json,ini}=vim
alias -s log=bat
alias -s git='git clone'

# Support for GUI clipboard
source $ZDOTDIR/clipboard.zsh

# A separate file that gets sourced; convenient for putting things you may not want to upstream
() { local FILE="$ZDOTDIR/ignore.zsh" && test -f $FILE && . $FILE }

# recent directories
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 1000

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

      #--preview 'bat --color=always {1} --highlight-line{2}'
fzf-rg-widget() {
  RG_PREFIX="rg --line-number --no-heading --color=always --smart-case "
  fzf --ansi \
      --disabled --query "$INITIAL_QUERY" \
      --bind "change:reload:sleep 0.1; eval $RG_PREFIX {q} || true" \
      --delimiter : \
      --preview 'bat --color=always {1} --highlight-line {2}' \
      --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
}
zle -N fzf-rg-widget
bindkey '^S' fzf-rg-widget

# Intuitive back-forward navigation, similar to a browser.
# Also provides up (cd ..), and down (fzf recursive dir search).
# Bound to Ctrl-hjkl
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
bindkey -v '^T' fzf-file-widget

maybe_source "$ZDOTDIR/ignore_rc.zsh"

unsetopt LIST_BEEP

# From prezto
# zpreztrc
autoload zmv
autoload zargs

# environment
setopt COMBINING_CHARS      # Combine zero-length punctuation characters (accents)
                            # with the base character.
setopt INTERACTIVE_COMMENTS # Enable comments in interactive shell.
setopt RC_QUOTES            # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
unsetopt MAIL_WARNING       # Don't print a warning message if a mail file has been accessed.

# Allow mapping Ctrl+S and Ctrl+Q shortcuts
[[ -r ${TTY:-} && -w ${TTY:-} && $+commands[stty] == 1 ]] && stty -ixon <$TTY >$TTY

#
# Jobs
#

setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt HUP              # Don't kill jobs on shell exit.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.

# editor
# by default backspace is vi-delete-char which has some pretty funky behavior
bindkey "^?" backward-delete-char

# history
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.

HISTFILE="${ZDOTDIR}/.zsh_history"  # The path to the history file.
HISTSIZE=10000  # The maximum number of events to save in the internal history.
SAVEHIST=10000  # The maximum number of events to save in the history file.

# directory
setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Write to multiple descriptors.
setopt EXTENDED_GLOB        # Use extended globbing syntax.
unsetopt CLOBBER            # Do not overwrite existing files with > and >>.
                            # Use >! and >>! to bypass.

# utility
# Safe ops. Ask the user before doing anything destructive.
alias cp='cp -i'
alias ln='ln -i'
alias mv='mv -i'
alias rm='rm -i'

# completion
# Load and initialize the completion system ignoring insecure directories with a
# cache time of 20 hours, so it should almost always regenerate the first time a
# shell is opened each day.
autoload -Uz compinit
_comp_path="${XDG_CACHE_HOME:-$HOME/.cache}/prezto/zcompdump"
# #q expands globs in conditional expressions
if [[ $_comp_path(#qNmh-20) ]]; then
  # -C (skip function check) implies -i (skip security check).
  compinit -C -d "$_comp_path"
else
  mkdir -p "$_comp_path:h"
  compinit -i -d "$_comp_path"
  # Keep $_comp_path younger than cache time even if it isn't regenerated.
  touch "$_comp_path"
fi
unset _comp_path

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/prezto/zcompcache"

# We avoid completing user because it's VERY expensive on some setups (and not very useful)
zstyle ':completion:*:*:*:users' users

# fzf-tab recommendations
# disable sort when completing `git checkout`
# could consider extending this to other things; presumably this is because sort gets in the way
# of fzf's async nature
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'

zstyle ':fzf-tab:*' default-color $'\033[93m'
zstyle ':fzf-tab:*' single-color $'\033[93m'

export FZF_TAB_GROUP_COLORS=($'\033[34m' $'\033[31m' $'\033[32m' $'\033[35m' $'\033[36m'
    $'\033[33m' $'\033[95m' $'\033[91m' $'\033[93m')

zstyle ':fzf-tab:*' group-colors $FZF_TAB_GROUP_COLORS

. "${ZDOTDIR:h}/fzf-tab/fzf-tab.plugin.zsh"
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
# Surprisingly, fzf-tab doesn't respect FZF_DEFAULT_OPTS
zstyle ':fzf-tab:*' fzf-flags $(echo $FZF_DEFAULT_OPTS)
zstyle ':fzf-tab:*' prefix ''
zstyle ':fzf-tab:*' accept-line 'ctrl-l'

# Change cursor shape for different vi modes.
# https://unix.stackexchange.com/questions/433273/changing-cursor-style-based-on-mode-in-both-zsh-and-vim
# https://github.com/romkatv/powerlevel10k/issues/2151
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    >$TTY echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    >$TTY echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    >$TTY echo -ne "\e[5 q"
}
zle -N zle-line-init
preexec() {
    >$TTY echo -ne '\e[5 q' ;
} # Use beam shape cursor for each new prompt.

# To customize prompt, run `p10k configure` or edit $ZDOTDIR/.p10k.zsh.
[[ ! -f "${ZDOTDIR}/.p10k.zsh" ]] || source "${ZDOTDIR}/.p10k.zsh"

. "${ZDOTDIR:h}/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
fast-theme -q "${ZDOTDIR:h}/fast-syntax-solarized.ini"

. "${ZDOTDIR:h}/zsh-autosuggestions/zsh-autosuggestions.zsh"
bindkey '^ ' autosuggest-accept
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=14"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
