location=$(readlink -f ${(%):-%N})
export ZDOTDIR=${location:h}
unset location

# Dir bookmarks are potentially used in previewing recent dirs
hash -d config="${ZDOTDIR:h:h}"

# Prevents duplicate entries in PATH
typeset -U path PATH

# Uses the config repo's built in mamba setup - devtools env gives us access to things like eza and bat
# needed in env not rc because fzf previews launch in a non-interactive subshell
path[1,0]=~config/micromamba/envs/devtools/bin

# Useful to define editor "early" as it may be used by other code to set defaults, e.g. tmux
export EDITOR=vim
export VISUAL=vim

export TERM="xterm-256color"

export XDG_CONFIG_HOME=~config/xdg_config_home

# fzf previews run in a fresh, non-interactive shell, so anything needed by preview code
# should be defined here
# Typically we use ls (or eza) to preview dirs, and cat (or bat) to preview
# files. So any settings related to these should be here.
# Further, any reload commands are also executed similarly, so any functions called in a
# reload binding also need to be defined in zshenv.

__dir_entries() {
  local cmd1="cdr -l | tr -s ' ' | cut -d ' ' -f 2-"
  local cmd="fd --type d $1"
  eval "{ $cmd1 & $cmd }"
}

# This is done like this just to avoid needing the dependency on vivid everywhere
# to change the ls colors theme, use vivid generate solarized-light.yml > ls_colors.txt
export LS_COLORS=$(cat "${ZDOTDIR:h}/ls_colors.txt")

export BAT_THEME='Solarized (light)'


__fzf_ls_preview() {
    local d=${~1}
    eza --icons --group-directories-first --colour=always $d
}

__fzf_ls_bat_preview() {
    local d=${~1}
    if [[ -d $d ]]; then
        eza --icons --group-directories-first --color=always $d
    else
        bat --color=always --style numbers,grid $d
    fi
}

# A useful function and we already need it so may as well define it here
maybe_source () {
    test -f $1 && . $1
}

# If not using the built in micromamba setup ignore_env.zsh needs to provide
# the environment for finding eza and bat.
# It may also contain machine specific directory bookmarks; they need to be defined there
# for previews to work correctly (otherwise, fzf's non-interactive preview subshell will
# not recognize the bookmark and the preview will fail)
maybe_source "$ZDOTDIR/ignore_env.zsh"
