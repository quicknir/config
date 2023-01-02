# Useful to define editor "early" as it may be used by other code to set defaults
export EDITOR=vim
export VISUAL=vim

# fzf previews run in a fresh, non-interactive shell, so anything needed by preview code
# should be defined here

# Dir bookmarks are potentially used in previewing recent dirs
hash -d config=/spare/ssd_local/nir/config
hash -d gpfs=/n/gpfs0/mosaic/nir
hash -d hd=/spare/local/nir
hash -d pol=/spare/ssd_local/nir/code/dev/polygon
hash -d src=/spare/ssd_local/nir/code/dev
hash -d ssd=/spare/ssd_local/nir
hash -d vwap=/apps/mosaic/research/data/vwap

# Typically we use ls (or exa) to preview dirs, and cat (or bat) to preview
# files. So any settings related to these should be here.
export TERM="xterm-256color"
eval $(eval "dircolors ${ZDOTDIR:h}/dircolors-solarized/dircolors.ansi-light")

export BAT_THEME='Solarized (light)'

fzf_ls_preview() {
    local d=${~1}
    ls --color=always $d
}

# A useful function and we already need it so may as well define it here
maybe_source () {
    test -f $1 && . $1
}

# Adds PATH entries that we may need in scripts
maybe_source "$HOME/.cargo/env"
# May add e.g. machine-specific dir bookmarks
maybe_source "$ZDOTDIR/ignore_env.zsh"
