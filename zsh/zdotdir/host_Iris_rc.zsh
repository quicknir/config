export MAMBA_ROOT_PREFIX=/home/nir/micromamba
eval "$(~nikud/micromamba/bin/micromamba shell hook --shell zsh)" 
# Try activating main; if not available error silently
micromamba activate main 2> /dev/null
