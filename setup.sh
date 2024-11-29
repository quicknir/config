#!/bin/bash

function symlink_and_bak() {
  local src="$1"
  local dest="$2"
  if [[ -e $dest ]] ; then
    for ((i=0; ; i++)); do 
      local backup="${dest}.bak_${i}" 
      if [[ ! -e $backup ]] ; then
        break;
      fi;
    done
    mv "${dest}" "${backup}"
  fi
  ln "$src" "$dest"
}

local repo_dir=$(dirname $(realpath "$0"))
symlink_and_bak "${repo_dir}/zdotdir/.zshenv" "${HOME}/.zshenv"
local mamba_dir="${repo_dir}/micromamba"
curl -Ls https://micro.mamba.pm/api/micromamba/linux-64/latest | tar -xvj -C "$mamba_dir" bin/micromamba
"${mamba_dir}/bin/micromamba" -r "$mamba_dir" create -f "${mamba_dir}/devtools.yaml" -y
echo Install fonts - Jetbrains Mono Nerd Font from https://www.nerdfonts.com/font-downloads