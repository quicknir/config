import os
from os import path
import sys


def bak(dest):
    path = dest + ".bak"
    i = 0
    num_path = path + str(i)
    while os.path.exists(num_path):
        i += 1
        num_path = path + str(i)
    os.rename(dest, num_path)


def symlink_and_bak(src, dest):
    if os.path.exists(dest):
        bak(dest)
    try:  # Create directory if it doesn't exist. Suppress failure.
        os.makedirs(os.path.split(dest)[0])
    except:
        pass
    os.symlink(src, dest)


def setup_config(repo_path):
    # setup prezto
    prezto_path = path.join(repo_path, "terminal", "prezto")
    prezto_runcom_path = path.join(prezto_path, "runcoms")
    zdotdir_path = path.join(repo_path, "terminal", "zdotdir")

    with open(path.join(prezto_runcom_path, "zshenv")) as f:
        zshenv_data = f.readlines()

    zshenv_data.append("export ZDOTDIR={}\n".format(zdotdir_path))
    zshenv_path = path.expanduser("~/.zshenv")
    if path.exists(zshenv_path):
        bak(zshenv_path)
    with open(zshenv_path, 'w') as f:
        f.writelines(zshenv_data)

    # Setup terminator
    symlink_and_bak(
        path.join(repo_path, "terminal", "terminator", "config"),
        path.expanduser('~/.config/terminator/config'))

    symlink_and_bak(path.join(repo_path, 'spacemacs'), path.expanduser('~/.emacs.d'))
    symlink_and_bak(path.join(repo_path, 'spacemacs.d'), path.expanduser('~/.spacemacs.d'))

    symlink_and_bak(path.join(repo_path, 'fonts/Input'), path.expanduser('~/.fonts/Input'))

    print "Other things to do"
    print "Goto fzf, run install"
    print "Install xcape, xclip, silver surfer"

if __name__ == "__main__":
    setup_config(path.dirname(path.realpath(__file__)))
