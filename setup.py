from pathlib import Path
from itertools import count
import subprocess


def bak(dest: Path) -> Path:
    name = dest.name
    for i in count():
        new_path = dest.with_name(f"{name}.bak_{i}")
        if not new_path.exists() and not new_path.is_symlink():
            break
    dest.rename(new_path)
    return new_path


def symlink_and_bak(src: Path, dest: Path):
    if dest.exists() or dest.is_symlink():
        backup_path = bak(dest)
        print(f"Backing up existing file at {dest} to {backup_path}")
    dest.parent.mkdir(exist_ok=True, parents=True)
    dest.symlink_to(src)


def setup_config(repo_path: Path):
    print("Setup zsh")
    symlink_and_bak(repo_path / "terminal/zdotdir/.zshenv", Path("~/.zshenv").expanduser())

    print("Setup tmux")
    symlink_and_bak(repo_path / "tmux/.tmux.conf", Path("~/.tmux.conf").expanduser())

    print("Installing fzf")
    subprocess.run(["./install", "--bin"], cwd=(repo_path/"terminal/fzf"))

    print("Add zsh fast syntax highlighting symlink")
    symlink_and_bak(repo_path / "terminal", Path("~/.fsh").expanduser())

    # Install fonts; come back to this
    #symlink_and_bak(path.join(repo_path, 'fonts/Input'), path.expanduser('~/.fonts/Input'))


if __name__ == "__main__":
    setup_config(Path(__file__).parent.resolve())
