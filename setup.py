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
    symlink_and_bak(
        repo_path / "terminal/zdotdir/.zshenv", Path("~/.zshenv").expanduser()
    )

    print("Install fonts - Jetbrains Mono Nerd Font from https://www.nerdfonts.com/font-downloads")

    # Install micromamba
    mm_path = repo_path / "micromamba"
    subprocess.run(
        f"curl -Ls https://micro.mamba.pm/api/micromamba/linux-64/latest | tar -xvj -C {mm_path} bin/micromamba",
        shell=True,
        check=True,
    )

    # Create devtools environment
    subprocess.run(
        [
            f"{mm_path/'bin/micromamba'}",
            "-r",
            str(mm_path),
            "create",
            "-f",
            str(mm_path / "devtools.yaml"),
            "-y",
        ],
        check=True,
    )


if __name__ == "__main__":
    setup_config(Path(__file__).parent.resolve())
