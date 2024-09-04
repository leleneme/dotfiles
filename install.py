#!/usr/bin/env python

import subprocess
import os

def get_link_targets() -> list[tuple[str, str]]:
    return [
        ("emacs", "~/.config/"),
        ("alacritty", "~/.config/"),
        ("bash/rc.bash", "~/.bashrc"),
    ]

def symlink(source: str, target: str) -> tuple[bool, str]:
    sp = subprocess.run(["ln", "-sf", source, target], capture_output=True)
    return (sp.returncode == 0, sp.stderr.decode("utf-8"))

def abspath(path: str) -> str:
    return os.path.abspath(os.path.expanduser(path))

def main():
    targets = get_link_targets()
    targets = map(lambda t: map(abspath, t), targets)

    for source, target in targets:
        print(f"Linking {source} to {target}")
        (ok, err) = symlink(source, target)
        if not ok:
            print(f"Symlink failed: {err}")

if __name__ == "__main__":
    main()
