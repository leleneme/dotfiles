#!/usr/bin/env python

import subprocess

def get_link_targets() -> list[tuple[str, str]]:
    return [("./emacs/", "~/.config")]

def symlink(source: str, target: str) -> tuple[bool, str]:
    sp = subprocess.run(['ln', '-s', source, target], capture_output=True)
    return (sp.returncode == 0, sp.stdout.decode('utf-8'))

def main():
    targets = get_link_targets()
    for (source, target) in targets:
        (ok, err) = symlink(source, target)
        if not ok:
            print(f'Symlink failed: {err}')

if __name__ == '__main__':
    main()