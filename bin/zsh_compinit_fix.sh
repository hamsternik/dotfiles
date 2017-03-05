#!/bin/sh

# Script need for next case (this version for macOS).
# "zsh compinit: insecure directories, run compaudit for list."
# "Ignore insecure directories and continue [y] or abort compinit [n]?"

cd /usr/local/share/
sudo chmod -R 755 zsh
sudo chown -R root:staff zsh


