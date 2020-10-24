#!/bin/sh

if command -v brew >/dev/null 2>&1; then
	brew tap | grep -q 'getantibody/tap' || brew tap getantibody/tap
	brew install antibody
fi

antibody bundle <"$DOTFILES/antibody/plugins.txt" >~/.zsh_plugins.sh
antibody update
