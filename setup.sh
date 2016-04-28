#!/bin/sh

echo "Start to deploy all dotfiles into checked system"

#DOTFILES=".Xresources .aliases .bash_profile .bashrc .gitattributes .gitconfig .gitignore .vimrc .zshenv .zshrc"
FILES=`ls -A | grep "^\."`

for FILE in DOTFILES; do
	cp -r FILE "${HOME}/"
done

