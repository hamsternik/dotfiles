#!/bin/bash

if [[ ! -f '/usr/local/bin/brew' ]] ; then
    echo "Will install Homebrew for local user...\n"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle
brew cleanup
