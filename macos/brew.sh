#!/bin/bash

if [ "$(uname -s)" != "Darwin" ]; then
    echo "Run brew only at macOS system!"
    exit -1
fi

if [[ ! -f '/usr/local/bin/brew' ]] ; then
    echo "===> Installing Homebrew for @$USER user..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew bundle check >/dev/null 2>&1 || {
    echo "===> Installing Homebrew dependecies..."
    brew bundle
}

brew cleanup
