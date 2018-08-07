#!/bin/bash

brew_readlink=`readlink $(which brew)`
if ! [[ $brew_readlink ]] ; then 
    echo "Will install Homebrew for local user."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Make Homebrew update
brew update

# Make upgrade of all already-installed formulae
brew upgrade --all

# Installation
brew install \
    appledoc \
    carthage \
    ctags \
    diff-so-fancy \
    gradle \
    htop \
    kotlin \
    tree \
    mas \
    maven \
    mono \
    mosh \
    openssl \
    python3 \
    ruby \
    watchman \
    wget

# Remove outdate data
brew cleanup

### Ruby
# brew unlink ruby && brew link ruby # Link the latest ruby version
# brew link --overwrite ruby # If ruby won't be linked and setuped as default in the system, run this

