#!/bin/bash

# Make Homebrew update
brew update

# Make upgrade of all already-installed formulae
brew upgrade --all

# Installation
brew install appledoc \
    carthage \
    diff-so-fancy \
    gradle \
    htop \
    tree \
    maven \
    mono \
    openssl \
    python3 \
    watchman

# Adding Taps
brew tap caskroom/cask

### Ruby
brew install ruby 
brew unlink ruby && brew link ruby # Link the latest ruby version
# brew link --overwrite ruby # If ruby won't be linked and setuped as default in the system, run this

### Emacs
# brew install --with-cocoa --srgb emacs
# brew linkapp emacs    ### Don't use it! It disturbs to install Emacs.app into Application
# brew cask install emacs # Cask install

# Remove outdate data
brew cleanup
