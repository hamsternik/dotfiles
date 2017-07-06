#!/bin/bash

# Make Homebrew update
brew update

# Make upgrade of all already-installed formulae
brew upgrade --all

# Emacs installation
brew install --with-cocoa --srgb emacs
# brew linkapp emacs    ### Don't use it! It disturbs to install Emacs.app into Application

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

# Cask install
brew cask install emacs

# Remove outdate data
brew cleanup
