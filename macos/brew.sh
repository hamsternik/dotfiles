#!/bin/bash

# Make Homebrew update
brew update

# Make upgrade of all already-installed formulae
brew upgrade --all

# Installation
brew install appledoc \
     carthage \
     emacs \
     htop \
     maven \
     mono \
     openssl \
     python3 \
     tree \
     watchman

# Remove outdate data
brew cleanup
