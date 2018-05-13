#!/bin/bash

brew tap caskroom/cask
brew install brew-cask

brew update 
brew upgrade brew-cask

brew cask install \
    docker \
    google-chrome \
    iterm2 \
    java \
    qlmarkdown \
    soundcleod \
    vlc

brew cask cleaup

