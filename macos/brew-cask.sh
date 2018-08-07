#!/bin/bash

if ! [[ $(brew tap | grep -i phinze/cask) ]] ; then
    echo "Will tap _phinze/homebrew-cask_ repo"
    brew tap phinze/homebrew-cask
fi

brew upgrade brew-cask

brew cask install \
    docker \
    dropbox \
    google-chrome \
    iterm2 \
    java \
    qlmarkdown \
    soundcleod \
    vlc \
    zeplin

brew cask cleaup

