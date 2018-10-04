#!/bin/bash

# ---------------------- DEPENDENCY SCOPE ---------------------- #
CORE_APPS="
ansible
appledoc
carthage
ctags
diff-so-fancy
htop
tree
mas
mosh
openssl
watchman
wget
zsh
"

LANGS="
cocoapods
python3 
mono 
rbenv
ruby
ruby-build
ghc
cabal-install
stack
" 

JAVA_DEPEND="
gradle
maven
kotlin
"

CASK_APPS="
docker
dropbox
fastlane
google-chrome
iterm2
java
qlmarkdown
soundcleod
vlc
zeplin
"
# ---------------------- SCOPE END ---------------------- #

# ---------------------- FUNCTION SCOPE ---------------------- #
function brew_install {
    ITEMS=$1
    for ITEM in $ITEMS; do
        if ! [[ "$(brew ls --versions $ITEM)" ]] > /dev/null; then
            brew install $ITEM
        else
            echo "[$ITEM] binary already installed via brew."
        fi  
    done
}

function brew_cask_install {
    ITEMS=$1
    for ITEM in $ITEMS; do
        brew cask install $ITEM
    done
    brew cask cleanup
}
# ---------------------- SCOPE END ---------------------- #


# ---------------------- MAIN SCOPE ---------------------- #
brew_readlink=`readlink $(which brew)`
if ! [[ $brew_readlink ]] ; then 
    echo "Will install Homebrew for local user."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else 
    brew update
    brew upgrade --all
fi

brew_cask_install "$CASK_APPS"

brew_install "$CORE_APPS" 
brew_install "$LANGS"
brew_install "$JAVA_DEPEND"

### Install: Ruby
# brew unlink ruby && brew link ruby # Link the latest ruby version
# brew link --overwrite ruby # If ruby won't be linked and setuped as default in the system, run this

# Remove Outdate Data
brew cleanup
# ---------------------- SCOPE END ---------------------- #

