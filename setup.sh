#!/bin/bash

# ---------------------- INPUT SCOPE ---------------------- #
HOMEPATH="${HOME}"
FILES="
.aliases
.gitattributes
.gitconfig
.gitignore_global
.tmux.conf
.vim
.vimrc
.zsh
.zshenv
.zshrc
"
#.ssh/config
# ---------------------- END SCOPE ---------------------- #

# ---------------------- FUNCTION SCOPE ---------------------- #
SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

function echo_finish {
    echo "---------------------- END SCOPE ----------------------"; echo
}

function deploy {
    ITEMS=$1
    for ITEM in $ITEMS; do
        SOURCE=$(pwd)/$ITEM
        TARGET=$HOMEPATH/$ITEM

        if [[ -e $TARGET ]]; then
            echo "$ITEM is exist. Skipping...\n"
        else
            ln -v -s "$SOURCE" "$TARGET"
            echo "$ITEM symlink was created successfully!\n"
        fi
    done

    # TODO: Should be deployed only for OS X system.
    # if [[ -f $SCRIPT_DIR/xterm-256color.terminfo ]]; then
        # echo "TERMINFO for xterm will be compiled."
        # tic $SCRIPT_DIR/xterm-256color.terminfo
    # fi
}

function packages_deploy {
    echo "Run deploy packages for $OSTYPE"
    case "$OSTYPE" in
        darwin*)
            ./macos/brew.sh
            ./macos/brew-cask.sh
            ./macos/gem.sh
        ;;
        linux*)
            bash debian/*.sh
        ;;
    esac
    echo_finish
}

function binary_deploy {
    echo "Run deploy custom scripts/binaries. Store into HOME/.bin directory."
    if [[ ! -d $HOME/.bin ]]; then
        mkdir $HOME/.bin
        echo "@.bin directory was created successfuly.\n"
    else
        echo "@.bin directory was created before.\n"
    fi

    # TODO: Add code to create symlinks for all .sh scripts.
    # TODO: Should be installed with depending on the platform.
    echo_finish
}

function config_deploy {
    echo "Run deploy all dotfiles/config files.\n"
    deploy "$FILES"
    echo_finish
}
# ---------------------- END SCOPE ---------------------- #


# ---------------------- MAIN SCOPE ---------------------- #
packages_deploy
binary_deploy
config_deploy

