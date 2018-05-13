#!/bin/bash

#============ SETUP ============#

HOMEPATH="${HOME}"
FILES="
.aliases
.emacs.d
.gitattributes
.gitconfig
.gitignore_global
.gemrc
.vim
.vimrc
.zsh
.zshenv
.zshrc
.ssh/config
"

SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

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

    if [[ -f $SCRIPT_DIR/xterm-256color.terminfo ]]; then
        echo "TERMINFO for xterm will be compiled."
        tic $SCRIPT_DIR/xterm-256color.terminfo
    fi
}

#============ MAIN ============#

echo "Start to create @bin folder localy."
if [[ ! -d $HOME/.bin ]]; then
    mkdir $HOME/.bin
    echo "@.bin directory was created successfuly.\n"
else
    echo "@.bin directory was created before. Aborted\n"
fi

echo "Start to deploy...\n"
deploy "$FILES"

