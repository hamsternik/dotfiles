#!/bin/sh

#============ SETUP ============#

HOMEPATH="${HOME}"
FILES="
.aliases
.gitattributes
.gitconfig
.gemrc
.vim
.vimrc
.zsh
.zshenv
.zshrc
"

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

