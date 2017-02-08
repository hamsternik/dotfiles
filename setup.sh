#!/bin/sh

#============ SETUP ============#

HOMEPATH="${HOME}/test"
FILES="
.aliases
.gitattributes
.gitconfig
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

        echo "$ITEM: Start to deploy"

        if [[ -e $TARGET ]]; then
            echo "$ITEM is exist. Skipping...\n"
        else
            ln -v -s "$SOURCE" "$TARGET"
            echo "$ITEM symlink was created successfully!\n"
        fi
    done
}

#============ MAIN ============#

echo "Start deploy..."
deploy "$FILES"

