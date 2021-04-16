#!/bin/sh

if command -v brew >/dev/null 2>&1; then
	brew tap | grep -q 'getantibody/tap' || brew tap getantibody/tap
	brew install antibody
fi

ABSOLUTE_PATH=$PWD
CURRENT_DIR=`basename $ABSOLUTE_PATH`

if [[ $CURRENT_DIR != "antibody" ]]; then
    ABSOLUTE_PATH="$ABSOLUTE_PATH/antibody"
fi

antibody bundle < "$ABSOLUTE_PATH/plugins.txt" > ~/.zsh_plugins.sh
antibody update
