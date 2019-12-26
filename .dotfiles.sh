#!/bin/bash

# ---------------------- INPUT SCOPE ---------------------- #
HOMEPATH="${HOME}"
FILES="
.aliases
.gitattributes
.gitconfig
.gitignore_global
.lfrc
.tmux.conf
.vim
.vimrc
.zsh
.zshenv
.zshrc
"
# ---------------------- END SCOPE ---------------------- #

# ---------------------- FUNCTION SCOPE ---------------------- #
GREEN='\033[0;32m'
CYAN='\033[0;36m'
RESET='\033[0m'

function log_start {
    local RESULT=""
    if [[ -n "$1" ]]; then
        RESULT="${CYAN}Start${RESET} ${GREEN}${1}${RESET} ${CYAN}Scope${RESET}"
    else
        RESULT="${CYAN}Start Scope${RESET}"
    fi
    echo -e "----------------------- ${RESULT} ------------------------"
}

function log_finish {
    local RESULT=""
    if [[ -n "$1" ]]; then
        RESULT="${CYAN}Finish${RESET} ${GREEN}${1}${RESET} ${CYAN}Scope${RESET}"
    else
        RESULT="${CYAN}Finish Scope${RESET}"
    fi
    echo -e "------------------------ ${RESULT} ------------------------"; echo
}

function install_dotfiles {
    log_start "${FUNCNAME[0]}"
    ITEMS=$1
    for ITEM in $ITEMS; do
        SOURCE=$(pwd)/$ITEM
        TARGET=$HOMEPATH/$ITEM

        if [[ -e $TARGET ]]; then
            printf "%s is exist. Skipping...\n" "$ITEM"
        elif [[ "$ITEM" == ".lfrc" ]]; then
            if [[ ! -d "$HOMEPATH/.config/lf" ]]; then
                mkdir -p "$HOMEPATH"/.config/lf
            fi
            ln -v -s "$SOURCE" "$HOMEPATH/.config/lf/lfrc"
        else
            ln -v -s "$SOURCE" "$TARGET"
            printf "%s symlink was created successfully!\n" "$ITEM"
        fi
    done
    log_finish "${FUNCNAME[0]}"
}

function uninstall_dotfiles {
    log_start "${FUNCNAME[0]}"
    ITEMS="$FILES"
    for ITEM in $ITEMS; do
        TARGET=$HOMEPATH/$ITEM

        if [[ -L $TARGET ]]; then
            unlink "$TARGET"
            printf "%s -- symlink REMOVED from %s\n" "$ITEM" "$HOMEPATH"
        elif [[ "$ITEM" == ".lfrc" ]]; then
            if [[ -d "$HOMEPATH/.config/lf" ]]; then
                unlink "$HOMEPATH/.config/lf/lfrc"
                rm -rf "$HOMEPATH/.config"
                printf "%s -- .lfrc symlink & .config folder REMOVED from %s\n" "$ITEM" "$HOMEPATH"
            else
            	printf "%s -- .lfrc symlink does not exist at %s/.config/lf directory\n" "$ITEM" "$HOMEPATH"
            fi
        else
            printf "%s -- symlink does not exist at %s\n" "$ITEM" "$HOMEPATH"
        fi
    done
    log_finish "${FUNCNAME[0]}"
}
# ---------------------- END SCOPE ---------------------- #


# ---------------------- MAIN: EXECUTION ---------------------- #
while [ $# -gt 0 ]; do
    case $1 in
        -i | --install)
            install_dotfiles "$FILES" ;;
        -u | --uninstall)
            uninstall_dotfiles ;;
    esac
    shift
done

# Sources:
# 1. https://github.com/samoshkin/dotfiles/blob/master/install.sh
