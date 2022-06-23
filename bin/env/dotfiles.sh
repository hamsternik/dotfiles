#!/bin/bash

CONF_PREFIX="configs"
config_files=(
"aliases"
"editorconfig"
"gitattributes"
"gitconfig"
"gitignore_global"
"tmux.conf"
"vimrc"
"zshenv"
"zshrc"
)

function echo_linking {
    local fmt="$1"; shift
    printf "\n...Linking \`${fmt}\`\n" "$@"
}

function echo_success {
  local fmt="$1"; shift
  printf "[dotfiles] ✅ ${fmt}\n" "$@"
}

function install_dotfiles {
    printf "🚀🚀🚀 Linking configs into ${HOME}\n"
    for file in "${config_files[@]}"; do
        SOURCE=$(pwd)/${CONF_PREFIX}/${file}
        echo_linking ".${file}"
        ln -s -f -n "${SOURCE}" "${HOME}/.${file}" \
            && echo_success "${file} symlink created"
    done

    echo_linking ".vim folder" # Make .vim folder symlink
    ln -s -f -n "$(pwd)/configs/vim" "${HOME}/.vim" \
        && echo_success ".vim folder symlink"

    echo_linking "lfrc" # Make `lfrc` symlink
    if [[ ! -d "${HOME}/.config/lf" ]]; then mkdir -p "${HOME}"/.config/lf; fi
    ln -s -f -n "$(pwd)/${CONF_PREFIX}/lfrc" "${HOME}/.config/lf/lfrc" && \
        echo_success "lfrc symlink created"

    echo_linking "keybindings.json" # Make `keybindings.json` VSCodium symlink
    VSCODIUM_KEYBINDINGS_DIR="${HOME}/Library/Application Support/VSCodium/User"
    if [[ -d "$VSCODIUM_KEYBINDINGS_DIR" ]]; then
        ln -s -f -n "$(pwd)/${CONF_PREFIX}/vscode/keybindings.json" \
            "${VSCODIUM_KEYBINDINGS_DIR}/keybindings.json" \
                && echo_success "VSCodium keybindings.json symlink created"
    fi
}

function uninstall_dotfiles {
    printf "Unlink all config files for ${USER} user"
    for file in "${config_files[@]}"; do
        unlink "${HOME}/.${file}" \
            && echo_success "Symbolic link detected. Removing..." \
            || echo "😬Sumlink does not exist already"
    done
}

while [ $# -gt 0 ]; do
    case $1 in
        -i | --install)
            install_dotfiles ;;
        -u | --uninstall)
            uninstall_dotfiles ;;
    esac
    shift
done

# References
# https://github.com/webpro/dotfiles/blob/master/bin/dotfiles
# https://github.com/joshukraine/dotfiles/blob/master/install.sh
