#!/bin/bash

CONF_PREFIX="configs"
config_files=(
"aliases"
"editorconfig"
"gitattributes"
"gitconfig"
"gitignore_global"
"lfrc"
"tmux.conf"
"vimrc"
"zshenv"
"zshrc"
"vscode/keybindings.json"
)

dotfiles_echo() {
  local fmt="$1"; shift
  # shellcheck disable=SC2059
  printf "[dotfiles] ${fmt}" "$@"
}

function install_dotfiles {
    dotfiles_echo "🚀🚀🚀 Linking configs into ${HOME}"
    sleep 1 ### for better UI perception
    for file in "${config_files[@]}"; do
        SOURCE=$(pwd)/$CONF_PREFIX/${file}
        dotfiles_echo "Linking .${file}...\n"
        if [[ "${file}" == "lfrc" ]]; then
            if [[ ! -d "${HOME}/.config/lf" ]]; then
                mkdir -p "${HOME}"/.config/lf
            fi
            ln -nfs "${SOURCE}" "${HOME}/.config/lf/.${file}"
            dotfiles_echo "Symlink created ✅\n"
        else
            ln -nfs "${SOURCE}" "${HOME}/.${file}"
            dotfiles_echo "Symlink created ✅\n"
        fi
    done

    # Make .vim folder symlink
    ln -nfs "$(pwd)/configs/vim" "${HOME}/.vim"
    dotfiles_echo ".vim folder symlink created ✅\n"

    # Make `.lfrc` symlink
    #ln -s -f -n "$(pwd)/$CONF_PREFIX/"

    # Make `keybindings.json` VSCodium symlink
    ln -s -f -n "$(pwd)/$CONF_PREFIX/vscode/keybindings.json" "${HOME}/Library/Application\
        Support/VSCodium/User/keybindings.json"
    dotfiles_echo "VSCode::keybindings.json file symlink created ✅\n"
}

function uninstall_dotfiles {
    dotfiles_echo "-> 🚀🚀🚀 Linking basic dotfiles."
    sleep 1 ### personal UI decision.
    for file in "${config_files[@]}"; do
        if [[ -L "${HOME}/.${file}" ]]; then
            dotfiles_echo "☑️  Symbolic link detected. Removing..."
            rm -rf "${HOME}/.${file}"
        elif [[ -L "${HOME}/.config/lf/.${file}" ]]; then
            dotfiles_echo "☑️  Symbolic link detected. Removing..."
            rm -rf "$HOMEPATH/.config/lf"
        fi
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
