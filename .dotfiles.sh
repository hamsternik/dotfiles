#!/bin/bash

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
)

dotfiles_echo() {
  local fmt="$1"; shift
  # shellcheck disable=SC2059
  printf "[dotfiles] ${fmt}\\n" "$@"
}

function install_dotfiles {
    dotfiles_echo "-> 🚀🚀🚀 Linking basic dotfiles."
    sleep 1 ### personal UI decision.
    for file in "${config_files[@]}"; do
        SOURCE=$(pwd)/configs/${file}
        dotfiles_echo "✅ Linking .${file}"
        if [[ "${file}" == "lfrc" ]]; then
            if [[ ! -d "${HOME}/.config/lf" ]]; then
                mkdir -p "${HOME}"/.config/lf
            fi
            ln -nfs "${SOURCE}" "${HOME}/.config/lf/.${file}"
        else
            ln -nfs "${SOURCE}" "${HOME}/.${file}"
        fi
    done
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
