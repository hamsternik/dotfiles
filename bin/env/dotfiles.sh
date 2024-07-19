#!/bin/zsh

# References
# https://github.com/webpro/dotfiles/blob/master/bin/dotfiles
# https://github.com/joshukraine/dotfiles/blob/master/install.sh

CONF_PREFIX="configs"
declare -A dotfiles=(
    ["env/aliases"]=".aliases"
    ["env/bash_profile"]=".bash_profile"
    ["env/bashrc"]=".bashrc"
    ["env/profile"]=".profile"
    ["editorconfig"]=".editorconfig"
    ["finicky"]=".finicky"
    ["fish/config.fish"]=".config/fish/config.fish"
    ["fish/fish_plugins"]=".config/fish/fish_plugins"
#    ["fish/conf.d/fish_aliases.fish"]=".config/fish/conf.d/fish_aliases.fish"
    ["fish/functions/fish_prompt.fish"]=".config/fish/functions/fish_prompt.fish"
    ["git/gitattributes"]=".gitattributes"
    ["git/gitconfig"]=".gitconfig"
    ["git/gitignore_global"]=".gitignore_global"
    ["karabiner/change-lang-one-key.json"]=".config/karabiner/assets/complex_modifications/change-lang-one-key.json"
    ["kitty/]=".config/kitty",
    ["lf/lfrc"]=".config/lf/lfrc"
    ["nvim"]=".config/nvim"
    ["tmux/tmux.conf"]=".tmux.conf"
    ["vim/vim"]=".vim"
    ["vim/vimrc"]=".vimrc"
    ["vscode/keybindings.json"]="Library/Application Support/Code/User/keybindings.json"
    ["zsh/plugins"]=".zsh"
    ["zsh/zshenv"]=".zshenv"
    ["zsh/zshrc"]=".zshrc"
)

function echo_linking {
    local fmt="$1"; shift
    printf "\n⌛️ Linking \`${fmt}\`\n" "$@"
}

function echo_success {
  local fmt="$1"; shift
  printf "[dotfiles] ✅ ${fmt}\n\n" "$@"
}

while [ $# -gt 0 ]; do
case $1 in
    -i | --install)
        printf "⛓ Linking configs into ${HOME}.\n"
        for filepath filename in ${(kv)dotfiles}; do
            SYMLINK_PATH=$(pwd)/$CONF_PREFIX/$filepath
            echo_linking $SYMLINK_PATH
            ln -s -f "$SYMLINK_PATH" "$HOME/$filename" \
                && echo_success "$filepath symlink was created."
        done ;;

        # echo_linking "keybindings.json" # Make `keybindings.json` VSCodium symlink
        # VSCODIUM_KEYBINDINGS_DIR="${HOME}/Library/Application Support/VSCodium/User"
        # if [[ -d "$VSCODIUM_KEYBINDINGS_DIR" ]]; then
        #     ln -s -f -n "$(pwd)/${CONF_PREFIX}/vscode/keybindings.json" \
        #         "${VSCODIUM_KEYBINDINGS_DIR}/keybindings.json" && echo_success "VSCodium keybindings.json symlink created"
        # fi
    
    -u | --uninstall)
        printf "⛓ Unlink config files for the ${USER} user.\n"
        for filename in ${(v)dotfiles}; do
            unlink "$HOME/$filename" \
                && echo_success "✅ Symbolic link detected." \
                || echo "❌ Symlink does not exist already.\n"
        done ;;
esac
shift
done
