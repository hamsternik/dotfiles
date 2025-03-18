# References
# https://github.com/webpro/dotfiles/blob/master/Makefile
# https://xs-labs.com/en/blog/2020/11/07/introduction-to-makefiles/

# Warning:
# Do not use`rm -rf path/to/dir/` for symlinks applied to directories.
# To remove the dir symlink, only use `rm path/to/dir_symlink` w/o last dash.
# Then you will remove only the symlink on the disk.

# Warning:
# To symlink the dir in macOS (the right way), you need to use `-n` .
# -n flag treats the destination as file, even if it is a dir.

# Warning:
# To suppress errors and skip the command when it fails (returns non-zero exit code):
# Use `-` before a command to ignore errors and continue with next one.
# Use `|| true` in the end of the command to suppress the error.

CURRENTDIR := $(shell pwd)

help:
	@cat Makefile

install-brew:
	brew bundle install --file=Brewfile

## mas, Mac App Store command-line interface, https://github.com/mas-cli/mas
install-mas:
# @echo "Agenda, note taking application focusing on dates"
# mas install 1287445660
	@echo "Bear, beautiful, powerfully simple Markdown app to capture, write, and organize your life."
	mas install 1091189122
# @echo "Flow, a pomodoro based work timer to help you get things done."
# mas install 1423210932
	@echo "PDF Viewer Pro by PSPDFKit"
	mas install 1120099014

## Install Configuration Files
## WARNING! The whole setup is macOS-oriented, no plans to suuport Linux currenlty.

install-all:
	$(MAKE) install-finicky-conf
	$(MAKE) install-fish-conf
	$(MAKE) install-git-conf
	$(MAKE) install-gpg-conf
	$(MAKE) install-karabiner-conf
	$(MAKE) install-kitty-conf
	$(MAKE) install-lf-conf
	$(MAKE) install-nvim-conf
	$(MAKE) install-shell-conf
	$(MAKE) install-ssh-conf
	$(MAKE) install-tmux-conf
	$(MAKE) install-vim-conf
	$(MAKE) install-vscode-conf
	$(MAKE) install-zellij-conf
	$(MAKE) install-zsh-conf

uninstall-all:
	$(MAKE) uninstall-finicky-conf
	$(MAKE) uninstall-fish-conf
	$(MAKE) uninstall-git-conf
# $(MAKE) uninstall-gpg-conf
	$(MAKE) uninstall-karabiner-conf
	$(MAKE) uninstall-kitty-conf
	$(MAKE) uninstall-lf-conf
	$(MAKE) uninstall-nvim-conf
	$(MAKE) uninstall-shell-conf
	$(MAKE) uninstall-ssh-conf
	$(MAKE) uninstall-tmux-conf
	$(MAKE) uninstall-vim-conf
	$(MAKE) uninstall-vscode-conf
	$(MAKE) uninstall-zellij-conf
	$(MAKE) uninstall-zsh-conf

## finicky configs (Utility for customizing which browser to start)

FINICKY_DIR := $(CURRENTDIR)/configs/finicky
install-finicky-conf:
	@$(MAKE) uninstall-finicky-conf
	@echo "\n✨ Installing finicky.js config files."
	ln -s -n $(FINICKY_DIR)/finicky.js ~/.finicky.js

uninstall-finicky-conf:
	rm ~/.finicky.js || true

## fish shell

FISH_DIR := $(CURRENTDIR)/configs/fish
install-fish-conf:
	@$(MAKE) uninstall-fish-conf
	@echo "\n✨ Installing fish shell config files."
	@if [ ! -d "$(HOME)/.config/fish" ]; then echo "❌ ~/.config/fish dir already exists. Exit."; exit 1; fi
	ln -s $(FISH_DIR)/config.fish ~/.config/fish/config.fish
	ln -s $(FISH_DIR)/fish_plugins ~/.config/fish/fish_plugins
# @echo "✨ Installed fish config files. Installing config directories..."
# ln -s -n $(FISH_DIR)/completions ~/.config/fish/completions
# ln -s -n $(FISH_DIR)/conf.d ~/.config/fish/conf.d
# ln -s -n $(FISH_DIR)/functions ~/.config/fish/functions
# ln -s -n $(FISH_DIR)/themes ~/.config/fish/themes

uninstall-fish-conf:
	rm ~/.config/fish/config.fish || true
# rm ~/.config/fish/fish_plugins || true
# rm ~/.config/fish/completions || true
# rm ~/.config/fish/conf.d || true
# rm ~/.config/fish/functions || true
# rm ~/.config/fish/themes || true

## git configs

GIT_DIR := $(CURRENTDIR)/configs/git
install-git-conf:
	@$(MAKE) uninstall-git-conf
	@echo "\n✨ Installing git config files."
	ln -s -n $(GIT_DIR)/gitattributes ~/.gitattributes
	ln -s -n $(GIT_DIR)/gitignore_global ~/.gitignore_global
	ln -s -n $(GIT_DIR)/gitconfig ~/.gitconfig
	ln -s -n $(GIT_DIR)/gitconfig.work ~/.gitconfig.work

uninstall-git-conf:
	rm ~/.gitattributes || true
	rm ~/.gitignore_global || true
	rm ~/.gitconfig || true
	rm ~/.gitconfig.work || true

## gnupg configs

GNUPG_DIR := $(CURRENTDIR)/configs/gnupg
install-gpg-conf:
	@echo "GPG public key(ring) is not used at the moment. Exit."
# brew list gnupg || HOMEBREW_NO_AUTO_UPDATE=1 brew install gnupg
# ln -s -n $(GNUPG_DIR)/gpg-agent.conf ~/.gnupg/gpg-agent.conf
# ln -s -n $(GNUPG_DIR)/gpg.conf ~/.gnupg/gpg.conf

## karabiner configs (Keyboard customiser)

KARABINER_DIR := $(CURRENTDIR)/configs/karabiner
install-karabiner-conf:
	@$(MAKE) uninstall-karabiner-conf
	@echo "\n✨ Installing Karabiner config files."
	ln -s -n $(KARABINER_DIR)/karabiner.json ~/.config/karabiner/karabiner.json
	ln -s -n $(KARABINER_DIR)/change-lang-one-key.json ~/.config/karabiner/assets/complex_modifications/change-lang-one-key.json

uninstall-karabiner-conf:
	rm ~/.config/karabiner/karabiner.json || true
	rm ~/.config/karabiner/assets/complex_modifications/change-lang-one-key.json || true

## kitty configs (GPU-based terminal emulator on Python)

KITTY_DIR := $(CURRENTDIR)/configs/kitty
install-kitty-conf:
	@$(MAKE) uninstall-kitty-conf
	@echo "\n✨ Installing kitty terminal config files."
	ln -s -n $(KITTY_DIR)/kitty.conf ~/.config/kitty/kitty.conf

uninstall-kitty-conf:
	rm ~/.config/kitty/kitty.conf || true

## lf configs (Terminal file manager)

LF_DIR := $(CURRENTDIR)/configs/lf
install-lf-conf:
	@$(MAKE) uninstall-lf-conf
	@echo "\n✨ Installing lf config files."
	ln -s -n $(LF_DIR)/lfrc ~/.config/lf/lfrc

uninstall-lf-conf:
	rm ~/.config/lf/lfrc || true

## NVIM configs

NVIM_DIR := $(CURRENTDIR)/configs/nvim
install-nvim-conf:
	@$(MAKE) uninstall-nvim-conf
	@echo "\n✨ Installing NeoVim config files."
	@if [ ! -d "$$HOME/.config/nvim" ]; then echo "~/.config/nvim configuration dir does not exist. Exit."; exit 1; fi
	ln -s -n $(NVIM_DIR)/init.lua ~/.config/nvim/init.lua
	ln -s -n $(NVIM_DIR)/lua ~/.config/nvim/lua
	ln -s -n $(NVIM_DIR)/plugin ~/.config/nvim/plugin

uninstall-nvim-conf:
	rm ~/.config/nvim/init.lua || true
	rm ~/.config/nvim/lua || true
	rm ~/.config/nvim/plugin || true

## Sublime Text 4 configs

SUBLIME_TEXT_DOTFILES_CONF_DIR := $(CURRENTDIR)/configs/sublime-text
SUBLIME_TEXT_CONF_DIR := "$(HOME)/Library/Application\ Support/Sublime\ Text/Packages/User"
install-sublime-text-conf:
	@$(MAKE) uninstall-sublime-text-conf
	@echo "\n✨ Installing Sublime Text 4 config files."
	@if [ ! -d "$(SUBLIME_TEXT_CONF_DIR)" ]; then echo "❌ Sublime Text 4 is not installed"; exit -1; fi
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Default (OSX).sublime-keymap" "$(SUBLIME_TEXT_CONF_DIR)/Default (OSX).sublime-keymap"
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Package Control.sublime-settings" "$(SUBLIME_TEXT_CONF_DIR)/Package Control.sublime-settings"
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Preferences.sublime-settings" "$(SUBLIME_TEXT_CONF_DIR)/Preferences.sublime-settings"

uninstall-sublime-text-conf:
	rm "$(SUBLIME_TEXT_CONF_DIR)/Default (OSX).sublime-keymap" || true
	rm "$(SUBLIME_TEXT_CONF_DIR)/Package Control.sublime-settings" || true
	rm "$(SUBLIME_TEXT_CONF_DIR)/Preferences.sublime-settings" || true

## basic shell configs

SHELL_DIR := $(CURRENTDIR)/configs/shell
install-shell-conf:
	@$(MAKE) uninstall-shell-conf
	@echo "\n✨ Installing bash & shell config files."
	ln -s -n $(SHELL_DIR)/aliases ~/.aliases
	ln -s -n $(SHELL_DIR)/bashrc ~/.bashrc
	ln -s -n $(SHELL_DIR)/profile ~/.profile

uninstall-shell-conf:
	rm ~/.aliases || true
	rm ~/.bashrc || true
	rm ~/.profile || true

## SSH configuration

SHELL_DIR := $(CURRENTDIR)/configs/ssh
install-ssh-conf:
	@echo "\n✨Installing SSH configuration files."
	ln -s -n $(SHELL_DIR)/config ~/.ssh/config
	@echo "\n✨Check out SSH private keys for the 'ssh-agent':"
	ssh-add -l

uninstall-ssh-conf:
	rm ~/.ssh/config || true


# TMUX config (Terminal multiplexer)

TMUX_DIR := $(CURRENTDIR)/configs/tmux
install-tmux-conf:
	@$(MAKE) uninstall-tmux-conf
	@echo "\n✨ Installing TMUX config files..."
	ln -s -n $(TMUX_DIR)/tmux.conf ~/.tmux.conf

uninstall-tmux-conf:
	rm ~/.tmux.conf || true

## ViM configs (Vi 'workalike' with many additional features)

VIM_DIR := $(CURRENTDIR)/configs/vim
install-vim-conf:
	@$(MAKE) uninstall-vim-conf
	@echo "\n✨ Installing ViM config files."
	@if [ ! -d "$(HOME)/.vim" ]; then echo "~/.vim dir does not exist! Exit."; exit 1; fi
	ln -s -n $(VIM_DIR)/vimrc ~/.vim/.vimrc

uninstall-vim-conf:
	rm ~/.vim/.vimrc || true

## VSCode configs (Open-source code editor)

VSCODE_DIR := $(CURRENTDIR)/configs/vscode
install-vscode-conf:
	@$(MAKE) uninstall-vscode-conf
	@echo "\n✨ Installing VSCode config files."
	ln -s -n $(VSCODE_DIR)/keybindings.json ~/Library/Application\ Support/Code/User/keybindings.json
	ln -s -n $(VSCODE_DIR)/settings.json ~/Library/Application\ Support/Code/User/settings.json

uninstall-vscode-conf:
	rm ~/Library/Application\ Support/Code/User/keybindings.json || true
	rm ~/Library/Application\ Support/Code/User/settings.json || true

## zellij configs (Pluggable terminal workspace, with terminal multiplexer as the base feature)

ZELLIJ_DIR := $(CURRENTDIR)/configs/zellij
install-zellij-conf:
	@$(MAKE) uninstall-zellij-conf
	@echo "\n✨ Installing zellij config files..."
	ln -s -n $(ZELLIJ_DIR)/config.kdl ~/.config/zellij/config.kdl

uninstall-zellij-conf:
	rm ~/.config/zellij/config.kdl || true

doc-zellij:
	@echo "There are 2 config files available, config.kdl and default.config.kdl in the repo."
	@echo "The config.default.kdl is generated by zellij if there is no config files in the ~/.config/zellij dir by-default."

## Zsh configs (shell designed for interactive use, although it is also a powerful scripting language)

ZSH_DIR := $(CURRENTDIR)/configs/zsh
install-zsh-conf:
	@$(MAKE) uninstall-zsh-conf
	@echo "\n✨ Installing Zsh config files..."
	@echo "TBD find zsh-git-prompt plugin or theme with build-in git using antigen PM:"
	@echo "antigen: https://github.com/zsh-users/antigen"
	ln -s -n $(ZSH_DIR)/zshenv ~/.zshenv
	ln -s -n $(ZSH_DIR)/zshrc ~/.zshrc

uninstall-zsh-conf:
	@echo "Uninstalling Zsh config files..."
# rm -rf ~/.zsh/*
	rm ~/.zshenv || true
	rm ~/.zshrc || true

# Misc Targets:

## VSCode Extensions

colorcheck:
	pastel colorcheck
.PHONY: colorcheck

fish-install-plugins:
	@echo "\n✨Preparing fish plugin manager before installing plugins..."
	brew list fisher || HOMEBREW_NO_AUTO_UPDATE=1 brew install fisher
	@echo "TBD parse ~/.config/fish/fish_plugins file, putting all plugins in a line, and provide as arg to the `fisher install <arg>`"
	fisher install edc/bass
	fisher install ilancosman/tide@v6
	fisher install jorgebucaran/nvm.fish
	fisher install sentriz/fish-pipenv

vscode-install-extensions:
	./bin/vscode/vscode-extensions-install

vscode-export-extension-list:
	./bin/vscode/vscode-extensions-list-export
.PHONY: export-vscode-extension-list

terminal-macos-doc:
	@echo "Use Atom.terminal theme in the Termainl.app -> Settings -> Profiles -> More -> Import..."
