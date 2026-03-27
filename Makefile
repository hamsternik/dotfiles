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
HOMEDIR := $(HOME)

help:
	@cat Makefile

install-brew:
	brew bundle install --file=Brewfile

## Batch mode - test if config loads without errors.
emacs-lint:
	emacs --batch -l configs/emacs/init.el

## mas 
### 📦 Mac App Store command line interface
mas-list-apps:
	@echo "Here is the list of all macOS apps installed via App Store:"
	mas list

mas-install-bear:
	@echo "Bear, beautiful, powerfully simple Markdown app to capture, write, and organize your life."
	mas install 1091189122

mas-install-pdf:
	@echo "PDF Viewer Pro by PSPDFKit"
	mas install 1120099014

## Install Configuration Files
# WARNING! The whole setup is macOS-oriented, no plans to suuport Linux currenlty.

install-all:
	$(MAKE) install-finicky-conf
	$(MAKE) install-fish-conf
	$(MAKE) install-git-conf
	$(MAKE) install-gpg-conf
	$(MAKE) install-karabiner-conf
	$(MAKE) install-nvim-conf
	$(MAKE) install-shell-conf
	$(MAKE) install-ssh-conf
	$(MAKE) install-tmux-conf
	$(MAKE) install-vim-conf
	$(MAKE) install-vscode-conf
	$(MAKE) install-zed-conf
	$(MAKE) install-zellij-conf
	$(MAKE) install-zsh-conf

uninstall-all:
	$(MAKE) uninstall-finicky-conf
	$(MAKE) uninstall-fish-conf
	$(MAKE) uninstall-git-conf
# $(MAKE) uninstall-gpg-conf
	$(MAKE) uninstall-karabiner-conf
	$(MAKE) uninstall-nvim-conf
	$(MAKE) uninstall-shell-conf
	$(MAKE) uninstall-ssh-conf
	$(MAKE) uninstall-tmux-conf
	$(MAKE) uninstall-vim-conf
	$(MAKE) uninstall-vscode-conf
	$(MAKE) uninstall-zed-conf
	$(MAKE) uninstall-zellij-conf
	$(MAKE) uninstall-zsh-conf

# https://github.com/johnste/finicky
# A macOS app for customizing which browser to start
FINICKY_SOURCE := $(CURRENTDIR)/configs/finicky
FINICKY_DEST := $(HOMEDIR)
install-finicky-conf:
	@$(MAKE) uninstall-finicky-conf
	@echo "\nInstalling finicky configuration 🚀"
	ln -s -f $(FINICKY_SOURCE)/finicky.js $(FINICKY_DEST)/.finicky.js

uninstall-finicky-conf:
	rm ~/.finicky.js || true

## fish shell

FISH_DIR := $(CURRENTDIR)/configs/fish
install-fish-conf:
	@$(MAKE) uninstall-fish-conf
	@echo "\nInstalling fish shell config files. ✨"
	@if [ ! -d "$(HOME)/.config/fish" ]; then echo "❌ ~/.config/fish dir does not exist. Exit."; exit 1; fi
	ln -s -f $(FISH_DIR)/config.fish ~/.config/fish/config.fish
	ln -s -f $(FISH_DIR)/fish_plugins ~/.config/fish/fish_plugins
# @echo "✨ Installed fish config files. Installing config directories..."
# ln -s -n $(FISH_DIR)/completions ~/.config/fish/completions
# ln -s -n $(FISH_DIR)/conf.d ~/.config/fish/conf.d
# ln -s -n $(FISH_DIR)/functions ~/.config/fish/functions
# ln -s -n $(FISH_DIR)/themes ~/.config/fish/themes

uninstall-fish-conf:
	rm ~/.config/fish/config.fish || true
	rm ~/.config/fish/fish_plugins || true
# rm ~/.config/fish/completions || true
# rm ~/.config/fish/conf.d || true
# rm ~/.config/fish/functions || true
# rm ~/.config/fish/themes || true

## git configs

GIT_SOURCE := $(CURRENTDIR)/configs/git
GIT_DEST := $(HOMEDIR)
install-git-conf:
	@$(MAKE) uninstall-git-conf
	@echo "\nInstalling git configuration 🚀"
	ln -s -f $(GIT_SOURCE)/gitattributes $(GIT_DEST)/.gitattributes
	ln -s -f $(GIT_SOURCE)/gitconfig $(GIT_DEST)/.gitconfig
	ln -s -f $(GIT_SOURCE)/gitconfig.work $(GIT_DEST)/.gitconfig.work
	ln -s -f $(GIT_SOURCE)/gitignore_global $(GIT_DEST)/.gitignore_global

uninstall-git-conf:
	rm ~/.gitattributes || true
	rm ~/.gitconfig || true
	rm ~/.gitconfig.work || true
	rm ~/.gitignore_global || true

## gnupg configs

GNUPG_DIR := $(CURRENTDIR)/configs/gnupg
install-gpg-conf:
	@echo "GPG public key(ring) is not used at the moment. Exit."
# brew list gnupg || HOMEBREW_NO_AUTO_UPDATE=1 brew install gnupg
# ln -s -n $(GNUPG_DIR)/gpg-agent.conf ~/.gnupg/gpg-agent.conf
# ln -s -n $(GNUPG_DIR)/gpg.conf ~/.gnupg/gpg.conf

## karabiner configs (Keyboard customiser)

KARABINER_SOURCE := $(CURRENTDIR)/configs/karabiner
KARABINER_DEST := $(HOME)/.config/karabiner
install-karabiner-conf:
	@$(MAKE) uninstall-karabiner-conf
	@echo "\nInstalling Karabiner configuration 🚀"
	ln -s $(KARABINER_SOURCE)/karabiner.json $(KARABINER_DEST)/karabiner.json
	ln -s $(KARABINER_SOURCE)/change-lang-one-key.json $(KARABINER_DEST)/assets/complex_modifications/change-lang-one-key.json

uninstall-karabiner-conf:
	rm ~/.config/karabiner/karabiner.json || true
	rm ~/.config/karabiner/assets/complex_modifications/change-lang-one-key.json || true

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


## shell configuration, bash-based.
SHELL_SOURCE := $(CURRENTDIR)/configs/shell
SHELL_DEST := $(HOMEDIR)
install-shell-conf:
	@$(MAKE) uninstall-shell-conf
	@echo "\nInstalling bash & shell config files 🚀"
	echo "$(SHELL_SOURCE)/aliases link to $(SHELL_DEST)/.aliases"
	ln -s -f $(SHELL_SOURCE)/aliases $(SHELL_DEST)/.aliases
	ln -s -f $(SHELL_SOURCE)/bashrc $(SHELL_DEST)/.bashrc
	ln -s -f $(SHELL_SOURCE)/profile $(SHELL_DEST)/.profile

uninstall-shell-conf:
	rm ~/.aliases || true
	rm ~/.bashrc || true
	rm ~/.profile || true

## SSH configuration

SSH_SOURCE := $(CURRENTDIR)/configs/ssh
SSH_DEST := $(HOMEDIR)/.ssh
install-ssh-conf:
	@$(MAKE) uninstall-ssh-conf
	@echo "\nInstalling SSH configuration 🚀"
	ln -s -f $(SSH_SOURCE)/config $(SSH_DEST)/config
	@echo "\nChecking out SSH private keys for the 'ssh-agent:"
	ssh-add -l

uninstall-ssh-conf:
	rm ~/.ssh/config || true

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

# TMUX config (Terminal multiplexer)

TMUX_DIR := $(CURRENTDIR)/configs/tmux
install-tmux-conf:
	@$(MAKE) uninstall-tmux-conf
	@echo "\n✨ Installing TMUX config files..."
	mkdir -p ~/.config/tmux || true
	ln -s -n $(TMUX_DIR)/tmux.conf ~/.config/tmux/tmux.conf

uninstall-tmux-conf:
	rm ~/.config/tmux/tmux.conf || true

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

## Xcode keybindings
XCODE_DIR := $(CURRENTDIR)/configs/xcode
XCODE_KEYBINDINGS_DESTINATION := $(HOME)/Library/Developer/Xcode/UserData/KeyBindings
install-xcode-keybinds:
	@$(MAKE) uninstall-xcode-keybinds
	@echo "\nInstalling Xcode custom key bindings 🚀"
	mkdir -p $(XCODE_KEYBINDINGS_DESTINATION)
	ln -s -n $(XCODE_DIR)/custom.idekeybindings $(XCODE_KEYBINDINGS_DESTINATION)/custom.idekeybindings

uninstall-xcode-keybinds:
	rm $(XCODE_KEYBINDINGS_DESTINATION)/custom.idekeybindings || true

## Zed editor configs

ZED_EDITOR := $(CURRENTDIR)/configs/zed
install-zed-conf:
	@$(MAKE) uninstall-zed-conf
	@echo "\n✨ Installing Zed editor config files."
	ln -s -n $(ZED_EDITOR)/settings.json ~/.config/zed/settings.json
	ln -s -n $(ZED_EDITOR)/keymap.json ~/.config/zed/keymap.json

uninstall-zed-conf:
	rm ~/.config/zed/settings.json || true
	rm ~/.config/zed/keymap.json || true

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
.PHONY: fish-install-plugins

vscode-install-extensions:
	./bin/vscode/vscode-extensions-install
.PHONY: vscode-install-extensions

vscode-export-extension-list:
	./bin/vscode/vscode-extensions-list-export
.PHONY: export-vscode-extension-list

terminal-macos-doc:
	@echo "Use Atom.terminal theme in the Termainl.app -> Settings -> Profiles -> More -> Import..."
.PHONY: terminal-macos-doc





