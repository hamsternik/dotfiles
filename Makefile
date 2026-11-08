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

help:
	@cat Makefile

install-brew:
	brew bundle install --file=Brewfile

# https://github.com/emacs-mirror/emacs
# EMACS CONFIGURATION

EMACS_SOURCE := $(CURDIR)/configs/emacs
EMACS_DEST := $(HOME)/.config/emacs
# Emacs only reads $(EMACS_DEST) when none of these exist; an empty ~/.emacs.d is enough to shadow it.
EMACS_LEGACY_PATHS := $(HOME)/.emacs $(HOME)/.emacs.el $(HOME)/.emacs.d
install-emacs-conf: uninstall-emacs-conf
	@echo "\nInstalling Emacs configuration 🚀"
	mkdir -p $(EMACS_DEST)
	ln -s -f $(EMACS_SOURCE)/init.el $(EMACS_DEST)/init.el
	ln -s -f $(EMACS_SOURCE)/custom.init.el $(EMACS_DEST)/custom.init.el

uninstall-emacs-conf:
	@for f in $(EMACS_LEGACY_PATHS); do \
		if [ -e "$$f" ] || [ -L "$$f" ]; then \
			echo "Removing $$f (it shadows $(EMACS_DEST))"; \
			rm -rf "$$f"; \
		fi; \
	done
	rm -f $(EMACS_DEST)/init.el
	rm -f $(EMACS_DEST)/custom.init.el

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
	$(MAKE) install-emacs-conf
	$(MAKE) install-finicky-conf
	$(MAKE) install-fish-conf
	$(MAKE) install-git-conf
#$(MAKE) install-gpg-conf
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
	$(MAKE) uninstall-emacs-conf
	$(MAKE) uninstall-finicky-conf
	$(MAKE) uninstall-fish-conf
	$(MAKE) uninstall-git-conf
#$(MAKE) uninstall-gpg-conf
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

FINICKY_SOURCE := $(CURDIR)/configs/finicky
FINICKY_DEST := $(HOME)
install-finicky-conf: uninstall-finicky-conf
	@echo "\nInstalling finicky configuration 🚀"
	ln -s -f $(FINICKY_SOURCE)/finicky.js $(FINICKY_DEST)/.finicky.js

uninstall-finicky-conf:
	rm -f $(HOME)/.finicky.js

# https://github.com/fish-shell/fish-shell
# FISH SHELL CONFIGURATION

#TODO(fish): fix up make error log isses:
#make: fisher: No such file or directory
#make: *** [install-fish-plugins] Error 1
install-fish-plugins:
	@echo "\nPreparing fish plugin manager before installing plugins..."
	brew list fisher || HOMEBREW_NO_AUTO_UPDATE=1 brew install fisher
	@echo "TBD parse ~/.config/fish/fish_plugins file, putting all plugins in a line, and provide as arg to the `fisher install <arg>`"
	fisher install edc/bass
#fisher install ilancosman/tide@v6
	fisher install jorgebucaran/nvm.fish
	fisher install sentriz/fish-pipenv

FISH_SOURCE := $(CURDIR)/configs/fish
FISH_DEST := $(HOME)/.config/fish
install-fish-conf: uninstall-fish-conf
	@echo "\nInstalling fish shell configuration 🚀"
	mkdir -p $(FISH_DEST)/functions $(FISH_DEST)/conf.d
	ln -s -f $(FISH_SOURCE)/config.fish $(FISH_DEST)/config.fish
	ln -s -f $(FISH_SOURCE)/fish_plugins $(FISH_DEST)/fish_plugins
	ln -s -f $(FISH_SOURCE)/functions/fish_prompt.fish $(FISH_DEST)/functions/fish_prompt.fish
	ln -s -f $(FISH_SOURCE)/conf.d/fnm.fish $(FISH_DEST)/conf.d/fnm.fish

uninstall-fish-conf:
	rm -f $(FISH_DEST)/config.fish
	rm -f $(FISH_DEST)/fish_plugins
	rm -f $(FISH_DEST)/functions/fish_prompt.fish
	rm -f $(FISH_DEST)/conf.d/fnm.fish

# https://github.com/git/git
# git configuration 

GIT_SOURCE := $(CURDIR)/configs/git
GIT_DEST := $(HOME)
install-git-conf: uninstall-git-conf
	@echo "\nInstalling git configuration 🚀"
	ln -s -f $(GIT_SOURCE)/gitattributes $(GIT_DEST)/.gitattributes
	ln -s -f $(GIT_SOURCE)/gitconfig $(GIT_DEST)/.gitconfig
	ln -s -f $(GIT_SOURCE)/gitignore_global $(GIT_DEST)/.gitignore_global

uninstall-git-conf:
	rm -f $(HOME)/.gitattributes
	rm -f $(HOME)/.gitconfig
	rm -f $(HOME)/.gitignore_global

# https://github.com/gpg/gnupg
# gnupg configuration 

GNUPG_SOURCE := $(CURDIR)/configs/gnupg
GNUPG_DEST := $(HOME)/.gnupg
install-gpg-conf: uninstall-gpg-conf
	@echo "GPG public key(ring) is not used at the moment. Exit."
	brew list gnupg || HOMEBREW_NO_AUTO_UPDATE=1 brew install gnupg
	@echo "\nInstalling GnuPG (gpg) configuration 🚀"
	ln -s -f $(GNUPG_SOURCE)/common.conf $(GNUPG_DEST)/common.conf
# ln -s -f $(GNUPG_SOURCE)/gpg.conf $(GNUPG_DEST)/gpg.conf
# ln -s -f $(GNUPG_SOURCE)/gpg-agent.conf $(GNUPG_DEST)/gpg-agent.conf

uninstall-gpg-conf:
	rm -f $(GNUPG_DEST)/common.conf
#rm -f $(GNUPG_DEST)/gpg.conf
#rm -f $(GNUPG_DEST)/gpg-agent.conf

# https://github.com/pqrs-org/Karabiner-Elements
# karabiner configuration (Keyboard customiser)

KARABINER_SOURCE := $(CURDIR)/configs/karabiner
KARABINER_DEST := $(HOME)/.config/karabiner
install-karabiner-conf: uninstall-karabiner-conf
	@echo "\nInstalling Karabiner configuration 🚀"
	mkdir -p $(KARABINER_DEST)/assets/complex_modifications
	ln -s -f $(KARABINER_SOURCE)/karabiner.json $(KARABINER_DEST)/karabiner.json
	ln -s -f $(KARABINER_SOURCE)/change-lang-one-key.json $(KARABINER_DEST)/assets/complex_modifications/change-lang-one-key.json

uninstall-karabiner-conf:
	rm -f $(KARABINER_DEST)/karabiner.json
	rm -f $(KARABINER_DEST)/assets/complex_modifications/change-lang-one-key.json

# https://github.com/neovim/neovim
# NEOVIM CONFIGURATION 

NVIM_SOURCE := $(CURDIR)/configs/nvim
NVIM_DEST := $(HOME)/.config/nvim
install-nvim-conf: uninstall-nvim-conf
	@echo "\nInstalling NeoVim configuration 🚀"
	mkdir -p $(NVIM_DEST)
	ln -s -f $(NVIM_SOURCE)/init.lua $(NVIM_DEST)/init.lua
	ln -s -n $(NVIM_SOURCE)/lua $(NVIM_DEST)/lua

uninstall-nvim-conf:
	rm -f $(NVIM_DEST)/init.lua
# No trailing slash: removes the lua symlink itself (or a real dir), never the repo files.
	rm -rf $(NVIM_DEST)/lua


# https://github.com/gitGNU/gnu_bash
# SHELL CONFIGURATION (bash-driven)

SHELL_SOURCE := $(CURDIR)/configs/shell
SHELL_DEST := $(HOME)
install-shell-conf: uninstall-shell-conf
	@echo "\nInstalling bash & shell config files 🚀"
	echo "$(SHELL_SOURCE)/aliases link to $(SHELL_DEST)/.aliases"
	ln -s -f $(SHELL_SOURCE)/aliases $(SHELL_DEST)/.aliases
	ln -s -f $(SHELL_SOURCE)/bashrc $(SHELL_DEST)/.bashrc
	ln -s -f $(SHELL_SOURCE)/profile $(SHELL_DEST)/.profile

uninstall-shell-conf:
	rm -f $(HOME)/.aliases
	rm -f $(HOME)/.bashrc
	rm -f $(HOME)/.profile

# https://github.com/openssh/openssh-portable
# SSH CONFIGURATION

SSH_SOURCE := $(CURDIR)/configs/ssh
SSH_DEST := $(HOME)/.ssh
install-ssh-conf: uninstall-ssh-conf
	@echo "\nInstalling SSH configuration 🚀"
	mkdir -p -m 700 $(SSH_DEST)
	ln -s -f $(SSH_SOURCE)/config $(SSH_DEST)/config
	@echo "\nChecking out SSH private keys for the 'ssh-agent:"
	ssh-add -l || true

uninstall-ssh-conf:
	rm -f $(HOME)/.ssh/config

## Sublime Text 4 configs
SUBLIME_TEXT_DOTFILES_CONF_DIR := $(CURDIR)/configs/sublime-text
SUBLIME_TEXT_CONF_DIR := $(HOME)/Library/Application Support/Sublime Text/Packages/User
install-sublime-text-conf:
	@$(MAKE) uninstall-sublime-text-conf
	@echo "\n✨ Installing Sublime Text 4 config files."
	@if [ ! -d "$(SUBLIME_TEXT_CONF_DIR)" ]; then echo "❌ Sublime Text 4 is not installed"; exit -1; fi
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Default (OSX).sublime-keymap" "$(SUBLIME_TEXT_CONF_DIR)/Default (OSX).sublime-keymap"
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Package Control.sublime-settings" "$(SUBLIME_TEXT_CONF_DIR)/Package Control.sublime-settings"
	ln -s -n "$(SUBLIME_TEXT_DOTFILES_CONF_DIR)/Preferences.sublime-settings" "$(SUBLIME_TEXT_CONF_DIR)/Preferences.sublime-settings"

uninstall-sublime-text-conf:
	rm -f "$(SUBLIME_TEXT_CONF_DIR)/Default (OSX).sublime-keymap"
	rm -f "$(SUBLIME_TEXT_CONF_DIR)/Package Control.sublime-settings"
	rm -f "$(SUBLIME_TEXT_CONF_DIR)/Preferences.sublime-settings"

# https://github.com/tmux/tmux
# TMUX CONFIGURATION (Terminal multiplexer)

TMUX_SOURCE := $(CURDIR)/configs/tmux
TMUX_DEST := $(HOME)/.config/tmux
install-tmux-conf: uninstall-tmux-conf
	@echo "\nInstalling tmux configuration 🚀"
	mkdir -p $(TMUX_DEST)
	ln -s -f $(TMUX_SOURCE)/tmux.conf $(TMUX_DEST)/tmux.conf

uninstall-tmux-conf:
	rm -f $(TMUX_DEST)/tmux.conf

# https://github.com/vim/vim
# VIM CONFIGURATION

VIM_SOURCE := $(CURDIR)/configs/vim
VIM_DEST := $(HOME)/.vim
install-vim-conf: uninstall-vim-conf
	@echo "\nInstalling Vim configuration 🚀"
	mkdir -p $(VIM_DEST)
	ln -s -f $(VIM_SOURCE)/vimrc $(VIM_DEST)/.vimrc

uninstall-vim-conf:
	rm -f $(VIM_DEST)/.vimrc

# https://github.com/microsoft/vscode
# VSCODE CONFIGURATION

VSCODE_SOURCE := $(CURDIR)/configs/vscode
VSCODE_DEST := $(HOME)/Library/Application Support/Code/User
install-vscode-conf: uninstall-vscode-conf
	@echo "\nInstalling VSCode configuration 🚀"
	mkdir -p "$(VSCODE_DEST)"
	ln -s -f $(VSCODE_SOURCE)/keybindings.json "$(VSCODE_DEST)/keybindings.json"
	ln -s -f $(VSCODE_SOURCE)/settings.json "$(VSCODE_DEST)/settings.json"

uninstall-vscode-conf:
	rm -f "$(VSCODE_DEST)/keybindings.json"
	rm -f "$(VSCODE_DEST)/settings.json"

## Xcode keybindings
XCODE_DIR := $(CURDIR)/configs/xcode
XCODE_KEYBINDINGS_DESTINATION := $(HOME)/Library/Developer/Xcode/UserData/KeyBindings
install-xcode-keybinds:
	@$(MAKE) uninstall-xcode-keybinds
	@echo "\nInstalling Xcode custom key bindings 🚀"
	mkdir -p $(XCODE_KEYBINDINGS_DESTINATION)
	ln -s -n $(XCODE_DIR)/custom.idekeybindings $(XCODE_KEYBINDINGS_DESTINATION)/custom.idekeybindings

uninstall-xcode-keybinds:
	rm -f $(XCODE_KEYBINDINGS_DESTINATION)/custom.idekeybindings

# https://github.com/zed-industries/zed
# ZED EDITOR CONFIGURATION

ZED_SOURCE := $(CURDIR)/configs/zed
ZED_DEST := $(HOME)/.config/zed
install-zed-conf: uninstall-zed-conf
	@echo "\nInstalling Zed editor configuration 🚀"
	mkdir -p $(ZED_DEST)
	ln -s -f $(ZED_SOURCE)/settings.json $(ZED_DEST)/settings.json
	ln -s -f $(ZED_SOURCE)/keymap.json $(ZED_DEST)/keymap.json

uninstall-zed-conf:
	rm -f $(ZED_DEST)/settings.json
	rm -f $(ZED_DEST)/keymap.json

# https://github.com/zellij-org/zellij
# ZELLIJ CONFIGURATION (Pluggable terminal workspace)

ZELLIJ_SOURCE := $(CURDIR)/configs/zellij
ZELLIJ_DEST := $(HOME)/.config/zellij
install-zellij-conf: uninstall-zellij-conf
	@echo "\nInstalling zellij configuration 🚀"
	mkdir -p $(ZELLIJ_DEST)
	ln -s -f $(ZELLIJ_SOURCE)/config.kdl $(ZELLIJ_DEST)/config.kdl

uninstall-zellij-conf:
	rm -f $(ZELLIJ_DEST)/config.kdl

doc-zellij:
	@echo "There are 2 config files available, config.kdl and default.config.kdl in the repo."
	@echo "The config.default.kdl is generated by zellij if there is no config files in the ~/.config/zellij dir by-default."

# https://github.com/zsh-users/zsh
# ZSH CONFIGURATION

ZSH_SOURCE := $(CURDIR)/configs/zsh
ZSH_DEST := $(HOME)
install-zsh-conf: uninstall-zsh-conf
	@echo "\nInstalling Zsh configuration 🚀"
	ln -s -f $(ZSH_SOURCE)/zshenv $(ZSH_DEST)/.zshenv
	ln -s -f $(ZSH_SOURCE)/zshrc $(ZSH_DEST)/.zshrc

uninstall-zsh-conf:
	rm -f $(ZSH_DEST)/.zshenv
	rm -f $(ZSH_DEST)/.zshrc

# Misc Targets:

colorcheck:
	pastel colorcheck
.PHONY: colorcheck

vscode-install-extensions:
	./bin/vscode/vscode-extensions-install
.PHONY: vscode-install-extensions

vscode-export-extension-list:
	./bin/vscode/vscode-extensions-list-export
.PHONY: export-vscode-extension-list

terminal-macos-doc:
	@echo "Use Atom.terminal theme in the Termainl.app -> Settings -> Profiles -> More -> Import..."
.PHONY: terminal-macos-doc

