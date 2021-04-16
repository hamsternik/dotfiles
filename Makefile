SHELL := /bin/zsh
.zshrc: .SHELLFLAGS := --rcfile zshrc -ic --

bold := $(shell tput bold) # use as $(tput bold)
yellow := $(shell tput setaf 3)
tput_off := $(shell tput sgr0)

install:
	${shell} ./.dotfiles.sh --install
.PHONY: install

uninstall:
	$(shell) .dotfiles.sh --uninstall
.PHONY: uninstall

brew-core:
	brew bundle --file=brew/Brewfile-core
.PHONY: brew-core

brew-tools:
	brew bundle --file=brew/Brewfile-tools
.PHONY: brew-tools

brew-cask:
	brew bundle --file=brew/Brewfile-cask
.PHONY: brew-cask

brew:
	make brew-core
	make brew-tools
	make brew-cask
.PHONY: brew

vscode-extensions = "vscode/.vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi
.PHONY: vscode-extensions-install

antibody:
	$(shell) antibody/install.sh
.PHONY: antibody

