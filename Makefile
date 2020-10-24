SHELL := /bin/zsh

install:
	.dotfiles.sh --install
	make brew
.PHONY: install

uninstall:
	.dotfiles.sh --uninstall
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

vscode-extensions = ".vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi
.PHONY: vscode-extensions-install

antibody:
	@echo "Check is antibody installed. Install all packages..."
	${SHELL} ${DOTFILES}/antibody/install.sh
	@echo "\nIntegrate installed packages via Zsh..."
	${SHELL} `source ${HOME}/.zshrc`
	@exit
.PHONY: antibody
