SHELL := /bin/zsh
.PHONY : install uninstall vscode-extensions-install brew-core brew-tools brew-cask antibody

install:
	.dotfiles.sh --install
	make brew

uninstall:
	.dotfiles.sh --uninstall

brew-core:
	brew bundle --file=brew/Brewfile-core

brew-tools:
	brew bundle --file=brew/Brewfile-tools

brew-cask:
	brew bundle --file=brew/Brewfile-cask

brew:
	make brew-core
	make brew-tools
	make brew-cask

vscode-extensions = ".vscode-extensions"
vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi

antibody:
	@echo "Check is antibody installed. Install all packages..."
	${SHELL} ${DOTFILES}/antibody/install.sh
	@echo "\nIntegrate installed packages via Zsh..."
	${SHELL} `source ${HOME}/.zshrc`
	@exit
