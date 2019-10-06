SHELL := /bin/zsh
.PHONY : vscode-extensions-install brew-core brew-tools brew-cask antibody

vscode-extensions = "vscode-extensions.txt"

vscode-extensions-install:
	@if [ -e "$(vscode-extensions)" ]; then cat $(vscode-extensions) | xargs -L 1 code --install-extension; fi

brew-core:
	brew bundle --file=macos/Brewfile-core

brew-tools:
	brew bundle --file=macos/Brewfile-tools

brew-cask:
	brew bundle --file=macos/Brewfile-cask

brew:
	make brew-core
	make brew-tools
	make brew-cask

antibody:
	@echo "Check is antibody installed. Install all packages..."
	${SHELL} ${DOTFILES}/antibody/install.sh
	@echo "\nIntegrate installed packages via Zsh..."
	${SHELL} `source ${HOME}/.zshrc`
	@exit