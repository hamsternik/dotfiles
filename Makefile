.PHONY : vscode-extensions-install brew-core brew-tools brew-cask

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
