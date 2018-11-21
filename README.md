<h1 align="center">dotfiles 💻 ~/</h1>

Title | Description
----- | ----------
Operation System | macOS Mojave (10.14)
Text Editors | Vim/VS Code
Unix Shell | Zsh
Vim Plugin Manager | Vundle

## Install

`git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git`

## Deploy

1. macos/gem.sh
2. macos/brew.sh
3. setup.sh

## CLI Details
- Zsh Plugins (don't use any PMs)
    - [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions) - Fish-like autosuggestions for zsh.
    - [zsh-git-prompt](https://github.com/olivierverdier/zsh-git-prompt) - Informative git prompt for zsh.
    - [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) - Fish shell like syntax highlighting for Zsh.
- Vim Plugins
    - [scrooloose/nerdtree](https://github.com/scrooloose/nerdtree) - A tree explorer plugin for vim.
    - [scrooloose/nerdcommenter](https://github.com/scrooloose/nerdcommenter) - Vim plugin for intensely orgasmic commenting.
    - [Xuyuanp/nerdtree-git-plugin](https://github.com/Xuyuanp/nerdtree-git-plugin) - A plugin of NERDTree showing git status.
    - [vim-airline/vim-airline](https://github.com/vim-airline/vim-airline) - Lean & mean status/tabline for vim that's light as air.
    - [vim-airline/vim-airline-themes](https://github.com/vim-airline/vim-airline-themes) - A collection of themes for vim-airline.
    - [tpope/vim-fugitive](https://github.com/tpope/vim-fugitive) - A Git wrapper so awesome, it should be illegal.
    - [godlygeek/tabular](https://github.com/godlygeek/tabular) - Vim script for text filtering and alignment.
    - [plasticboy/vim-markdown](https://github.com/plasticboy/vim-markdown) - Markdown Vim Mode.
    - [JamshedVesuna/vim-markdown-preview](https://github.com/JamshedVesuna/vim-markdown-preview) - A light Vim plugin for previewing markdown files in a browser.
    - [luochen1990/rainbow](https://github.com/luochen1990/rainbow) - Rainbow Parentheses Improved, shorter code, no level limit, smooth and fast, powerful
        configuration.
    - [lepture/vim-velocity](https://github.com/lepture/vim-velocity) - Velocity syntax for vim.
    - [danro/rename.vim](https://github.com/danro/rename.vim) - Rename the current file in the vim buffer + retain relative path.

#### TERMINFO

An `xterm-256color` TERMINFO file adds escape sequence for italic and overwrite conflicting sequence for standout text.

To check that the terminal could display the text italic, type:

```
echo `tput sitm`italics`tput ritm` `tput smso`standout`tput rmso`
```
