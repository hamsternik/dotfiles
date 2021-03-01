<h1 align="center">~/.dotfiles 💻</h1>

## ⛏ Install

`git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git`

## 🗿 Deploy

`Makefile` is the source of truth for all of scripts/gems/etc that I need in today.

There're plenty of nice-to-havae commands that facilitates my every dotfiles setup:

- install
- uninstall
- brew
- vscode-extensions-install
- antibody

Type `make %command name%` and you will repro one part of my daily configuration.

## 📑 Zsh Configuration

[Antibody](https://getantibody.github.io) - my main shell plugin manager for all Zsh plugins.

**All Plugins**

- [zsh-git-prompt](https://github.com/olivierverdier/zsh-git-prompt) - Informative git prompt for zsh.
- [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions) - Fish-like autosuggestions for zsh.
- [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) - Fish shell like syntax highlighting for Zsh.

### Antibody + Clean My Mac

My one and only OS is *macOS* and also I am the active user of the `CleanMyMac` app to eventually clean my disk from any sort of trash. But due to the cleaning CleanMyMac removes all antibody plugins cache 😞

After finishing basic `CleanMyMac` clean up it is a good time to re-install all Zsh plugins.

Run `make antibody` command in the root directory. Another option: open `~/dotfiles/antibody` directory and manually run `install.sh`.

## 📑 ViM Configuration

**All Plugins**

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

## 👀 Miscellaneous

### Fonts

I'm using `spaceship-prompt` as a default Zsh prompt after a long time trying to customize it on my own. It required `zsh` to be installed and [Powerline Font](https://github.com/powerline/fonts) as well.

#### Deploy

- download the [repo](https://github.com/hamsternik/dotfiles/tree/master/fonts)
- cd to the `fonts` directory
- copy fonts to the `~/Library/Fonts` directory

### True Color Supporting

I switched from the default Terminal.app macOS application to the [cross-platform, GPU-accelerated and popular terminal emulator](https://github.com/alacritty/alacritty). Say hello, alacritty 👋🏻

The topmost reason of doing that is that [the Terminal.app does not support true colors](https://gist.github.com/XVilka/8346728#not-supporting-true-color) and I **basically can not** make my vim so much brilliant as I can do, obviously.

Right now I am using [onedark](https://github.com/joshdick/onedark.vim) colorscheme paired with the [lightline](https://github.com/itchyny/lightline.vim) which [is veeery sweety](https://www.sainnhe.dev/post/status-line-config/) yeah!

### MongoDB

Basically, I am not a cloud engineer, so I really need some space to recall the basic information about i.e. how to run
a mongoDB service localy. Here it is!

- to run MongoDB process

```
brew services start mongodb-community
```

- to stop MongoDB as macOS service
```
brew services stop mongodb-community
```

More info how to install and work with the latest version of MongoDB Community take a look in the [documentation page](https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/#run-mongodb-community-edition).
