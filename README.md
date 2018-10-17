## Installation

`git clone --recurse-submodules -j8 git@github.com:hamsternik/dotfiles.git`

#### TERMINFO

An `xterm-256color` TERMINFO file adds escape sequence for italic and overwrite conflicting sequence for standout text.

To check that the terminal could display the text italic, type:

```
echo `tput sitm`italics`tput ritm` `tput smso`standout`tput rmso`
```
