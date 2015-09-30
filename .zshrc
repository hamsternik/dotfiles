# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# @info     .zshrc configuration file
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# @author   Nikita Khomitsevich <hamsternik9@gmail.com>
# @licence  GNU General Public License, version 3.0 (GPL-3.0)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# included aliases in `zsh` environment 
source $HOME/.aliases


autoload -U compinit promptinit
autoload -U colors
compinit
promptinit
colors

git_promt() {
    temp=`git symbolic-ref HEAD 2>/dev/null | cut -d / -f 3`
    if [ "$temp" != "" ]; then echo "$temp:"; fi
}

setopt histignorealldups sharehistory
setopt prompt_subst

PROMPT='%B%F{green}%n %B%F{blue}%1~%b %B%F{blue}%# %b%f%k'
RPROMPT='[$(git_promt)%~]'


# Use emacs keybindings even if our EDITOR is set to vi
bindkey -v

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
