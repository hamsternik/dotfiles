# Imports
source $HOME/.zsh/zsh-git-prompt/zshrc.sh
source $HOME/.aliases

# Prompt
PROMPT='%B%F{green}%n: %B%F{blue}%1~%b$(git_super_status) %B%F{blue}%# %b%f%k'
RPROMPT=''

autoload -U compinit && compinit
autoload -U promptinit && promptinit
autoload -U colors && colors

setopt correctall
setopt prompt_subst
setopt histignorealldups sharehistory
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt prompt_subst
setopt autocd

zstyle ':completion:*' list-colors $ZLS_COLORS_CUSTOM 
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'

# Menu Selection
zstyle ':completion*:default' menu 'select=0'
zstyle ':completion*:windows' menu on=0

HISTFILE="~/.zsh_history"
HISTSIZE=2000
SAVEHIST=2000

