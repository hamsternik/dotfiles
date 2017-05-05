PROMPT='%B%F{blue}%n%B%F{white}: %B%F{green}%1~%b$(git_super_status) %B%F{white}$ %b%f%k'
RPROMPT=''

HISTFILE="~/.zsh_history"
HISTSIZE=2000
SAVEHIST=2000

autoload -U compinit && compinit
autoload -U promptinit && promptinit
autoload -U colors && colors

setopt autocd
setopt prompt_subst
setopt correctall
setopt prompt_subst
setopt sharehistory
setopt histignorealldups 
setopt hist_ignore_all_dups
setopt hist_ignore_space

zstyle ":completion:*" auto-description "specify: %d"
zstyle ":completion:*" completer _expand _complete _correct _approximate 
zstyle ':completion:*' list-colors $ZLS_COLORS_CUSTOM 
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' verbose true

# Menu Selection
zstyle ':completion*:default' menu 'select=0'
zstyle ':completion*:windows' menu on=0

zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Check and execute
function check_and_exec {
    if [[ -e $1 ]]; then
        source $1
    fi
}

### Copied from that gist
### https://gist.github.com/cocoalabs/2fb7dc2199b0d4bf160364b8e557eb66
man() {
	  env \
		    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
		    LESS_TERMCAP_md=$(printf "\e[1;31m") \
		    LESS_TERMCAP_me=$(printf "\e[0m") \
		    LESS_TERMCAP_se=$(printf "\e[0m") \
		    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
		    LESS_TERMCAP_ue=$(printf "\e[0m") \
		    LESS_TERMCAP_us=$(printf "\e[1;32m") \
			  man "$@"
}

# Imports & Plugins 
check_and_exec $HOME/.aliases
check_and_exec $HOME/.zsh/zsh-git-prompt/zshrc.sh
check_and_exec $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
check_and_exec $HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
#check_and_exec $HOME/.zsh/exorcism/exercism_completion.zsh
