# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac
export TERM=xterm-256color

force_color_prompt=yes
if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    if [ -d ".git" ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[1;32m\]\u:\[\033[0;35m\]\w \[\033[1;36m\]$(__git_ps1 "(%s)") \[\033[00m\]\$ '
    else
        PS1='${debian_chroot:+($debian_chroot)}\[\033[0;36m\]\h\[\033[1;31m\]@\[\033[1;32m\]\u:\[\033[0;35m\]\w\[\033[00m\]\$ '
    fi

    else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Extended 'ls' aliases
alias ll='ls -AFl'
alias la='ls -A'
alias l='ls -CF'

# Extended 'cd' aliases
alias ..='cd ..'

# Extended (optional) aliases
alias cl='clear'

# Extended 'gdb' aliases
alias gdb='gdb -q'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
        # show in .git folder your current branch
        #export PS1='\W$(__git_ps1 "(%s)")>'                    
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# No ttyctl, so we need to save and then restore terminal settings
vim()
{
    local STTYOPTS="$(stty --save)"
    stty stop '' -ixoff
    command vim "$@"
    stty "$STTYOPTS"
}

# _____Hadoop configurations______ #

# Set Hadoop-related environment variables
export HADOOP_HOME=/opt/hadoop-1.2.1
# Set JAVA_HOME
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/jre

# Add Hadoop bin/ directory to PATH
export PATH=/$PATH:$HADOOP_HOME/bin

# Some convenient aliases for working with Hadoop comands
unalias fs &> /dev/null
alias fs="hadoop fs"
unalias hls &> /dev/null
alias hls="fs -ls"

# _____Mahout Configuration_____ #

#Set Mahout-related environment varibles 
export MAHOUT_HOME=/opt/mahout-distribution-0.10.0
# Add Mahout bin/ directory to PATH
export PATH=/$PATH:$MAHOUT_HOME/bin
