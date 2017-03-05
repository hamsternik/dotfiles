# env vars
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

export ZLS_COLORS_CUSTOM='di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'

# 256 color mode
export TERM="xterm-256color"

# cocoapods
export LC_ALL="en_US.UTF-8"

##################
### PATH Setup ###
##################

export PATH=""
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.bin"

# java
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH=${PATH}:${JAVA_HOME}/bin

# android
export ANDROID_HOME=${HOME}/Library/Android/sdk
export PATH=$PATH:${ANDROID_HOME}/tools
export PATH=$PATH:${ANDROID_HOME}/platform-tools

# OpenSSL
# before make ln -s /path/to/openssl-by-brew /usr/local/openssl
# as ex.: ln -s /usr/local/Cellar/openssl/1.0.2j/bin/openssl /usr/local/openssl
export OPENSSL_BREW=/usr/local/Cellar/openssl/1.0.2j/bin
export PATH=$OPENSSL_BREW:$PATH

# .NET Core + CLI
export C_SHARP=/usr/local/share/dotnet
export PATH=$PATH:$C_SHARP

