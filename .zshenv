# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# @info         .zshenv configuration file
# @description  .zshenv is sourced on all invocations of the shell
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# @author       Nikita Khomitsevich <hamsternik9@gmail.com>
# @licence      GNU General Public License, version 3.0 (GPL-3.0)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Editors
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Java
export JAVA_HOME=/usr/lib/jvm/java-8-oracle
export CATALINA=/opt/apache-tomcat-8.0.15
#export CLASSPATH=/usr/share/java:$CATALINA/lib/servlet-api.jar


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# PATH export
export PATH=$JAVA_HOME/bin:${PATH}
