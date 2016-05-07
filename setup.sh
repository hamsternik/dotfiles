#!/bin/sh

COUNTER=0
PROGRESS=""
STDOUT_INFO=""
LOAD_CHAR="##########"
HOMEPATH="${HOME}/test"	# change for testing"w
FILES=`find . -maxdepth 1 -type f -name "\.*" ! -name "*.swp" | sed 's .\{2\}  '`
FOLDERS=`find . -maxdepth 1 -type d -name "\.*" ! -name ".git" ! -name "." ! -name ".."`
FILES_COUNT=`echo ${FILES} | wc -w`

# NK: will show percent description of deploing proccess
progress_bar() {
	for i in $1; do
		PROGRESS=$PROGRESS$LOAD_CHAR
		echo "${PROGRESS} on ${2} file"
	done
}

for FILE in $FILES ; do
	clear 
	echo "Start to deploy all dotfiles into your system ...\n"
	progress_bar $COUNTER $FILE

	DIFF_INFO=`diff $FILE "${HOMEPATH}/${FILE}"`
	if [ "$DIFF_INFO" ] || [ ! -z "${HOMEPATH}/${FILE}" ] ; then
		STDOUT_INFO="$STDOUT_INFO"+"$DIFF_INFO info in $FILE file\n"
		cp -r $FILE "$HOMEPATH/"
	else
		STDOUT_INFO="$STDOUT_INFO"+"diff of $FILE if empty\n"
	fi

	echo $STDOUT_INFO
	COUNTER=$COUNTER+1

	sleep 0.1
done

read -r -p "Are you want to copy system folders? [yes]: " resp
case $resp in 
	[yY]|[eE]|[sS])
		for FOLDER in $FOLDERS ; do
			echo "Copy $FOLDER into $HOMEPATH catalog"
			cp -r $FOLDER $HOMEPATH
		done
	;;
esac

# NK:  added stderr output into file provided below 
echo "\nAll problems look at deploy_problems.txt"
echo "DONE!"

