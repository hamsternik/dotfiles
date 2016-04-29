#!/bin/sh

COUNTER=0
PROGRESS=""
STDOUT_INFO=""
LOAD_CHAR="##########"
FILES=`find . -maxdepth 1 -type f -name "\.*" ! -name "*.swp"` 
FILES_COUNT=`echo ${FILES} | wc -w`

progress_bar() {
	for i in $1; do
		PROGRESS+=$LOAD_CHAR
		echo "${PROGRESS} on ${i} step"
	done
}

for FILE in $FILES; do
	clear 
	echo "Start to deploy all dotfiles into checked system ...\n"

	progress_bar $COUNTER

	DIFF_INFO=`diff $FILE "${HOME}/folder/$FILE"`
	if [ "$DIFF_INFO" ]; then
		STDOUT_INFO+="${DIFF_INFO} info in ${FILE} file\n"
		cp -r $FILE "${HOME}/folder"
		#cp -r $FILE "${HOME}/"
	else
		STDOUT_INFO+="diff of ${FILE} if empty\n"
	fi
	echo $STDOUT_INFO
	let	"COUNTER++"	

	sleep 0.2
done
