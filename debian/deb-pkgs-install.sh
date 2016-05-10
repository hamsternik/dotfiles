#!/bin/bash

CUR_FOLDER=`pwd`
PACKAGES_LIST="apt-app-list.txt"


if [ $(id -u) -eq 0 ]; then

		read -e -p "Are you want to update aptitude first? [y]: " -i "y" resp
	if [ "$resp" == "y" ]; then
		aptitude update
	fi

	if [ -f $CUR_FOLDER/$PACKAGES_LIST ]; then
		PACKAGES=`cat apt-app-list.txt`
		for package in $PACKAGES; do
			aptitude install $package
		done
	fi
else
	echo "Please, login within a ROOT user"
fi

