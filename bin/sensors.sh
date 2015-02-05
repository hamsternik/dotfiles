#!/usr/bin/env bash

# a small script to check and monitor for my system's sensors
# including CPUs, HDDs, etc

user=`whoami`
if [[ "$user" == "root" ]]
then
	clear && sensors && hddtemp /dev/sda
else
	echo "You are not a root!"
fi
