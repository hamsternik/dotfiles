#!/bin/sh

# VARIABLES
NETWORK_STATUS=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I | grep -i state | awk '{print $2}'`
ACTIVE_WIFI_DEVICE_NAME=`/usr/sbin/networksetup -listallhardwareports | awk '/Wi-Fi/{getline; print $2}'`

# TEXT OUTPUT
SEARCH_OUTPUT_PREFIX="[NETWORKUP]"
WIFI_CONNECTION_DISABLED="${SEARCH_OUTPUT_PREFIX} Wi-Fi has been turned off. Please, wait a second to reconnect... 🕒"
WIFI_CONNECTION_ENABLED="${SEARCH_OUTPUT_PREFIX} Wi-Fi has just raised up. Success! 💪"
NETWORK_CONNECTION_EXIST="${SEARCH_OUTPUT_PREFIX} Network connection has already been established. Nothing to do here 🚀"

# FUNCTIONS
function echo_with_syslog {
    echo $1
    syslog -s -l 5 $1
}

if [[ "$NETWORK_STATUS" != "running" ]]; then
    /usr/sbin/networksetup -setairportpower $ACTIVE_WIFI_DEVICE_NAME off
    echo_with_syslog "$WIFI_CONNECTION_DISABLED"

    sleep 1

    /usr/sbin/networksetup -setairportpower $ACTIVE_WIFI_DEVICE_NAME on
    echo_with_syslog "$WIFI_CONNECTION_ENABLED"
else
    echo_with_syslog "$NETWORK_CONNECTION_EXIST"
fi
