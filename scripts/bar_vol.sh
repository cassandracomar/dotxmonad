#!/bin/bash

source $(dirname $0)/config.sh

VOL=$(pamixer --get-volume | egrep -o "[0-9]*")
MUTED=$(pamixer --get-mute | egrep -o "[a-z]*")
ICON=""

if [[ $MUTED = "false" ]]; then
    ICON="spkr_01.xbm"
    PERCBAR=`echo "$VOL"\
        | gdbar -bg $bar_bg -fg $bar_fg -h 3 -w 50`
else
    ICON="spkr_02.xbm"
    PERCBAR=`echo 0 \
        | gdbar -bg $bar_bg -fg $bar_fg -h 3 -w 50`
fi

ICON='^i(/home/arjun/.xmonad/dzen2/'"$ICON)"
echo "$ICON $PERCBAR"
