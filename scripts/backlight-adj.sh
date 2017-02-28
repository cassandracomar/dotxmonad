#!/bin/bash


case "$1" in
	up)
	  xbacklight -inc 5
	  perc=$(xbacklight -get)
	  notify-send " " -i notification-display-brightness-low -h int:value:$perc -h string:x-canonical-private-synchronous:brightness &
	;;
	down)
	  xbacklight -dec 5
	  perc=$(xbacklight -get)
	  notify-send " " -i notification-display-brightness-low -h int:value:$perc -h string:x-canonical-private-synchronous:brightness &
	;;
	status)
	  xbacklight -get
	;;
	*)
	  echo "Accepted arguments are: up, down, status."
	;;
esac

exit 0
