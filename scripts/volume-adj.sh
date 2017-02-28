#!/bin/bash


case "$1" in
	up)
	  pamixer -i 5
	  perc=$(pamixer --get-volume)
	  volume_id=
	  if [ "$perc" -gt 66 ]; then
	    volume_id='notification-audio-volume-high'
	  elif [ "$perc" -gt 33 ]; then
	    volume_id='notification-audio-volume-medium'
	  elif [ "$perc" -gt 0 ]; then
	    volume_id='notification-audio-volume-low'
	  else
	    volume_id='notification-audio-volume-off'
	  fi
	  notify-send " " -i $volume_id -h int:value:$perc -h string:x-canonical-private-synchronous:volume &
	;;
	down)
	  pamixer -d 5
	  perc=$(pamixer --get-volume)
	  volume_id=
	  if [ "$perc" -gt 66 ]; then
	    volume_id='notification-audio-volume-high'
	  elif [ "$perc" -gt 33 ]; then
	    volume_id='notification-audio-volume-medium'
	  elif [ "$perc" -gt 0 ]; then
	    volume_id='notification-audio-volume-low'
	  else
	    volume_id='notification-audio-volume-off'
	  fi
	  notify-send " " -i $volume_id -h int:value:$perc -h string:x-canonical-private-synchronous:volume &
	;;
	mute)
	  pamixer --toggle-mute
	  perc=$(pamixer --get-volume)
	  volume_id=
	  if [[ "$(pamixer --get-mute)" == 'true' ]]; then
	    volume_id='notification-audio-volume-muted'
	  elif [ "$perc" -gt 66 ]; then
	    volume_id='notification-audio-volume-high'
	  elif [ "$perc" -gt 33 ]; then
	    volume_id='notification-audio-volume-medium'
	  elif [ "$perc" -gt 0 ]; then
	    volume_id='notification-audio-volume-low'
	  else
	    volume_id='notification-audio-volume-off'
	  fi
	  notify-send " " -i $volume_id -h int:value:$perc -h string:x-canonical-private-synchronous:volume &
	;;
	status)
	  pamixer --get-volume
	;;
	*)
	  echo "Accepted arguments are: up, down, mute. status."
	;;
esac

exit 0
