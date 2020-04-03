#! /bin/bash

if pgrep compton &> /dev/null; then
    echo "Turning compton OFF"
    pkill compton
else
    echo "Turning compton ON"
    compton -CGb --no-fading-openclose --config $HOME/.config/picom.conf
fi

exit 0
