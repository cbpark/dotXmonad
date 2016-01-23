#! /bin/bash

function backup {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

if command -v xmonad 2>/dev/null; then
    backup ".xmonad"
    backup ".xmobarrc"
    backup ".stalonetrayrc"
    git clone git@github.com:cbpark/dotXmonad.git $HOME/.xmonad
    command -v xmobar >/dev/null 2>&1 || { echo "You have to install xmobar." >&2; exit 1;}
    ln -sf $HOME/.xmonad/xmobarrc $HOME/.xmobarrc
    ln -sf $HOME/.xmonad/stalonetrayrc $HOME/.stalonetrayrc
    xmonad --recompile
else
    echo "-- xmonad is not found."
    exit 1
fi
