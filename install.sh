#! /usr/bin/env bash

function backup {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

function check {
    command -v $1 >/dev/null 2>&1 \
        || { echo "-- $1 is not found." >&2; exit 1; }
}

DOTXMONAD=$HOME/.xmonad

if command -v xmonad >/dev/null 2>&1; then
    check xmobar
    check stalonetray
    backup ".xmonad"
    backup ".xmobarrc"
    backup ".stalonetrayrc"
    git clone git@github.com:cbpark/dotXmonad.git $DOTXMONAD
    ln -sf $DOTXMONAD/xmobarrc $HOME/.xmobarrc
    ln -sf $DOTXMONAD/stalonetrayrc $HOME/.stalonetrayrc
    xmonad --recompile
    [ ! -s $DOTXMONAD/xmonad.errors ] && { cat xmonad.errors; exit 1; }
else
    echo "-- xmonad is not found."
    exit 1
fi
