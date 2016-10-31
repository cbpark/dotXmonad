#! /usr/bin/env bash

REPO=https://github.com/cbpark/dotXmonad.git
DOTXMONAD=$HOME/.xmonad
CONTRIB=$(ghc-pkg list | grep "xmonad-contrib")

function backup_old {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

function check_or_die {
    command -v $1 >/dev/null 2>&1 || { echo "-- $1 is not found." >&2; exit 1; }
}

if command -v xmonad >/dev/null 2>&1; then
    check_or_die xmobar
    [ "" == "$CONTRIB" ] && { echo "-- xmonad-contrib is not found."; exit 1; }
    backup_old ".xmonad"
    backup_old ".xmobarrc"
    git clone $REPO $DOTXMONAD
    ln -sf $DOTXMONAD/xmobarrc $HOME/.xmobarrc
    xmonad --recompile
    [ ! -s $DOTXMONAD/xmonad.errors ] && { cat xmonad.errors; exit 1; }
else
    echo "-- xmonad is not found."
    exit 1
fi

if command -v stalonetray >/dev/null 2>&1; then
    echo "-- stalonetray found."
    backup_old ".stalonetrayrc"
    ln -sf $DOTXMONAD/stalonetrayrc $HOME/.stalonetrayrc
    stalonetray &
fi

if command -v compton >/dev/null 2>&1; then
    echo "-- compton found."
    backup_old ".config/compton.conf"
    # mkdir -p $HOME/.config
    ln -sf $DOTXMONAD/compton.conf $HOME/.config/compton.conf
    compton -b
fi
