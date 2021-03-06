# dotXmonad

My configuration and customization for using xmonad, a tiling X11 window manager, and related tools.

## How to install

```
curl -L https://raw.githubusercontent.com/cbpark/dotXmonad/master/install.sh | $SHELL
```

[`xmonad`](http://xmonad.org/), [`xmonad-contrib`](http://xmonad.org/contrib.html), and [`xmobar`](http://projects.haskell.org/xmobar/) must be installed. The system tray [`stalonetray`](http://stalonetray.sourceforge.net/) and composite manager [`compton`](https://github.com/chjj/compton) are optional. [D2 Coding](https://github.com/naver/d2codingfont) font is used in `xmobar`.

## Terminal

Note that the default terminal emulator is set to be [`urxvt`](http://software.schmorp.de/pkg/rxvt-unicode.html). See [`Xresources`](Xresources) for its configuration.

## Window switcher and application launcher

[`rofi`](https://github.com/DaveDavenport/rofi) is used for the window switcher and the application launcher. The key bindings are `M-<Tab>` and `C-<Return>`, respectively.

## Keyboard backlight

See [Keyboard backlight](https://wiki.archlinux.org/index.php/Keyboard_backlight) in ArchWiki. It seems to work with the Linux kernel `>=4.9` and the recent version of [`upower`](https://upower.freedesktop.org/).
