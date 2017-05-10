#!/bin/sh
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

LANG="en_GB.UTF-8"

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xrdb -merge ~/tools/colors/night.xresource

if [ "$THEME" == white ]; then
    xrdb -merge ~/tools/colors/white.xresource
fi

xmodmap ~/.Xmodmap

xset r rate 200 25
exec startfluxbox
