#!/bin/bash
ln -s $(readlink -f .config/emacs) ~/.config/emacs
ln -s $(readlink -f .config/i3) ~/.config/i3
ln -s $(readlink -f .config/rofi) ~/.config/rofi

# Require sudo

ln -s $(readlink -f usr/share/X11/xorg.conf.d/41-libinput-user.conf) /usr/share/X11/xorg.conf.d/41-libinput-user.conf
