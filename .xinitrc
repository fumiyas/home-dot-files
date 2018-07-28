#!/bin/bash

set -x

## startx(1) invokes this script via /bin/sh
[ ${BASH##*/} = "bash" ] || exec bash "$0" "$@" || exit 1

set -u

log="$HOME/.xclient.log"
[ -f "$log" ] && mv "$log" "$log.old"

export LANG=ja_JP.UTF-8
#export UIM_CANDWIN_PROG=uim-candwin-qt4
#export UIM_CANDWIN_PROG=uim-candwin-gtk3
eval $(wcwidth-cjk --sh-init)

xset b off

(
  #syndaemon -d -R -t -i 1.0
  #fcitx -r
  uim-xim &
  uim-toolbar-qt4 &
  startkde
) > >(ts '%Y-%m-%d %H:%M:%S' |tee -a "$log") 2>&1

