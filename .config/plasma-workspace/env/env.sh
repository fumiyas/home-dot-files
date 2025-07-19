#!/bin/sh
##
## KDE: Plasma Workspace ~/.config/plasma-workspace/env/env.sh
##

set -u

export LANG="ja_JP.UTF-8"
export PATH="$HOME/bin:$PATH"

if [ -z "${DEBUGINFOD_URLS-}" ]; then
  DEBUGINFOD_URLS=$(cat /dev/null "/etc/debuginfod"/*.urls 2>/dev/null | tr '\n' ' ')
  if [ -n "$DEBUGINFOD_URLS" ]; then
    export DEBUGINFOD_URLS
  else
    unset DEBUGINFOD_URLS
  fi
fi

export XMODIFIERS=@im=fcitx

if [ "${XDG_SESSION_TYPE-}" = wayland ]; then
  export QT_QPA_PLATFORM="wayland;xcb"
  export QT_IM_MODULES="wayland;fcitx;ibus"
  ## FIXME: Broken?
  #export GTK_IM_MODULES="wayland"
  export GTK_IM_MODULE="fcitx"
  export GDK_BACKEND="wayland"
  export MOZ_ENABLE_WAYLAND="1"
else
  xset b off
  #export GTK_IM_MODULE=fcitx5
  touch_dev=$(xinput --list --name-only |grep Finger)
  if [ -n "$touch_dev" ]; then
    xinput --disable "$touch_dev"
  fi
fi

if type wcwidth-cjk >/dev/null 2>&1; then
  eval "$(wcwidth-cjk --sh-init)"
fi

pactl load-module module-native-protocol-tcp listen=127.0.0.1
