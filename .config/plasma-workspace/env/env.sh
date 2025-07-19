#!/bin/sh
##
## KDE: Plasma Workspace ~/.config/plasma-workspace/env/env.sh
##

export LANG="ja_JP.UTF-8"
export PATH="$HOME/bin:$PATH"

if [ -z "$DEBUGINFOD_URLS" ]; then
    DEBUGINFOD_URLS=$(cat /dev/null "/etc/debuginfod"/*.urls 2>/dev/null | tr '\n' ' ')
    [ -n "$DEBUGINFOD_URLS" ] && export DEBUGINFOD_URLS || unset DEBUGINFOD_URLS
fi

xset b off

touch_dev=$(xinput --list --name-only |grep Finger)
if [ -n "$touch_dev" ]; then
  xinput --disable "$touch_dev"
fi

if type wcwidth-cjk >/dev/null 2>&1; then
  eval "$(wcwidth-cjk --sh-init)"
fi

pactl load-module module-native-protocol-tcp listen=127.0.0.1
