#!/bin/sh
##
## KDE: Plasma Workspace ~/.config/plasma-workspace/env/env.sh
##

export PATH="$HOME/bin:$PATH"

xset b off

touch_dev=$(xinput --list --name-only |grep Finger)
if [ -n "$touch_dev" ]; then
  xinput --disable "$touch_dev"
fi

if type wcwidth-cjk >/dev/null 2>&1; then
  eval "$(wcwidth-cjk --sh-init)"
fi
