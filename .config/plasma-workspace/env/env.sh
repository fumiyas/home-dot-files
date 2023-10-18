#!/bin/sh
##
## KDE: Plasma Workspace ~/.config/plasma-workspace/env/env.sh
##

export PATH="$HOME/bin:$PATH"

syndaemon -d -t
xset b off

if type wcwidth-cjk >/dev/null 2>&1; then
  eval "$(wcwidth-cjk --sh-init)"
fi
