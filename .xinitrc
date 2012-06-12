#!/bin/sh

log="$HOME/.xclient.log"

LANG=ja_JP.UTF-8
export LANG

[ -f "$log" ] && mv "$log" "$log.old"
touch "$log"

uim-xim >>"$log" 2>&1 &

startkde 2>&1\
  |{ type ts >/dev/null 2>&1 && ts '%Y-%m-%d %H:%M:%S' || cat; } \
  |tee -a "$log" \
  ;

