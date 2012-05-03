## /bin/sh profile
## Copyright (c) 1999-2008 SATOH Fumiyasu. All rights reserved.
##
## Date: 2008-05-03, since 1999-09-20

PATH=/bin:/usr/bin:/usr/local/bin
umask 0002

TTY=`tty`
: ${HOSTNAME:=`hostname |sed 's/\..*//'`}
: ${HOSTNAME_DOT:=`hostname`}

if [ -n "$PS1" ]; then
  if [ "`id |sed 's/(.*//;s/.*=//'`" -eq 0 ]; then
    ## Super user
    PS1="$HOSTNAME # "
  else
    ## Ordinary user
    PS1="$HOSTNAME \$ "
  fi
  PS2="> "
fi

## Read separated profiles
## ----------------------------------------------------------------------

for profile in "$HOME/.sh.d/"*; do
  if [ -f "$profile" ]; then
    . "$profile"
  fi
done

## Read specific profile
## ----------------------------------------------------------------------

[ -n "${ZSH_VERSION-}" ] && . "$HOME/.zsh"
[ -n "${BASH_VERSION-}" ] && . "$HOME/.bash"

