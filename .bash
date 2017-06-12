#!/bin/bash
##
## Bash profile
## Copyright (c) 2007-2016 SATOH Fumiyasu @ OSS Technology Corp.
## Copyright (c) 2006-2007 SATOH Fumiyasu @ MIRACLE LINUX Corp.
## Copyright (c) 1999-2003 SATOH Fumiyasu @ Bento Internet
##

## C-u	unix-line-discard
## C-t	transpose-char
## C-/	undo
## M-.	yank-last-word

[ -z "$PS1" ] && return

## Prompt
if [ "$UID" -eq 0 ]; then
  PS1='# '
else
  PS1='$ '
fi
PS1="\u@\h:\w $PS1"
PS2="> "
PS3="#? "
PS4="+"

if type tput >/dev/null 2>&1 && [[ $(tput colors) -ge 8 ]]; then
  ## 1/2	bold
  ## 3/4	underline
  ## 5	glay background
  ## 6	black background
  ## 7	white reverse

  ## 30	black
  ## 31	red
  ## 32	green
  ## 33	orange (?)
  ## 34	blue
  ## 35	purple
  ## 36	cyan
  ## 37	white

  ## 40	black background
  ## 41	red background
  ## 42	green background
  ## 43	orange (?) background
  ## 44	blue background
  ## 45	purple background
  ## 46	cyan background
  ## 47	white (light glay) background

  PS1="\[\e[36m\]$PS1\[\e[m\]"
  PS2=""
  #PS2="\[\e[41m \[\e[m "
fi

case "$TERM" in
*term)
  PS1="$PS1\[\e]2;\$USER@\$HOSTNAME_DOT:\$PWD $(tty)\007\]"
  ;;
esac

## Command history
HISTSIZE=100000
HISTFILESIZE=100000
HISTTIMEFORMAT='%Y/%m/%d %T '
HISTCONTROL=ignorespace:erasedups
shopt -s cmdhist lithist histappend histverify

shopt -s globstar
shopt -s checkwinsize
shopt -s lastpipe

set -o emacs
set -o physical
set -o ignoreeof
set -o pipefail

if type complete >/dev/null 2>&1; then
  complete -d cd
  complete -c man type
  complete -u -c -f -d sudo
  complete -u su finger
fi

