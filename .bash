#!/bin/bash
##
## Bash profile
## Copyright (c) 2007-2024 SATOH Fumiyasu @ OSSTech Corp.
## Copyright (c) 2006-2007 SATOH Fumiyasu @ MIRACLE LINUX Corp.
## Copyright (c) 1999-2003 SATOH Fumiyasu @ Bento Internet
##

## C-u	unix-line-discard
## C-t	transpose-char
## C-/	undo
## M-.	yank-last-word

[[ -z $PS1 ]] && return

## Prompt
if [[ $UID -eq 0 ]]; then
  PS1='# '
else
  PS1='$ '
fi
PS1="\u@\h:\w $PS1"
PS2="> "
PS3="#? "
PS4="+"

if type tput >/dev/null 2>&1 && [[ $(tput colors) -ge 8 ]]; then
  ##  1         bold
  ##  4         underline
  ## 30, 40     black, background ...
  ## 31, 41     red, background ...
  ## 32, 42     green, background ...
  ## 33, 43     yellow, background ...
  ## 34, 44     blue, background ...
  ## 35, 45     magenta, background ...
  ## 36, 46     cyan, background ...
  ## 37, 47     white, background ...
  PS1="\[\e[36m\]$PS1\[\e[m\]"
  PS2="\[\e[41m \[\e[m "
fi

case "$TERM" in
*term*)
  PS1="$PS1\[\e]2;\$USER@\$HOSTNAME_DOT:\$PWD $(tty)\007\]"
  ;;
esac

HISTSIZE=100000
HISTFILESIZE=100000
HISTTIMEFORMAT='%Y-%m-%d %T '
HISTCONTROL=ignorespace:erasedups
shopt -s cmdhist lithist histappend histverify

shopt -s globstar
shopt -s cdspell
shopt -s dirspell
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

## readline
## ======================================================================

bind "set meta-flag on"
bind "set convert-meta off"
bind "set mark-symlinked-directories on"
bind "set completion-ignore-case on"
bind "set expand-tilde on"
bind "set enable-bracketed-paste on"

bind "set menu-complete-display-prefix on"
bind "set show-all-if-ambiguous on"
bind 'TAB:menu-complete'
bind '"\e[Z": menu-complete-backward'
