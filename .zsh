#!/bin/zsh
##
## zsh profile
## Copyright (c) 2008-2013 SATOH Fumiyasu @ OSS Technology Corp. Japan
##
## Date: 2013-03-04, since 2008-05-02
##

## ESC q		Suspend current command-line editing
## fc -l 1		Print all command-history
##
## Expansion:
##	wget http://example.jp/foo-{001..100}.jpg
##	ldd =smbd
##
## Glob:
## 	rm foo-<-1999>????.log
## 	ls foo-<2000-2001>????.log
##

## http://gihyo.jp/dev/serial/01/zsh-book/0003

autoload -U is-at-least
autoload -U colors; colors

disable r

## Key bindng
## ======================================================================

bindkey -e

bindkey -s '^z' '^[q %\\$EDITOR^m'

#run-fg-editor() {
#  zle push-input
#  local BUFFER="fg %$EDITOR:t"
#  zle accept-line
#}
#zle -N run-fg-editor
#bindkey '^z' run-fg-editor


## Completion
## ======================================================================

autoload -U compinit; compinit

zstyle ':completion:*' menu select=1
zstyle ':completion:*' matcher-list '' 'm:{a-z-}={A-Z_} r:|[-_.]=**'
if [ -n "${LS_COLORS:-}" ]; then
  zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
else
  zstyle ':completion:*' list-colors \
    'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
fi
zstyle ':completion:*' file-patterns \
  '%p:globbed-files *(-/):directories' '*:all-files'

zstyle ':completion:*:cd:*' ignore-parents parent pwd

setopt AUTO_LIST
setopt LIST_TYPES
setopt LIST_PACKED

setopt CORRECT_ALL
#CORRECT_IGNORE='_*'

bindkey '^i'    expand-or-complete	## Tab
bindkey '\e[Z'  reverse-menu-complete	## Shift+Tab

function _delete-char-or-list-expand() {
  if [[ -z "${RBUFFER}" ]]; then
    zle list-expand
  else
    zle delete-char
  fi
}
zle -N _delete-char-or-list-expand
bindkey '^d' _delete-char-or-list-expand

#WORDCHARS="${WORDCHARS//[ |\/._-]/}#"
autoload -U select-word-style
select-word-style default
#select-word-style bash
#zstyle ':zle:*' word-chars " _-./;@"
#zstyle ':zle:*' word-style unspecified
bindkey '^w' vi-backward-kill-word
bindkey '^[^w' backward-kill-word
bindkey '^[^B' vi-backward-blank-word
bindkey '^[^F' vi-forward-blank-word
#bindkey '^[^K' delete-word

## Change directory
## ======================================================================

DIRSTACKSIZE=20

## ディレクトリ名のみの入力をcdの引数として実行
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_IGNORE_DUPS

## History
## ======================================================================

HISTSIZE=999999
SAVEHIST=999999
HISTFILE="$HOME/.zhistory"
setopt SHARE_HISTORY
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE
setopt HIST_FIND_NO_DUPS
setopt HIST_VERIFY
setopt EXTENDED_HISTORY
# setopt APPEND_HISTORY		# 履歴を上書きせず追加のみ行う
setopt INC_APPEND_HISTORY	# 履歴を上書きせず追加のみ行う(コマンド実行後に追加)
				# (入力が空の状態で補完・PATH検索をしないのはデフォルト)

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

## Global alias
## ======================================================================

function colorgrep_pager()
{
  local grep="egrep"
  type gegrep >/dev/null && grep="gegrep"
  "$grep" --color=always "$@" |sed 's/\[K//g' |$PAGER
}

alias -g P='|$PAGER'
alias -g P2='2>&1|$PAGER'
alias -g T='|tee'
alias -g T2='2>&1|tee'
alias -g G='|colorgrep_pager'
alias -g D='|colordiff |$PAGER'

## ======================================================================

setopt no_HUP			# exit時にHUPシグナルを出さない
setopt no_ALL_EXPORT		# 変数を勝手にexportしない
#setopt no_UNSET			# 未定義の変数を参照するとエラーを表示
#setopt no_FLOW_CONTROL		# フロー制御(Ctrl+S, Ctrl+Q)を使わない
setopt RM_STAR_SILENT
setopt IGNORE_EOF		# Ctrl+dを続けて9回までは押しても終了しない
setopt no_HASH_CMDS		# コマンド位置をハッシュしない
setopt no_HASH_DIRS		# ディレクトリ位置をハッシュしない

setopt PRINT_EIGHT_BIT
setopt NOTIFY			# バックグラウンドのジョブが終了したらその時点で通知
setopt PRINT_EXIT_VALUE		# コマンド終了コードが 0 以外のときに表示
unset MAILCHECK			# メールチェックはしない
#setopt no_CLOBBER		# リダイレクト(>, >&, <>)で既存ファイルを上書きしない

## Brace expansion
## ======================================================================

setopt BRACE_CCL		# {a-z} を a b c d ... に展開
setopt EXTENDED_GLOB		# 拡張パターンマッチングを使用

## Filename glob
## ======================================================================

if is-at-least 4.2.0; then
  setopt no_CASE_GLOB
fi

setopt NUMERIC_GLOB_SORT

## Do not do glob for URI-like string
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## Prompt
## ======================================================================

if is-at-least 4.3.0; then
  setopt PROMPT_CR
else
  setopt PROMPT_SP
fi

case "$TERM" in
screen.*)
  unset PROMPT
  precmd() { echo -ne "\ek\e\\"; print -Pn "\e]0; %~ %n@%m\a" }
#  unalias s
#  function t()
#  {
#    echo -ne "\ek$1${2+ $2}${3+ $3}\e\\"
#    "$@"
#  }
#  # tと同様・sudoの短縮も兼ねる
#  function s()
#  {
#    echo -ne "\eks:$1${2+ $2}${3+ $3}\e\\"
#    sudo "$@"
#  }
  ;;
*term|rxvt*|gnome*)
  precmd() { print -Pn "\e]2;%n@%m:%~ (${TTY#/dev/})\a" }
  ;;
esac

PROMPT="%{$fg[blue]%}%d%{${reset_color}%}
%{$fg[cyan]%}%B%n@%m%b%{${reset_color}%}"
if [ "$UID" -eq 0 ]; then
  PROMPT="$PROMPT %{$fg[red]%}%B#%b%{${reset_color}%} "
else
  PROMPT="$PROMPT %{$fg[cyan]%}%B\$%b%{${reset_color}%} "
fi

SPROMPT="%{$fg[yellow]%}%R%{${reset_color}%}
%{$fg[yellow]%}%r%{${reset_color}%} %B[No|Yes|Abort|Edit]?%b "

