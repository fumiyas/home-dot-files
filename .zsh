#!/bin/zsh
##
## zsh profile
## Copyright (c) 2008-2016 SATOH Fumiyasu @ OSS Technology Corp. Japan
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

if [[ -f "$HOME/.zplug/init.zsh" ]]; then
  source "$HOME/.zplug/init.zsh"
  zplug "mollifier/anyframe"
  zplug check || zplug install
  zplug load
fi

## ======================================================================

## http://gihyo.jp/dev/serial/01/zsh-book/0003

autoload -U is-at-least
autoload -U colors; colors

disable r

## Key bindng
## ======================================================================

bindkey -e

bindkey '^]'   vi-find-next-char
bindkey '^[^]' vi-find-prev-char

#bindkey -s '^z' '^[q %\\$EDITOR^m'
#
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

zstyle ':completion:*:complete:ssh:*:hosts' command \
  'getent hosts; sed -n "s/^Host[[:blank:]]*/dummy /p" ~/.ssh/config*'

compdef s=env

setopt AUTO_LIST
setopt LIST_TYPES
setopt LIST_PACKED

setopt CORRECT_ALL
#CORRECT_IGNORE='_*'
CORRECT_IGNORE_FILE='.*'

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

if type anyframe-init >&/dev/null; then
  #autoload -Uz anyframe-init
  #anyframe-init
  zstyle ":anyframe:selector:" use peco
  zstyle ":anyframe:selector:peco:" command 'env TERM=xterm peco --layout=bottom-up'
  bindkey '^R' anyframe-widget-put-history
fi

## Change directory
## ======================================================================

DIRSTACKSIZE=20

## ディレクトリ名のみの入力をcdの引数として実行
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt PUSHD_IGNORE_DUPS

## ----------------------------------------------------------------------

cdpath=(~)

cdp()
{
  if [[ $# -eq 0 ]]; then
    # shellcheck disable=SC2164 # Use 'cd ... || exit' or 'cd ... || return'
    builtin cd
    return $?
  fi

  local cd="$1"
  local dir
  local reply

  if [[ ! -d "$cd" ]]; then
    for dir in ${cdpath-}; do
      if [[ -d $dir/$cd ]]; then
        read -rq "reply?Change directory under $dir? (y/n) "
        if [[ $reply != y ]]; then
          echo 'Canceled.'
          return 1
        fi
        break
      fi
    done
  fi

  builtin cd "$cd" || return $?
}

alias cd=cdp
compdef cdp=cd

## Heading for all respective groups of completion suggestions
#zstyle ':completion:*' group-name ''
#zstyle ':completion:*:descriptions' format %d

## Remove path-directories from the suggestion sources
zstyle ':completion:*:complete:(cd|cdp|pushd):*' tag-order 'local-directories named-directories'

## History
## ======================================================================

HISTSIZE=999999
SAVEHIST=999999
HISTFILE="$HOME/.zhistory"
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE
setopt HIST_FIND_NO_DUPS
setopt HIST_VERIFY

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
alias -g V='|view -'
alias -g V2='2>&1|view -'
alias -g T='|tee'
alias -g T2='2>&1|tee'
alias -g 2N='2>/dev/null'
alias -g G='|colorgrep_pager'
alias -g D='|diff-highlight |${=PAGER}'

## ======================================================================

setopt no_HUP			# exit時にHUPシグナルを出さない
setopt no_ALL_EXPORT		# 変数を勝手にexportしない
#setopt no_UNSET		# 未定義の変数を参照するとエラーを表示
#setopt no_FLOW_CONTROL		# フロー制御(Ctrl+S, Ctrl+Q)を使わない
setopt RM_STAR_SILENT
setopt IGNORE_EOF		# Ctrl+dを続けて9回までは押しても終了しない
setopt no_HASH_CMDS		# コマンド位置をハッシュしない
setopt no_HASH_DIRS		# ディレクトリ位置をハッシュしない
setopt INTERACTIVE_COMMENTS	# 対話シェル時に「#コメント」を記述可能にする

setopt no_MULTIOS
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

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

## Prompt, Window Title
## ======================================================================

setopt TRANSIENT_RPROMPT
if is-at-least 4.3.0; then
  setopt PROMPT_SP
  setopt PROMPT_CR
fi

## ----------------------------------------------------------------------

autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' formats '%s %b '
zstyle ':vcs_info:*' actionformats '%s %b|%a '
zstyle ':vcs_info:svn:*' branchformat '%b:r%r'

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "+"
zstyle ':vcs_info:git:*' unstagedstr "-"
zstyle ':vcs_info:git:*' formats '%s %b%c%u'
zstyle ':vcs_info:git:*' actionformats '(%s)[%b|%a]%c%u'

## ----------------------------------------------------------------------

case "$TERM" in
screen.*)
  unset PROMPT
  precmd_set_windowtitle() { echo -ne "\ek\e\\"; print -Pn "\e]0; %~ %n@%m\a" }
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
*term|*term[-+]*|rxvt*|gnome*)
  precmd_set_windowtitle() { print -Pn "\e]2;%n@%m:%~ (${TTY#/dev/})\a" }
  ;;
esac

precmd() {
  precmd_set_windowtitle
  LC_ALL=en_US.UTF-8 vcs_info
  [[ $PWD == $HOME ]] && CWD='~' || CWD="${PWD/$HOME\//~/}"
}

## ----------------------------------------------------------------------

setopt PROMPT_SUBST

PROMPT=
## 1st-line left prompt:  Current working directory
PROMPT+="%F{blue}"'$CWD'"%f"
## 1st-line right prompt: VCS information
PROMPT+='${(r:($COLUMNS-${#CWD}-${#vcs_info_msg_0_}):: :)}'
PROMPT+="%F{green}"'$vcs_info_msg_0_'"%f"$'\n'
## 2nd-line left prompt:  Username and hostname
PROMPT+="%F{cyan}%B%n@%m%b%f"

if [ "$UID" -eq 0 ]; then
  PROMPT+=" %F{red}%B#%b%f "
else
  PROMPT+=" %F{cyan}%B\$%b%f "
fi

SPROMPT="%F{yellow}%R%f
%F{yellow}%r%f %B[No|Yes|Abort|Edit]?%b "

## Plugin
## ======================================================================

if is-at-least 4.3.17 && [ -d ~/git/zsh-syntax-highlighting ]; then
  source ~/git/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
fi

