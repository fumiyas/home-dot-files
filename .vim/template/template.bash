#!/bin/bash

set -u
set -o pipefail || exit $?		## bash 3.0+
shopt -s lastpipe || exit $?		## bash 4.2+
#shopt -s inherit_errexit || exit $?	## bash 4.4+ for set -e
#shopt -s nullglob || exit $?		## bash 2.0+
#shopt -s failglob || exit $?		## bash 3.0+

if tty >/dev/null 2>&1; then
  _pdeco_reset=$(tput sgr0)
  _pdeco_info=$(tput setaf 2)
  _pdeco_warn=$(tput setaf 3)
  _pdeco_error=$(tput setaf 1)
else
  _pdeco_reset=''
  _pdeco_info=''
  _pdeco_warn=''
  _pdeco_error=''
fi

pinfo() {
  echo "$0: ${_pdeco_info}INFO${_pdeco_reset}: $1" 1>&2
}

pwarn() {
  echo "$0: ${_pdeco_warn}WARNING${_pdeco_reset}: $1" 1>&2
}

perr() {
  echo "$0: ${_pdeco_error}ERROR${_pdeco_reset}: $1" 1>&2
}

pdie() {
  perr "$1"
  exit "${2-1}"
}

## ----------------------------------------------------------------------

password_prompt() {
  local for="$1"; shift
  local password password2 ret

  IFS= read -r -s -p "Enter password for $for: " password
  ret="$?"
  echo >/dev/tty 2>/dev/null
  [[ $ret == 0 ]] || pdie "Failed to read password"
  [[ -n "$password" ]] || pdie "No password given"

  IFS= read -r -s -p "Re-enter password for $for to confirm: " password2
  ret="$?"
  echo >/dev/tty 2>/dev/null
  [[ $ret == 0 ]] || pdie "Failed to read password"
  [[ $password == "$password2" ]] || pdie "Passwords does NOT match"

  echo -n "$password"
}

string_escape() {
  local str="$1"; shift
  local escape_char="${1-\\}"

  local escape_char_escaped="${escape_char//\\/\\\\}";
  local needle_char needle_char_escaped

  for needle_char in "$@"; do
    needle_char_escaped="${needle_char//\\/\\\\}";
    str="${str//$needle_char_escaped/$escape_char_escaped$needle_char_escaped}"
  done

  echo -n "$str"
}

## ----------------------------------------------------------------------

_cmds_at_exit=()

# shellcheck disable=SC2317 # Command appears to be unreachable
cmds_at_exit() {
  local cmd

  for cmd in "${_cmds_at_exit[@]}"; do
    "$cmd"
  done
}

# shellcheck disable=SC2154 # ret is referenced but not assigned
trap 'ret="$?"; cmds_at_exit; exit "$ret"' EXIT
for signal in HUP INT TERM; do
  trap 'trap - EXIT '$signal'; cmds_at_exit; kill -'$signal' -$$' $signal
done

## ----------------------------------------------------------------------

_temp_files=()
_cmds_at_exit+=(clean_tempfiles)

create_tempfile() {
  local vname="$1"; shift
  local fname

  if [[ $vname == *[!_0-9A-Za-z]* ]]; then
    perr "${FUNCNAME[0]}: Invalid variable name: $vname"
    return 1
  fi

  fname=$(mktemp "$@" "${TMPDIR:-/tmp}/${0##*/}.XXXXXXXX") || return $?
  _temp_files+=("$fname")
  eval "$vname=\"\$fname\""
}

clean_tempfiles() {
  [[ -n "${_temp_files[0]+set}" ]] && rm -rf "${_temp_files[@]}"
}

## ======================================================================

getopts_want_arg()
{
  if [[ $# -lt 2 ]]; then
    pdie "Option requires an argument: $1"
  fi
  if [[ -n ${3:+set} ]]; then
    if [[ $2 =~ $3 ]]; then
      : OK
    else
      pdie "Invalid value for option: $1: $2"
    fi
  fi
  if [[ -n ${4:+set} ]]; then
    if [[ $2 =~ $4 ]]; then
      pdie "Invalid value for option: $1: $2"
    fi
  fi
}

while [[ $# -gt 0 ]]; do
  opt="$1"; shift

  if [[ -z "${opt##-[!-]?*}" ]]; then
    set -- "-${opt#??}" ${1+"$@"}
    opt="${opt%"${1#-}"}"
  fi
  if [[ -z "${opt##--*=*}" ]]; then
    set -- "${opt#--*=}" ${1+"$@"}
    opt="${opt%%=*}"
  fi

  case "$opt" in
  -f|--force)
    force_p="yes"
    ;;
  -v|--verbose)
    getopts_want_arg "$opt" ${1+"$1"} ${1+"[0-9]"}
    verbose_level="$1"; shift
    ;;
  --)
    break
    ;;
  -*)
    pdie "Invalid option: $opt"
    ;;
  *)
    set -- "$opt" ${1+"$@"}
    break
    ;;
  esac
done

echo "${force_p-}"
echo "${verbose_level-}"
