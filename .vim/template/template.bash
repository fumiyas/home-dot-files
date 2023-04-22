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

_cmds_at_exit=()

cmds_at_exit() {
  local cmd

  for cmd in "${_cmds_at_exit[@]}"; do
    "$cmd"
  done
}

trap 'cmds_at_exit' EXIT
for signal in HUP INT TERM; do
  trap 'cmds_at_exit; trap - EXIT '$signal'; kill -'$signal' -$$' $signal
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
