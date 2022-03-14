#!/bin/bash

set -u
set -o pipefail || exit $?	## bash 3.0+
shopt -s lastpipe || exit $?	## bash 4.2+
shopt -s nullglob || exit $?	## bash 2.0+
#shopt -s failglob || exit $?	## bash 3.0+

perr() {
  echo "$0: ERROR: $1" 1>&2
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

