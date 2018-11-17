#!/bin/bash

set -u
set -o pipefail || exit $?	## bash 3.0+
shopt -s lastpipe || exit $?	## bash 4.2+
shopt -s nullglob || exit $?	## bash 2.0+
#shopt -s failglob || exit $?	## bash 3.0+

