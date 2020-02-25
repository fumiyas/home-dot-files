## info locals
##
## catch syscall <system call name>
## info b[reakpoint]
## del <number>
##
## info threads
## thead <number>

set history save on
set history size 99999
set history filename ~/.gdb_history
set print pretty on
set print static-members off
set charset ASCII
set tui border-kind ascii
add-auto-load-safe-path /usr/share/go/src/pkg/runtime/runtime-gdb.py
