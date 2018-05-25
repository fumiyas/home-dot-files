## catch syscall <system call name>
## info b[reakpoint]
## del <number>

set history save on
set history size 99999
set history filename ~/.gdb_history
set print pretty on
set print static-members off
set charset ASCII
add-auto-load-safe-path /usr/share/go/src/pkg/runtime/runtime-gdb.py
