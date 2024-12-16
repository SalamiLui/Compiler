format PE console
entry start
include 'C:\FASM\include\win32a.inc'
section '.idata' import data readable writeable
library kernel32, 'kernel32.dll'
import kernel32, \ 
       ExitProcess, 'ExitProcess'
section '.code' code readable executable
start:
    invoke ExitProcess, 0