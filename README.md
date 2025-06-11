# Compileeeeeer

This is a simple compiler for my own invented progaming language,
i didn't investigate too much about compilers and stuff and this is
one of my first "big projects" i have made so the compiler
is a little bit  trash

Im also a non native english speaker so the comments and some vars
may be trash as well 

## Usage
u will need the file with the code to compile with the format fileName.nzi, then compile the file with
```bash
compiler.exe fileName.nzi
```
This will create the new file out.asm with FASM code in PE  format.

If u want to execute it u will need the FASM compiler for windows, also make shure to change the include section in the asm file to the location of ur win32a.inc (it should be in the FASM folder)

