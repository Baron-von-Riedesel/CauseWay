@echo off
ml /DDEBUG1=1 /c /FoBuild\cwdebug.obj cw.asm
wl32 /sy/m/f/ds/i Build\cwdebug,Build\cwdebug
rem d:\devel\watcom\exesplit cwdebug.exe
rem erase file1.dsk
rem erase cwdebug.cw
rem ren file2.dsk cwdebug.cw
rem ..\cwc\cwc /l243 cwdebug.cw
rem erase cwdebug.exe
rem copy /b ..\cw32.exe + cwdebug.cw cwdebug.exe
