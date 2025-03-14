@echo off
rem ml -c -nologo -I.. 3plib.asm
jwasm -c -nologo -Fl -I.. 3plib.asm
wl32 /q/m/cs/f 3plib.obj
