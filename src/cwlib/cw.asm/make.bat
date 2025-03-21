@echo off
if not exist Build\NUL mkdir Build
jwasm -nologo -c -Cx -FoBuild\ *.asm
