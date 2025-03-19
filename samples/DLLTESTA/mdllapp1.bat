@echo off
rem
rem A simple CauseWay dll sample in assembly.
rem For CW v5.0, kernel imports (in cwapi.obj) are no longer needed, since the API
rem directly supports FindModule, UnFindModule and FindFunction.
rem
jwasm -nologo dllapp1.asm
rem wlink format os2 le f dllapp1.obj libfile ..\..\obj\dllfunc.obj,..\..\obj\cwapi.obj op q,m,stub=cwstub.exe
wlink format os2 le f dllapp1.obj libfile ..\..\obj\dllfunc.obj op q,m,stub=cwstub.exe
rem
jwasm -nologo dll1.asm
wlink format os2 le dll f dll1.obj op q,m export _SayHello
