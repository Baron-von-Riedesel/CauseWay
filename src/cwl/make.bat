@echo off
if not exist Build\NUL mkdir Build
rem ml -c -nologo /Cx /Zf /Zd /Fl cwl.asm
jwasm -c -nologo /Cx /Zf /Zd /FlBuild\ /FoBuild\ cwl.asm
wl32 /f /m /cs Build\cwl.obj, Build\cwl.exe, Build\cwl.map