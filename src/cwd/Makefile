
# current problem: the /3P option must be used for WL32;
# if using /f, lots of (invalid) relocations are written.

EXTENDERSTUB=\div\cwstub.exe

ALL	: Build\cwd.exe Build\cwd.ovl

Build\cwd.exe: Build\cwd.obj
	@wl32 /q/ex/m/non/cs Build\cwd.obj,Build\cwd.exe,Build\cwd.map

Build\cwd.obj: cwd.asm cmacros.inc equates.inc
	@ml -c -nologo -FoBuild\ cwd.asm

Build\cwd.ovl: Build\cwd-ovl.obj
#	..\cwl\build\cwl.exe /MAP Build\cwd-ovl.obj,Build\cwd.ovl,Build\cwd-ovl.map
#	wl32 /m/f/cs Build\cwd-ovl.obj,Build\cwd.ovl,Build\cwd-ovl.map
	wl32 /m/3p/cs Build\cwd-ovl.obj,Build\cwd.tmp,Build\cwd-ovl.map
	@copy /b $(EXTENDERSTUB) + Build\cwd.tmp Build\cwd.ovl

Build\cwd-ovl.obj: cwd-ovl.asm disas.inc generr.inc fpu.inc disaseq.inc ..\cw.inc ..\strucs.inc win.inc evaluate.inc
	@ml /c /nologo /DENGLISH=1 /FlBuild\ /FoBuild\ cwd-ovl.asm
