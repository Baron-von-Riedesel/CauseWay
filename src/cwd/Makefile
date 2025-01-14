
# make CWD.EXE & CWD.OVL, now using WL32 instead of CWL.

ALL	: Build Build\cwd.exe Build\cwd.ovl

Build:
	@mkdir Build

Build\cwd.exe: Build\cwd.obj
	@wl32 /q/ex/m/non/cs Build\cwd.obj,Build\cwd.exe,Build\cwd.map

Build\cwd.obj: cwd.asm cmacros.inc equates.inc ..\strucs.inc
	@ml -c -nologo -FoBuild\ -I.. cwd.asm

Build\cwd.ovl: Build\cwd-ovl.obj
#	@..\cwl\build\cwl.exe /MAP Build\cwd-ovl.obj,Build\cwd.ovl,Build\cwd-ovl.map
	@wl32 /q/m/cs/non Build\cwd-ovl.obj,Build\cwd.ovl,Build\cwd-ovl.map
	@..\..\bin\cwc Build\cwd.ovl

Build\cwd-ovl.obj: cwd-ovl.asm disas.inc generr.inc fpu.inc disaseq.inc win.inc evaluate.inc macros.inc \
		getkeys.inc print.inc fileacc.inc ..\cw.inc ..\strucs.inc
	@ml /c /nologo /FlBuild\ /FoBuild\ /I.. cwd-ovl.asm

clean:
	@del Build\cwd.exe
	@del Build\cwd.ovl
	@del Build\cwd.obj
	@del Build\cwd-ovl.obj
