
# build the CauseWay extender with JWasm, no linker.
# CW32.EXE: uncompressed
# CWSTUB.EXE: compressed

ALL	: Build Build\cw32.exe Build\cwstub.exe

Build:
	@mkdir Build

Build\cwstub.exe: Build\cw32.exe
	@copy Build\cw32.exe Build\cwstub.exe >NUL
	@..\..\bin\cwc /q /m2 /l85 Build\cwstub.exe

Build\cw32.exe: cw32.asm rawvcpi1.inc rawvcpi2.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc ..\strucs.inc ..\cw.inc loadle.inc load3p.inc
	@jwasm -Cp -mz -nologo -DKRNLDBG -I.. -FlBuild\ -FoBuild\ cw32.asm

clean:
	@del Build\cw32.exe
	@del Build\cwstub.exe
	@del Build\cw32.lst
