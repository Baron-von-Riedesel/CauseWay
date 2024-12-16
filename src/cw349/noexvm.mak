
ALL	: Build\cw32.exe

Build\cw32.exe: Build\cw32.obj
	@cd Build
	wl32 /q/ex/32/cs/m/non cw32.obj
	del cw32.obj
	cwc /m2 /l85 cw32.exe
	@cd ..

Build\cw32.obj: cw32.asm raw_vcpi.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc \
		..\strucs.inc ..\cw.inc ..\loadle\loadle.inc
	ml /c /nologo /DPERMNOVM=1 /DPERMNOEX=1 /DENGLISH=1 /FlBuild\ -FoBuild\ /I.. cw32.asm
