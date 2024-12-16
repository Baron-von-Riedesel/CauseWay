
NOCOMP	: Build\cw32.exe

Build\cw32.exe: cw32.asm raw_vcpi.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc \
		..\strucs.inc ..\cw.inc ..\loadle\loadle.inc
	ml /c /nologo /DENGLISH=1 /FlBuild\ -FoBuild\ /I.. cw32.asm
	cd Build
	wl32 /q/ex/32/m/non/cs cw32.obj
	cd ..

