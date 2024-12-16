
ALL	: BuildD BuildD\cw32.exe

BuildD:
	@mkdir BuildD

BuildD\cw32.exe	: cw32.asm raw_vcpi.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc \
		..\strucs.inc ..\cw.inc ..\loadle\loadle.inc
	ml /c /nologo /DDEBUG4=1 /DENGLISH=1 /FlBuildD\ /FoBuildD\ /I.. cw32.asm
	@cd BuildD
	WL32 /q/ex/32/m/non cw32.obj
#	cwc /m2 /l85 cw32.exe
	@cd ..
