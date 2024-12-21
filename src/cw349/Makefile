
# build the causeway extender (CW32.EXE)

ALL	: Build\cw32.exe

Build\cw32.exe: Build\cw32.obj
	@cd Build
	WL32 /q/ex/32/m/cs/non cw32.obj
#	\msvc\bin\link /nologo/non/map/noi cw32.obj;
# jwlink ( and OW wlink ) have problems to identify CW32's stack segment
#	jwlink format dos f cw32.obj op q,m
#	linkold cw32;
#	cwc /m2 /l85 cw32.exe
	@cd ..

# Masm can be replaced by JWasm.

Build\cw32.obj: cw32.asm raw_vcpi.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc \
		..\strucs.inc ..\cw.inc ..\loadle\loadle.inc
	ml /c /Cp /nologo /DENGLISH=1 /I.. /FlBuild\ -FoBuild\ cw32.asm
#	jwasm /c /Cp /nologo /DENGLISH=1 /I.. /FlBuild\ -FoBuild\ cw32.asm

clean:
	@del Build\cw32.exe
	@del Build\cw32.obj
	@del Build\cw32.lst
	@del Build\cw32.map
