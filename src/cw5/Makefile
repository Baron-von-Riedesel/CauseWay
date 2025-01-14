
# build the CauseWay extender CW32.EXE;
# CWSTUB.EXE: the compressed binary;
# to assemble use either JWasm or Masm v6+.

# known defines:
# BIGSTACK: if defined, allocs 55k (instead 9k) for stacks; see strucs.inc
# KRNLDBG: if defined, supports wdeb386 kernel debugger interface
# EARLYKDINIT: if defined, kernel debugger can debug early stage of pm init phase
# PERMNOVM: if defined, same as CAUSEWAY environment variable setting NOVM 
# PERMNOEX: if defined, same as CAUSEWAY environment variable setting NOEX 
# LXWORK: if defined, enables support for LX
# DEBUG4X: if defined, enables debug displays for LE loader

ASM=jwasm.exe

ALL	: Build Build\cw32.exe Build\cwstub.exe

Build:
	@mkdir Build

Build\cwstub.exe: Build\cw32.exe
	@copy Build\cw32.exe Build\cwstub.exe >NUL
	@..\..\bin\cwc /q /m2 /l85 Build\cwstub.exe

# to link use wl32 or MS link;
# jwlink ( and OW wlink ) have problems to identify CW32's stack segment

Build\cw32.exe: Build\cw32.obj
	@cd Build
	@WL32 /q/ex/32/m/cs/non cw32.obj
#	@\msvc\bin\link /nologo/non/map/noi cw32.obj;
#	@jwlink format dos f cw32.obj op q,m
	@cd ..

Build\cw32.obj: cw32.asm rawvcpi1.inc rawvcpi2.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc equates.inc \
		..\strucs.inc ..\cw.inc loadle.inc load3p.inc
	@$(ASM) /c /Cp /nologo /DKRNLDBG /I.. /FlBuild\ -FoBuild\ cw32.asm

clean:
	@del Build\cw32.exe
	@del Build\cwstub.exe
	@del Build\cw32.obj
	@del Build\cw32.lst
	@del Build\cw32.map
