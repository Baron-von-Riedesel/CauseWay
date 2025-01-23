
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

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
AOPTD=-D_DEBUG -Sg
OUTD=Debug
DBGINC=dprint16.inc dprint32.inc
!else
OUTD=Build
!endif

ALL	: $(OUTD) $(OUTD)\cw32.exe $(OUTD)\cwstub.exe

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\cwstub.exe: $(OUTD)\cw32.exe
	@copy $(OUTD)\cw32.exe $(OUTD)\cwstub.exe >NUL
	@..\..\bin\cwc /q /m2 /l85 $(OUTD)\cwstub.exe

# to link use wl32 or MS link;
# jwlink ( and OW wlink ) have problems to identify CW32's stack segment

$(OUTD)\cw32.exe: $(OUTD)\cw32.obj
	@cd $(OUTD)
	@WL32 /q/ex/32/m/cs/non cw32.obj
#	@\msvc\bin\link /nologo/non/map/noi cw32.obj;
#	@jwlink format dos f cw32.obj op q,m
	@cd ..

$(OUTD)\cw32.obj: cw32.asm rawvcpi1.inc rawvcpi2.inc interrup.inc ldt.inc memory.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc equates.inc \
		..\strucs.inc ..\cw.inc loadle.inc load3p.inc $(DBGINC)
	@$(ASM) /c /Cp $(AOPTD) /nologo /DKRNLDBG /I.. /Fl$(OUTD)\ -Fo$(OUTD)\ cw32.asm

clean:
	@del $(OUTD)\cw32.exe
	@del $(OUTD)\cwstub.exe
	@del $(OUTD)\cw32.obj
	@del $(OUTD)\cw32.lst
	@del $(OUTD)\cw32.map
