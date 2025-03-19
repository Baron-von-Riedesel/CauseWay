
# build the CauseWay extender CW32.EXE;
# CWSTUB.EXE: the compressed binary;
# to assemble use either JWasm or Masm v6+.

# known defines:
# BIGSTACK: if defined, allocs 55k (instead 9k) for stacks; see strucs.inc.
# KRNLDBG:  if defined, supports (w)deb386 kernel debugger interface.
# EARLYKDINIT: if defined, kernel debugger can debug early stage of pm init
#           phase.
# PERMNOVM: if defined, same as CAUSEWAY environment variable setting NOVM.
# PERMNOEX: if defined, same as CAUSEWAY environment variable setting NOEX.
# LXWORK:   if defined, enables support for LX.
# DEBUGLE:  if defined, enables debug displays for LE loader.
# DBGOPT:   select debug displays (debug version only) 
# DEFLOWMEM:if defined, sets default for LOWMEM in kB.
# NOLOWMEM: if defined, VMM won't touch conv. memory, LOWMEM is a dummy.
# NOEXPORTS:if defined, disables the kernel exports in api.inc; not needed
#           for dll support.
# NOI21RMHOOK:if defined, removes code for real-mode int 21h hook;
#           PERMNOEX and CAUSEWAY=NOEX are dummies then.
# SRDPMISTATE: if defined, call save/restore DPMI state (barely useful at all)
# COPYFCBS: if 1, copy FCBs in int 21h, ax=4B00h emulation (default 0)
# XMSITEMS: max # of XMS handles the extender can manage (default 32)

ASM=jwasm.exe
#ASM=ml.exe

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
AOPT=-D_DEBUG -Sg -DDEBUG2 -DLLOUT -DDEFLOWMEM=256 -DKRNLDBG -DNOEXPORTS -DDBGOPT=8
OUTD=Debug
DBGINC=dprint16.inc dprint32.inc vioout.inc
!else
OUTD=Build
AOPT=-DDEFLOWMEM=256 -DKRNLDBG -DNOEXPORTS
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

$(OUTD)\cw32.obj: cw32.asm rawvcpi1.inc rawvcpi2.inc interrup.inc ldt.inc memory.inc dosmem.inc api.inc \
		int10h.inc int21h.inc int33h.inc decode_c.inc exceptn.inc equates.inc \
		..\strucs.inc ..\cw.inc loadle.inc load3p.inc $(DBGINC)
	@$(ASM) /c /Cp $(AOPT) /nologo /I.. /Fl$(OUTD)\ -Fo$(OUTD)\ cw32.asm

clean:
	@del $(OUTD)\cw32.exe
	@del $(OUTD)\cwstub.exe
	@del $(OUTD)\cw32.obj
	@del $(OUTD)\cw32.lst
	@del $(OUTD)\cw32.map
