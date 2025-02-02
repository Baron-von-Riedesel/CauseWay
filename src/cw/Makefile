#
# there are 2 variants:
# CW.EXE is a CauseWay app. Needs text macro CWAPP to be defined in Masm
# and WL32 linker ( with options /f and /ds )
#
# CWJ.EXE: is a HX binary, using jwlink.

!ifndef DEBUG
DEBUG=0
!endif

#all: Build\cw.exe Build\cwmake.exe
all: Build\cw.exe Build\cwj.exe Build\cwmake.exe

!if $(DEBUG)
AOPTD = /D_DEBUG
!else
AOPTD =
!endif

Build:
	mkdir Build

Build\cw.exe: Build\cw.obj
	@wl32 /q/m/ds Build\cw.obj;

Build\cw.obj: cw.asm general.inc
	@ml /c /DCWAPP /nologo $(AOPTD) /FlBuild\ /FoBuild\ cw.asm

Build\cwmake.exe: Build\cwmake.obj
	@wl32 /q/ex/m/cs/non Build\cwmake.obj,,Build\cwmake.map

Build\cwmake.obj: cwmake.asm general.inc
	@ml /c /nologo $(AOPTD) /FlBuild\ /FoBuild\ cwmake.asm

Build\cwj.exe: Build\cwj.obj
	@jwlink format win pe hx f Build\cwj.obj n Build\cwj.exe op q,m=Build\cwj.map,stub=loadpero.bin

Build\cwj.obj: cw.asm
	@jwasm /c /nologo $(AOPTD) /FlBuild\cwj.lst /FoBuild\cwj.obj cw.asm

clean:
	@del Build\*.obj
	@del Build\*.exe
	@del Build\*.map
	@del Build\*.lst
