#
# there are 2 variants:
# CW.EXE is a CauseWay app. Needs text macro CWAPP to be defined in Masm
# and WL32 linker ( with options /f and /ds )
#
# CWJ.EXE: is a HX binary, using jwlink.

#all: Build\cw.exe Build\cwmake.exe
all: Build\cw.exe Build\cwj.exe
#all: Build\cw.exe

DBGLOG =
#DBGLOG = /DDEBUG

Build:
	mkdir Build

Build\cw.exe: Build\cw.obj
	@wl32 /q/m/ds Build\cw.obj;

Build\cw.obj: cw.asm
	@ml /c /DCWAPP /nologo $(DBGLOG) /FlBuild\ /FoBuild\ cw.asm

Build\cwmake.exe: Build\cwmake.obj
	@wl32 /q/ex/m/cs/non Build\cwmake.obj,,Build\cwmake.map

Build\cwmake.obj: cwmake.asm
	@ml /c /nologo $(DBGLOG) /FlBuild\ /FoBuild\ cwmake.asm

Build\cwj.exe: Build\cwj.obj
	@jwlink format win pe hx f Build\cwj.obj n Build\cwj.exe op q,m=Build\cwj.map,stub=loadpero.bin

Build\cwj.obj: cw.asm
	@jwasm /c /nologo $(DBGLOG) /FlBuild\cwj.lst /FoBuild\cwj.obj cw.asm

clean:
	@del Build\*.obj
	@del Build\*.exe
	@del Build\*.map
	@del Build\*.lst
