#
# make cw as HX MZ binary
# make cw with jwasm and jwlink.
# cw32 has been adjusted, it's no longer a CauseWay app ( no cwstub.exe stub )

#all: Build\cw.exe Build\cwmake.exe
all: Build\cw.exe

Build\cw.exe: Build\cw.obj
	@jwlink format win pe hx f Build\cw.obj n Build\cw.exe op q,m=Build\cw.map,stub=loadpero.bin

Build\cw.obj: cw.asm
	@ml /c /nologo /DDEBUG /FlBuild\ /FoBuild\ cw.asm

#Build\cwmake.exe: Build\cwmake.obj
#	@wl32 /q/ex/m/cs/non Build\cwmake.obj,,Build\cwmake.map

#Build\cwmake.obj: cwmake.asm
#	@ml /c /nologo /DDEBUG /FlBuild\ /FoBuild\ cwmake.asm

clean:
	@del Build\*.obj
	@del Build\*.exe
	@del Build\*.map
	@del Build\*.lst
