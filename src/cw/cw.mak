
# create cw.exe as CauseWay app by setting Masm option /DCWAPP

all: Build\cw.exe Build\cwmake.exe

Build\cw.exe: Build\cw.obj
	wl32 /q/f/m/cs Build\cw.obj, Build\cw, Build\cw;

Build\cw.obj: cw.asm
	@ml /c /nologo /DCWAPP -FoBuild\ -FlBuild\ cw.asm

Build\cwmake.exe: Build\cwmake.obj
	@wl32 /q/ex/m/cs/non Build\cwmake.obj,,Build\cwmake.map

Build\cwmake.obj: cwmake.asm
	@ml /c /nologo /DDEBUG /FlBuild\ /FoBuild\ cwmake.asm

