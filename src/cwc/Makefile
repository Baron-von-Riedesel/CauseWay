
# tool makeasm.com generates an assembly include file from a binary.
# it's used to translate copystub.com and decstub.com; the include file are
# included into cmc.asm.

LINK = \msvc\bin\link
ASM  = jwasm

ALL: Build Build\cwc.exe

Build:
	@mkdir Build

Build\cwc.exe:	Build\cwc.obj
	@wl32 /q/m/cs/non Build\cwc.obj,Build\cwc.exe,Build\cwc.map,..\..\bin\cw.lib;
#	@..\..\bin\cwc /q /l245 Build\cwc.exe

Build\cwc.obj:	cwc.asm copystub.inc decstub.inc
	@$(ASM) -c -nologo -FlBuild\ -FoBuild\ -I.. cwc.asm

copystub.inc: copystub.asm
	@ml -c -nologo -FlBuild\ -FoBuild\ copystub.asm
	@$(LINK) /TINY Build\copystub.obj;
	@..\..\bin\makeasm copystub.com
	@del copystub.com

decstub.inc: decstub.asm
	@ml -c -nologo -FlBuild\ -FoBuild\ decstub.asm
	@$(LINK) /TINY Build\decstub.obj;
	@..\..\bin\makeasm decstub.com
	@del decstub.com

clean:
	@del Build\cwc.exe
	@del Build\cwc.map
	@del Build\*.obj
	@del Build\*.lst
