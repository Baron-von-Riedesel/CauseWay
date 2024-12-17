ALL: Build\cwc.exe

# tool makeasm.com generates an assembly include file from a binary.
# it's used to translate copystub.com and decstub.com; the include file are
# included into cmc.asm.

LINK = \msvc\bin\link

Build\cwc.exe:	Build\cwc.obj
#	..\..\bin\cwl /m /sy /flat Build\cwc
	WL32 /q/m/f/cs Build\cwc,,Build\cwc.map,..\..\bin\cw.lib;
#	cwc /l245 cwc.exe

Build\cwc.obj:	cwc.asm copystub.inc decstub.inc
	@ml -c -nologo -Cx /DENGLISH=1 /Zf /Zd -FlBuild\ -FoBuild\ -I.. cwc.asm

copystub.inc: copystub.asm
	@ml -c -nologo -FlBuild\ -FoBuild\ copystub.asm
	@$(LINK) /TINY Build\copystub.obj;
	..\..\bin\makeasm copystub.com
	@del copystub.com

decstub.inc: decstub.asm
	@ml -c -nologo -FlBuild\ -FoBuild\ decstub.asm
	@$(LINK) /TINY Build\decstub.obj;
	..\..\bin\makeasm decstub.com
	@del decstub.com
