#
all:	screen32.exe

screen32.exe : screen32.obj
	@wl32 /f/m screen32.obj

screen32.obj: screen32.asm
	@jwasm -nologo screen32.asm


