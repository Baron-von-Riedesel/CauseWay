#
# there are 2 variants:
# CWEM.EXE is a CauseWay app. Needs text macro CWAPP to be defined in Masm
# and WL32 linker ( with options /f and /ds )
#
# CWEMJ.EXE: is a HX binary, using jwlink.

!ifndef DEBUG
DEBUG=0
!endif

NAME=CWEM
NAME2=CWMAKE

ASM=ml.exe

!if $(DEBUG)
AOPTD = /D_DEBUG
ODIR=Debug
!else
AOPTD =
ODIR=Build
!endif

#all: Build\cw.exe Build\cwmake.exe
all: $(ODIR) $(ODIR)\$(NAME).exe $(ODIR)\$(NAME)j.exe $(ODIR)\$(NAME2).exe

$(ODIR):
	mkdir $(ODIR)

$(ODIR)\$(NAME).exe: $(ODIR)\$(NAME).obj
	@wl32 /q/m/ds $(ODIR)\$(NAME).obj;

$(ODIR)\$(NAME).obj: $(NAME).asm general.inc
	@$(ASM) /c /DCWAPP /nologo $(AOPTD) /Fl$(ODIR)\ /Fo$(ODIR)\ $(NAME).asm

$(ODIR)\$(NAME2).exe: $(ODIR)\$(NAME2).obj
	@wl32 /q/ex/m/cs/non $(ODIR)\cwmake.obj,,$(ODIR)\cwmake.map

$(ODIR)\$(NAME2).obj: $(NAME2).asm general.inc
	@$(ASM) /c /nologo $(AOPTD) /Fl$(ODIR)\ /Fo$(ODIR)\ $(NAME2).asm

$(ODIR)\$(NAME)j.exe: $(ODIR)\$(NAME)j.obj
	@jwlink format win pe hx f $(ODIR)\$(NAME)j.obj n $(ODIR)\$(NAME)j.exe op q,m=$(ODIR)\$(NAME)j.map,stub=loadpero.bin

$(ODIR)\$(NAME)j.obj: $(NAME).asm
	@jwasm /c /nologo $(AOPTD) /Fl$(ODIR)\$(NAME)j.lst /Fo$(ODIR)\$(NAME)j.obj $(NAME).asm

clean:
	@del $(ODIR)\*.obj
	@del $(ODIR)\*.exe
	@del $(ODIR)\*.map
	@del $(ODIR)\*.lst
