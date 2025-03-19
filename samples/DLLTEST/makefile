#
# Make file for CauseWay DLL support example.
# For CauseWay v5.0, kernel imports are no longer required,
# because the API directly supports FindModule, UnFindModule, FindFunction.
#
OBJDIR=..\..\obj

#IMPORTS=$(OBJDIR)\cwapi.obj

all: dlltest.exe dlls.dll dllr.dll

dlltest.exe : dlltest.c dllfunc.h $(IMPORTS) $(OBJDIR)\dllfunc.obj
	wcc386 /q /s /4s /d2 dlltest.c
	wlink @<<
name DLLTEST
debug all
system causeway
file dlltest.obj
libfile {$(OBJDIR)\dllfunc.obj $(IMPORTS) }
op q,m
<<

dlls.dll : dll.c
	wcc386 /q /s /4s /bd dll.c /fo=dlls.obj
	wlink @<<
name DLLS
system cwdlls
file dlls.obj
op q,m
<<

dllr.dll : dll.c
	wcc386 /q /s /4r /bd dll.c /fo=dllr.obj
	wlink @<<
name DLLR
system cwdllr
file dllr.obj
op q,m
<<
