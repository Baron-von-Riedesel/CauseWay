;
;CauseWay v2 main file.
;
;
;Things to note:
;
;The IDT has a page to itself even though it only needs 2k. For RAW/VCPI systems
;the top 2k could be used for something else.
;
;Things to do:
;
;Put in some sort of system stack overflow checking and terminate the program
;if it happens.
;

	include general.inc
	include equates.inc
	include strucs.inc
	include cw.inc
;	include cw-undoc.inc

ifndef SPANISH
SPANISH	equ	0
endif
ifndef ENGLISH
ENGLISH	equ	1-SPANISH
endif

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Main code segment. This takes care of calling the right initialisation routines
;and generaly getting everything rolling.
;
_cwMain	segment para public 'Main thread' use16
	assume cs:_cwMain, ds:_cwMain, ss:_cwStack
;
;Want a copyright message embedded first.
;
Copyright	label byte
	db 'CauseWay DOS Extender v'
VersionMajor	db '3.'
VersionMinor	db '49'
	db " Public Domain.",13,10,0


	.386
;-------------------------------------------------------------------------------
;
;Final init stuff.
;
cwOpen	proc	near
	assume ds:nothing		;make our data addresable.
	mov	ds,cs:DataSegment
	assume ds:_cwMain
	;
	mov	d[TerminationHandler],offset cwClose
	mov	w[TerminationHandler+4],cs
;
;Now we know the machines details, re-size the program
;block again to release as much memory as possible.
;
	mov	edi,offset Int21Buffer
	push	ds
	pop	es
	mov	ax,RealPSPSegment
	mov	es:RealRegsStruc.Real_ES[edi],ax
	mov	bx,_cwDPMIEMU
	cmp	ProtectedType,2
	jnz	@@KeepRaw
	mov	bx,_cwRaw
@@KeepRaw:	sub	bx,ax		;Size program.
	inc	bx
	mov	TSRSize,bx
	mov	es:RealRegsStruc.Real_EBX[edi],ebx
	mov	es:RealRegsStruc.Real_EAX[edi],4a00h
	mov	bl,21h
	mov	ErrorNumber,1
	Sys	IntXX
	test	es:w[edi+RealRegsStruc.Real_Flags],1
	jnz	@@9
	mov	ErrorNumber,0	;clear error number.
;
;Force accurate memory values.
;
	or	ecx,-1
	Sys	GetMemLinear32
;
;Enable resource tracking and MCB allocations.
;
	or	ResourceTracking,-1	;Enable resource tracking.
;
;Run the main program.
;
	mov	edx,offset MainExec	;name to exec.
	mov	esi,80h
	mov	es,PSPSegment
	xor	cx,cx
	Sys	cwExec		;run the bugger.
	jnc	@@8
	add	ax,10-1		;convert error number.
	mov	ErrorNumber,ax
	jmp	@@9
@@8:	cmp	DebugDump,0
	jnz	@@9
	mov	ErrorLevel,ax	;store programs error level.
	mov	ErrorNumber,0	;clear error number.
@@9:	jmp	cwClose
cwOpen	endp


;-------------------------------------------------------------------------------
;
;Shut everything down.
;
cwClose	proc	near

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	push	cs
	pop	ds
	mov	edx,OFFSET debugatext1
debugaloop2:
	cmp	BYTE PTR ds:[edx],0
	je	debugab
	mov	ecx,1
	mov	bx,1
	mov	ah,40h
	int	21h
	inc	edx
	jmp	debugaloop2
debugab:
	mov	edx,OFFSET debugatext2
	push	cs
	pop	ds
	mov	ecx,2
	mov	bx,1
	mov	ah,40h
	int	21h
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debugaout

debugatext1	DB	'Entering cwClose...',0
debugatext2	DB	13,10

debugaout:
	push	ecx
	mov	ecx,100000h
debugaloop:
;	dec	ecx
;	jne	debugaloop
	pop	ecx
ENDIF

	assume ds:nothing
	mov	ds,cs:DataSegment
	assume ds:_cwMain
	;
	mov	ResourceTracking,0
	mov	mcbAllocations,0
	sti
	mov	w[TerminationHandler+4],0
	;

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds

	mov	dx,w[ExceptionExtension+12]
;	lar	ax,dx
	cmp	dx,127h
	jnz	debughout
;	and	ah,3
;	cmp	ah,3
;	jne	debughout

	push	cs
	pop	ds
	mov	edx,OFFSET debughtext1
debughloop2:
	cmp	BYTE PTR ds:[edx],0
	je	debughb
	mov	ecx,1
	mov	bx,1
	mov	ah,40h
	int	21h
	inc	edx
	jmp	debughloop2
debughb:
	mov	edx,OFFSET debughtext2
	push	cs
	pop	ds
	mov	ecx,2
	mov	bx,1
	mov	ah,40h
	int	21h
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debughout

debughtext1	DB	'Calling DebugDisplay...',0
debughtext2	DB	13,10

debughout:
	push	ecx
	mov	ecx,100000h
debughloop:
;	dec	ecx
;	jne	debughloop
	pop	ecx

ENDIF

	push	cs
	push	offset @@dd0
	push	w[ExceptionExtension+12]
	mov	eax,offset DebugDisplay
	push	ax
	retf
@@dd0:	;

	cmp	ErrorNumber,0
	jz	@@NoError

	mov	ax,ErrorNumber	;Get the error number.
	xor	dx,dx
	mov	cx,10
	div	cx
	add	al,'0'
	mov	b[ErrorM00n],al
	add	dl,'0'
	mov	b[ErrorM00n+1],dl

	cmp	EnableDebugDump,0	; if debug dump turned off, no screen i/o
	je	@@NoError

	xor	edx,edx
	mov	dx,w[ErrorList]
	mov	ah,9
	int	21h
;
;Get a pointer to the appropriate error message and print it.
;
	mov	bx,ErrorNumber
	add	bx,bx
	xor	edx,edx
	mov	dx,[ErrorList+bx]
	mov	ah,9
	int	21h
;
;Now exit with the error number as the DOS "errorlevel".
;
@@NoError:
;
;Remove extension patches.
;

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	push	cs
	pop	ds
	mov	edx,OFFSET debugctext1
debugcloop2:
	cmp	BYTE PTR ds:[edx],0
	je	debugcb
	mov	ecx,1
	mov	bx,1
	mov	ah,40h
	int	21h
	inc	edx
	jmp	debugcloop2
debugcb:
	mov	edx,OFFSET debugctext2
	push	cs
	pop	ds
	mov	ecx,2
	mov	bx,1
	mov	ah,40h
	int	21h
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debugcout

debugctext1	DB	'Removing interrupt patches...',0
debugctext2	DB	13,10

debugcout:
	push	ecx
	mov	ecx,100000h
debugcloop:
;	dec	ecx
;	jne	debugcloop
	pop	ecx
ENDIF

	mov	di,offset ExtensionList	;list of interupt patches.
@@p1:	cmp	w[di+2],-1		;search for the end of the table so we can restore
	jz	@@p0		;vectors in reverse order.
	add	di,2
	jmp	@@p1
	;
@@p0:	mov	bp,w[di]
	cmp	ds:w[bp+32],-1	;installed?
	jnz	@@p2
	Pushm	di,bp
	push	cs
	push	offset @@p3
	push	ds:w[bp+20]
	push	ds:w[bp+16]
	retf
@@p3:	Popm	di,bp
@@p2:	sub	di,2
	cmp	di,offset ExtensionList-2
	jnz	@@p0
;
;Remove api exception patches.
;
	cmp	apiExcepPatched,0
	jz	@@pe0
	mov	ax,cs
	push	ax
	mov	ax,offset @@pe0
	push	ax
	mov	ax,apiCodeSeg
	push	ax
	mov	eax,offset UnPatchExc
	push	ax
	retf
@@pe0:	;
;
;Remove the API patch.
;
	mov	es,apiDataSeg
	assume es:_apiCode
	test	SystemFlags,1
	jz	@@Use32
	mov	dx,es:w[OldIntSys]
	mov	cx,es:w[OldIntSys+2]
	jmp	@@Use0
@@Use32:	mov	edx,es:d[OldIntSys]
	mov	cx,es:w[OldIntSys+4]
@@Use0:	mov	bl,31h
	mov	ax,205h
	int	31h
	mov	es:d[cwIdentity],0
	mov	es:d[cwIdentity+4],0
	assume es:nothing
	;
@@noAPI:
	cmp	ProtectedType,2	;DPMI?
	jz	@@DPMI

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	push	es
	mov	ax,KernalZero
	mov	es,ax
	mov	ebx,0b8000h
	mov	BYTE PTR es:[ebx],'1'
	push	cs
	pop	ds
	mov	ecx,OFFSET debugdtext1
debugdloop2:
	cmp	BYTE PTR ds:[ecx],0
	je	debugdb
	mov	dl,ds:[ecx]
	mov	ah,2
	int	21h
	inc	ecx
	jmp	debugdloop2
debugdb:
	mov	dl,13
	mov	ah,2
	int	21h
	mov	dl,10
	mov	ah,2
	int	21h
	pop	es
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debugdout

debugdtext1	DB	'Making raw addressable...',0
debugdtext2	DB	13,10

debugdout:
	push	ecx
	mov	ecx,100000h
debugdloop:
	dec	ecx
	jne	debugdloop
	pop	ecx
ENDIF

;
;Make RAW stuff addressable.
;
	cli			;Don't want interrupts interfering.
	mov	ax,KernalDS		;Get supervisor data descriptor,
	mov	ds,ax		;DS,ES,FS,GS,SS must be data with 64k limit
	assume ds:_cwRaw
	mov	ax,KernalZero
	mov	es,ax
;
;Switch to RAW exit code.
;
	push	_cwMain
	push	offset @@6
	mov	ax,KernalCS
	push	ax
	mov	ax,offset RawVCPIRealMode
	push	ax
	retf
@@6:	jmp	@@RealMode
;
;Remove DPMI stuff.
;
@@DPMI:

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	push	cs
	pop	ds
	mov	ecx,OFFSET debugetext1
debugeloop2:
	cmp	BYTE PTR ds:[ecx],0
	je	debugeb
	mov	dl,ds:[ecx]
	mov	ah,2
	int	21h
	inc	ecx
	jmp	debugeloop2
debugeb:
	mov	dl,13
	mov	ah,2
	int	21h
	mov	dl,10
	mov	ah,2
	int	21h
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debugeout

debugetext1	DB	'Removing DPMI setup...',0
debugetext2	DB	13,10

debugeout:
	push	ecx
	mov	ecx,100000h
debugeloop:
	dec	ecx
	jne	debugeloop
	pop	ecx
ENDIF

	assume ds:nothing
	mov	ds,cs:DataSegment
	assume ds:_cwMain

	if	0
	cmp	d[OldInt21hExec],0
	jz	@@d0
	mov	bl,21h
	mov	dx,w[OldInt21hExec]
	mov	cx,w[OldInt21hExec+2]
	mov	ax,201h
	int	31h
@@d0:	movzx	edx,dx
	endif

	jmp	@@InRealMode
;
;Make sure our data is addressable.
;
@@RealMode:	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
;
;Display the "CauseWay error: ??" bit.
;
@@InRealMode:

	if	0
	cmp	ErrorNumber,0
	jz	@@NoError

	push	ax
	mov	al,3
	call	bordm
	pop	ax

	mov	ax,ErrorNumber	;Get the error number.
	xor	dx,dx
	mov	cx,10
	div	cx
	add	al,'0'
	mov	b[ErrorM00n],al
	add	dl,'0'
	mov	b[ErrorM00n+1],dl

	cmp	EnableDebugDump,0	; if debug dump turned off, no screen i/o
	je	@@NoError

	xor	edx,edx
	mov	dx,w[ErrorList]
	mov	ah,9
	int	21h
;
;Get a pointer to the appropriate error message and print it.
;
	mov	bx,ErrorNumber
	add	bx,bx
	mov	dx,[ErrorList+bx]
	mov	ah,9
	int	21h
;
;Now exit with the error number as the DOS "errorlevel".
;
@@NoError:
	endif

; MED, 12/24/99, coalesce free memory by attempting to allocate largest possible
;  with upper memory in the chain
	mov	ax,5800h
	int	21h
	push	ax
	mov	ax,5802h
	int	21h
	push	ax
	mov	bx,1
	mov	ax,5803h
	int	21h
	mov	bx,81h
	mov	ax,5801h
	int	21h

	mov	ah,48h
	mov	bx,-1
	int	21h

	pop	bx
	mov	ax,5803h
	int	21h
	pop	bx
	mov	ax,5801h
	int	21h

	mov	ax,ErrorNumber
	or	ax,ax
	jnz	@@Exit
	mov	ax,ErrorLevel
@@Exit:

IFDEF DEBUG4
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	push	cs
	pop	ds
	mov	edx,OFFSET debugbtext1
debugbloop2:
	cmp	BYTE PTR ds:[edx],0
	je	debugbb
	mov	ecx,1
	mov	bx,1
	mov	ah,40h
	int	21h
	inc	edx
	jmp	debugbloop2
debugbb:
	mov	edx,OFFSET debugbtext2
	push	cs
	pop	ds
	mov	ecx,2
	mov	bx,1
	mov	ah,40h
	int	21h
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	jmp	debugbout

debugbtext1	DB	'Performing final real mode 4ch termination...',0
debugbtext2	DB	13,10

debugbout:
	push	ecx
	mov	ecx,100000h
debugbloop:
	dec	ecx
	jne	debugbloop
	pop	ecx
ENDIF

	mov	ah,4ch
	int	21h
cwClose	endp


;-------------------------------------------------------------------------------
Int21hExecPatch proc	near
	assume ds:nothing
	pushf
	cmp	ax,4b00h
	jnz	@@Old

	inc	cs:Int21hExecCount

	popf
	pushf
	call	cs:d[OldInt21hExec]

	pushf
	push bp
	push ax
	mov bp,sp
	mov ax,[bp+2+2]
	and ax,1
;	and w[bp+2+2+2+2+2],1
	and w[bp+2+2+2+2+2],0fffeh	; MED 01/24/96

	or w[bp+2+2+2+2+2],ax
	pop ax
	pop bp
	popf

	dec	cs:Int21hExecCount

	iret
@@Old:
	popf
	jmp	cs:d[OldInt21hExec]
OldInt21hExec	dd 0
	assume ds:_cwMain
Int21hExecCount db 0
Int21hExecPatch endp
	.386p


;-------------------------------------------------------------------------------
Bordm	proc	near
	Pushm	ax,dx
	mov	ah,al
	mov	dx,3dah
	in	al,dx
	mov	dl,0c0h
	mov	al,11h
	out	dx,al
	mov	al,ah
	out	dx,al
	mov	al,20h
	out	dx,al
	Popm	ax,dx
	ret
Bordm	endp

;-------------------------------------------------------------------------------
;
;Some global data.
;
RealPSPSegment	dw ?		;Real mode PSP segment.
RealENVSegment	dw ?		;Real mode environment segment.
ProtectedFlags	dw 0		;Bit significant, 0-DPMI,1-VCPI,2-RAW.
ProtectedType	dw 0		;0-RAW,1-VCPI,2-DPMI.
ProtectedForce	db 0
DOSVersion	dw 0
SystemFlags	dd 0
apiCodeSeg	dw 0
apiDataSeg	dw 0
;
CodeSegment	dw MainCS	;Setup CS selector.
CodeSegmenti	dw InitCS
DataSegment	dw MainDS	;Setup DS selector.
DataSegmenti	dw InitDS
StackSegment	dw MainSS
RealSegment	dw KernalZero
PSPSegment	dw MainPSP
ENVSegment	dw MainEnv
BasePSP	dw 0
BasePSPAddress	dd 0
;
TSRSize	dw 0
;
ErrorNumber	dw 0
ErrorLevel	dw 0
ErrorList	dw ErrorM00,ErrorM01,ErrorM02,ErrorM03,ErrorM04,ErrorM05,ErrorM06,ErrorM07
	dw ErrorM08,ErrorM09,ErrorM10,ErrorM11,ErrorM12,ErrorM13,ErrorM14,ErrorM15
	dw ErrorM16
ErrorM00	db 'CauseWay error '
ErrorM00n	db '00 : $'
ErrorM01	label byte
if ENGLISH
	db 'Unable to re-size program memory block.',13,10,'$'
elseif SPANISH
	db "Incapaz de redimensionar el bloque de memoria del programa.",13,10,"$"
endif
ErrorM02	db "$"
ErrorM03	db "$"
ErrorM04	db "$"
ErrorM05	label byte
if ENGLISH
	db 'Not enough memory for CauseWay.',13,10,'$'
elseif SPANISH
	db "Memoria insuficiente para CauseWay.",13,10,"$"
endif
ErrorM06	db "$"
ErrorM07	db "$"
ErrorM08	db "$"
ErrorM09	label byte
if ENGLISH
	db 'Unrecoverable exception. Program terminated.',13,10,'$'
elseif SPANISH
	db "Excepcion irrecuperable. Programa terminado.",13,10,"$"
endif
ErrorM10	label byte
if ENGLISH
	db 'Unable to find '
ErrorM10_T	db 'application to load.',13,10,'$',32 dup (0)
elseif SPANISH
	db "Incapaz de encontrar '
ErrorM10_T	db 'aplicacion a cargar.",13,10,"$",32 dup (0)
endif
ErrorM11	label byte
if ENGLISH
	db 'DOS reported an error or corrupt file found.'
elseif SPANISH
	db "DOS informo de un error o de un fichero corrupto."
endif
ErrorM11_0	db 13,10,'$'
	db "No:"
ErrorM11_1	db "00000000",13,10,"$"
ErrorM12	label byte
if ENGLISH
	db 'Not enough memory to load application.',13,10,'$'
elseif SPANISH
	db "Memoria insuficiente para cargar la aplicacion.",13,10,"$"
endif
ErrorM13	db "$"
ErrorM14	label byte
	if ENGLISH
	db 'Memory structures destroyed. Program terminated.',13,10,'$'
	elseif SPANISH
	db "Estructuras de memoria destruidas. Programa terminado.",13,10,"$"
	endif
ErrorM15	label byte
	if ENGLISH
	db 'DOS reported an error while accessing swap file. Program terminated.',13,10,'$'
	elseif SPANISH
	db "DOS informa de un error mientras se accede al fichero de swap. Programa terminado.",13,10,"$"
	endif
ErrorM16	label byte
	if ENGLISH
	db "Unsupported DOS function call, program terminated.",13,10,"$"
	elseif SPANISH
	db "Llamada a funcion no soportada del DOS, programa terminado.",13,10,"$"
	endif
;
ALIGN 4
MainExec	db 128 dup (0)
;
DtaBuffer	db 128 dup (0)
;
TransferSize	dd 8192
TransferReal	dw ?
;
MouseETarget	dd 0,0
ResourceTracking dw 0
ForcedFind	dd 0,0
mcbAllocations	dw 0
LinearAddressCheck db 0
;
TerminationHandler dd offset InitError,InitCS
UserTermRoutine	DF	0
UserTermDump	DF	0	; dump location for register info
;
DPMIStateAddr	df 0
DPMIStateSize	dd 0
DPMIStackOff	dw ?
DPMIStackSeg	dw ?
;
DebugDump	db 0
EnableDebugDump	DB	1
UserTermFlag	DB	0
Pad1Flag	DB	0

IFDEF PERMNOEX
NoEXECPatchFlag	DB	1
ELSE
NoEXECPatchFlag	DB	0
ENDIF

; MED, 11/11/99
; used to flag checking XMS total memory because EMM386 lies and acts as VCPI
;  host when NOVCPI set, but provides no memory
VCPIHasNoMem	DB	0

NewCWErrName	DB	81 DUP (0)
DOS4GFlag	db 0
;
;*** MED change for Michael Devore name
CopyCheck	label byte
	db 'C'-44,'a'-44,'u'-44,'s'-44,'e'-44,'W'-44,'a'-44,'y'-44,' '-44
	db 'D'-44,'O'-44,'S'-44,' '-44,'E'-44,'x'-44,'t'-44,'e'-44,'n'-44
	db 'd'-44,'e'-44,'r'-44,' '-44,'v'-44,255,255,255,255
	db ' '-44,'C'-44,'o'-44,'p'-44,'y'-44,'r'-44,'i'-44,'g'-44
	db 'h'-44,'t'-44,' '-44,'1'-44,'9'-44,'9'-44,'2'-44,'-'-44,'9'-44
	db '9'-44,' '-44,'M'-44
	db 'i'-44,'c'-44,'h'-44,'a'-44,'e'-44,'l'-44,' '-44,'D'-44,'e'-44
	db 'v'-44,'o'-44,'r'-44,'e'-44,'.'-44,13-44,10-44,'A'-44,'l'-44,'l'-44
	db ' '-44,'r'-44,'i'-44,'g'-44,'h'-44,'t'-44,'s'-44,' '-44,'r'-44
	db 'e'-44,'s'-44,'e'-44,'r'-44,'v'-44,'e'-44,'d'-44,'.'-44,13-44,10-44
CopyCount	dw 0
;
Int21Buffer	db size RealRegsStruc dup (?)
Int10Buffer	db size RealRegsStruc dup (?)
Int33Buffer	db size RealRegsStruc dup (?)
;
apiExcepPatched db 0
;
ExtensionList	label word
	dw ExceptionExtension
	dw Int21hExtension,Int10hExtension,Int33hExtension
	dw -1
;
ExceptionExtension dw _Excep,0
	dd offset ExcepEnd-ExcepStart
	dd offset ExcepOpen,?
	dd offset ExcepClose,?
	dd 0
	dd 0
	dd 0
;
Int21hExtension dw _Int21h,1		;segment start.
	dd offset Int21hEnd-Int21hStart	;segment length.
	dd offset Int21hOpen,? 	;init code.
	dd offset Int21hClose,?	;remove code.
	dd 0			;code selector.
	dd 0			;data selector.
	dd 0			;installed flag.
;
Int10hExtension dw _Int10h,1		;segment start.
	dd offset Int10hEnd-Int10hStart	;segment length.
	dd offset Int10hOpen,?	;init code.
	dd offset Int10hClose,?	;remove code.
	dd 0			;code selector.
	dd 0			;data selector.
	dd 0			;installed flag.
;
Int33hExtension dw _Int33h,1		;segment start.
	dd offset Int33hEnd-Int33hStart	;segment length.
	dd offset Int33hOpen,?	;init code.
	dd offset Int33hClose,?	;remove code.
	dd 0			;code selector.
	dd 0			;data selector.
	dd 0			;installed flag.
;
Temp0_	dd ?
Temp1_	dd ?
;
DebugUserOff	DD	?
DebugUserSel	DW	?
DebugUserCount	DW	0	; must be initialized, nonzero value flags operation
DebugAsciiFlag	DB	?
_cwMain	ends


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Default stack used during startup and exit.
;
;--- wlink won't recognize this segment as stack if class isn't 'STACK'
_cwStack	segment para stack 'main stack' use16
	db 2048 dup (0)
_cwStackEnd	label byte
_cwStack	ends


	include raw_vcpi.inc

	include api.inc
	include exceptn.inc
	include int10h.inc
	include int21h.inc
	include int33h.inc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;The initialiseation code seg. Takes care of things like checking for the right
;processor and determining how we're going to get into protected mode.
;
_cwInit	segment para public 'init code' use16
	assume cs:_cwInit, ds:_cwMain

dpmiSelBuffer	db 8 dup (0)

;-------------------------------------------------------------------------------
Startup	proc	near
;
;Make global data addresable.
;
	mov	ax,_cwMain
	mov	ds,ax

; MED, 12/30/99
; works around weird bug in some machines due to Windows/hardware/BIOS??? error
;  where a CauseWay application in AUTOEXEC.BAT which uses DOS function 8
;  to get a keystroke when no other application in AUTOEXEC.BAT gets a
;  keystroke will cause failure in Windows 98 (and 95?) when opening a DOS
;  box after Windows startup.  Caused by A20 or keyboard port stabilization?
	mov	cx,127
kloop:
	mov	ax,0b00h
	int	21h
	test	al,al
	jne	done
	loop	kloop
done:

;
;Stow real mode PSP and environment values, we'll need them later.
;
	mov	RealPSPSegment,es
	mov	ax,es:w[02ch]
	mov	RealENVSegment,ax	;Stow ENV for later.
;
;Re-size memory so we can allocate what we want high.
;
	mov	cs:IErrorNumber,1
	mov	ax,es
	mov	bx,_cwEnd		;Get program end segment.
	sub	bx,ax		;Size program.
	inc	bx
	mov	ah,4ah
	int	21h		;Re-size memory block.

; MED 06/16/97
;	jc	InitError
	jnc	chk386

toiniterr:
	jmp	InitError

;
;Check we're on at least a 386 system.
;
chk386:
	mov	cs:IErrorNumber,2
	call	CheckProcessor

;	jc	InitError
	jc	toiniterr

;
;Check DOS version is high enough.
;
	mov	cs:IErrorNumber,4
	call	CheckDOSVersion
	jc	InitError
;
;Get execution name from environment.
;
	call	GetEXECName
;
;Retrieve setup info from 3P header.
;
	call	GetSystemFlags
;
;Check if a suitable method for switching to protected mode exists.
;
	call	GetProtectedType
	mov	cs:IErrorNumber,3
	cmp	ProtectedFlags,0	;Any types available?
	jz	InitError
;
;Get CAUSEWAY environment variable settings.
;
	call	GetENVStuff
;
;Decide which environment to use.
;
	call	SetProtectedType
;
;Move the DTA to where we can get at it in the future.
;
	mov	dx,offset DtaBuffer
	mov	ah,1ah
	int	21h
;
;now see about type specific initialisations.
;
	cmp	ProtectedType,2	;DPMI initialiseation?
	jz	@@InitDPMI
;
;Useing either RAW or VCPI so do the stuff that's common to both for now.
;
	mov	ax,_cwRaw
	mov	ds,ax
	assume ds:_cwRaw
;
;Get SDA address so VMM can change BREAK state.
;
	.386
	push	ds
	mov	ax,5d06h
	int	21h
	mov	ax,ds
	pop	ds
	add	si,17h
	movzx	eax,ax
	shl	eax,4
	movzx	esi,si
	add	eax,esi
	mov	BreakAddress,eax
;
;Find out if XMS is present.
;
	mov	ax,4300h		;XMS install check.
	int	2fh
	cmp	al,80h		;XMS present?
	jnz	@@NoXMS
;
;XMS detected so work out max block size and entry point.
;
	mov	ax,4310h		;Get XMS API.
	int	2fh
	mov	w[XMSControl],bx
	mov	w[XMSControl+2],es
	mov	XMSPresent,1		;flag XMS is available.

; MED, 09/10/99, support extended XMS API to calculate XMS available to CauseWay
;  (maximum of 2G-32K, i.e. 32 handles/entries of 64K-1)
	mov	ah,0
	call	d[XMSControl]	; get info
	cmp	ah,3
	jb	xms2
	cmp	bh,3
	jb	xms2
	cmp	bl,8
	jb	xms2			; treat early 3.x drivers < 3.08 as 2.x

; use extended XMS API
	mov	XMSVer3Present,1	; flag XMS 3.x driver present
	mov	ah,88h
	call	d[XMSControl]	;get size of biggest block free.
	mov	edx,eax
	test	eax,eax
	jz	@@YesXMS		;no memory available.
	mov	cs:d[@@XMSSize],edx
	mov	ah,89h
	call	d[XMSControl]	;claim biggest block to force XMS
	cmp	ax,1			;to stake a claim on int 15h.
	jnz	@@YesXMS
	mov	cs:b[@@NumXMSHandles],1
	push	dx
	mov	ah,8eh
	call	d[XMSControl]	; get handle information
	cmp	ax,1
	jnz	@@NoHandles3
	cmp	cx,4
	jc	@@NoHandles3
	sub	cx,2
	mov	cs:b[@@NumXMSHandles],32
	cmp	cx,32
	jnc	@@NoHandles3
	mov	cs:b[@@NumXMSHandles],cl	; cx known 8-bit value

@@NoHandles3:
	pop	dx
	mov	ah,0ah
	call	d[XMSControl]	;now free it.

	movzx	eax, BYTE PTR cs:[@@NumXMSHandles]
	mov	edx,eax
	shl	eax,16
	sub	eax,edx		; eax == handles (up to 32) * 65535
	cmp	eax,cs:d[@@XMSSize]
	jae	@@ComputeSize
	mov	cs:d[@@XMSSize],eax	; throttle maximum size

@@ComputeSize:
	push	eax
	xor	edx,edx
	movzx	ebx,BYTE PTR cs:[@@NumXMSHandles]
	div	ebx
	pop	ebx
	cmp	ax,4		; eax known 16-bit value
	jnc	@@SizeOK3
	mov	ax,bx		; ebx == maximum size, known 16-bit value here

@@SizeOK3:
	mov	XMSBlockSize,ax
	jmp	@@YesXMS

xms2:
	mov	ah,8
	call	d[XMSControl]	;get size of biggest block free.
	mov	dx,ax
	or	ax,ax
	jz	@@YesXMS		;no memory available.
	mov	cs:w[@@XMSSize],dx
	mov	ah,9
	call	d[XMSControl]	;claim biggest block to force XMS
	cmp	ax,1		;to stake a claim on int 15h.
	jnz	@@YesXMS
	mov	cs:b[@@NumXMSHandles],1
	push	dx
	mov	ah,0eh
	call	d[XMSControl]	;now free it.
	cmp	ax,1
	jnz	@@NoHandles
	cmp	bl,4
	jc	@@NoHandles
	sub	bl,2
	mov	cs:b[@@NumXMSHandles],32
	cmp	bl,32
	jnc	@@NoHandles
	mov	cs:b[@@NumXMSHandles],bl
@@NoHandles:	pop	dx
	mov	ah,0ah
	call	d[XMSControl]	;now free it.
	mov	ax,cs:w[@@XMSSize]
	push	ax
	xor	dx,dx
	xor	bh,bh
	mov	bl,cs:b[@@NumXMSHandles]
	div	bx
	pop	bx
	cmp	ax,4
	jnc	@@SizeOK
	mov	ax,bx
@@SizeOK:	mov	XMSBlockSize,ax
	jmp	@@YesXMS
;
;Install raw A20 handler.
;
@@NoXMS:	call	InstallA20
;
;Get A20 state.
;
@@YesXMS:	push	ds
	les	di,HighMemory	;   with the four at FFFF:0090
	lds	si,LowMemory		; Compare the four words at 0000:0080
	mov	cx,4
	cld
	repe  cmpsw
	pop	ds
	xor	ax,ax
	jcxz	@@A20OFF		 ; Are the two areas the same?
	inc	ax		    ; No, return A20 Enabled
@@A20OFF:	mov	A20Flag,al
;
;Change DOS allocation stratergy to highest so we'll get UMB's if available.
;
	mov	ax,5800h
	int	21h
	mov	cs:w[@@OldStrat],ax
	mov	ax,5802h
	int	21h
	mov	cs:w[@@OldStrat+2],ax
	mov	bx,1
	mov	ax,5803h
	int	21h
	mov	bx,81h
	mov	ax,5801h
	int	21h
;
;Grab memory for page dir, page alias & first page table entry.
;
	mov	bx,(4096*3)/16	;smallest allocation possible.
	mov	ah,48h
	int	21h
	jc	@@OldWay
	push	ax
	movzx	eax,ax
	shl	eax,4		;linear address.
	mov	ebx,eax
	add	eax,4095
	and	eax,0ffffffffh-4095	;round up to next page.
	sub	eax,ebx
	shr	eax,4
	mov	bx,ax
	mov	cx,ax
	add	bx,(4096*3)/16
	mov	ah,4ah
	pop	es
	Pushm	bx,cx,es
	int	21h		;re-size the block.
	Popm	bx,cx,es
	jnc	@@NewWay
	mov	ah,49h
	int	21h		;release this block.
	;
@@OldWay:	mov	cs:IErrorNumber,5
	mov	bx,(4096*4)/16	;need space for 3 page tables on
	mov	ah,48h		;4k boundary.
	int	21h
	jc	InitError
	mov	dx,ax
	movzx	eax,ax		;get segment address.
	shl	eax,4		;make linear.
	mov	ebx,eax
	add	eax,4095
	and	eax,0FFFFFFFFh-4095	;round up to nearest page.
	mov	ecx,eax
	sub	ecx,ebx
	shr	ecx,4
	shr	eax,4		;Get segment value again.
	jmp	@@GotSeg
	;
@@NewWay:	mov	ax,es
	mov	dx,ax
	add	ax,cx		;move to real start.
	;
@@GotSeg:
	Pushm	cx,dx
	mov	es,ax

; MED 09/19/96
;	mov	PageDirReal,ax	;setup page directory address.
;	add	ax,4096/16
;	mov	Page1stReal,ax	;setup 1st page table address.
	mov	Page1stReal,ax	;setup 1st page table address.
	add	ax,4096/16
	mov	PageDirReal,ax	;setup page directory address.

	add	ax,4096/16
	mov	PageAliasReal,ax	;setup alias table address.
	xor	di,di
	mov	cx,4096*3
	xor	al,al
	cld
	rep	stosb		;clear it.
	movzx	eax,PageDirReal
	shl	eax,4
	mov	PageDirLinear,eax
	mov	VCPI_CR3,eax
	movzx	eax,PageAliasReal
	shl	eax,4
	mov	PageAliasLinear,eax
	movzx	eax,Page1stReal
	shl	eax,4
	mov	Page1stLinear,eax
	Popm	cx,dx
;
;See if enough wasted space to squeeze TSS into.
;
	cmp	cx,(((size TSSFields)+2+16)/16)
	jc	@@TSSOld
	mov	ax,dx   		;get segment.
	add	dx,(((size TSSFields)+2+16)/16)	;move segment base.
	sub	cx,(((size TSSFields)+2+16)/16)	;update space left size.
	jmp	@@TSSGot
;
;Allocate memory for Kernal TSS.
;
@@TSSOld:	mov	cs:IErrorNumber,5
	mov	bx,(((size TSSFields)+2+16)/16)	;(4096/2)+2+16)/16
	mov	ah,48h
	int	21h
	jc	InitError
@@TSSGot:	mov	KernalTSSReal,ax
;
;See if enough wasted space to squeeze GDT into.
;
	cmp	cx,((8*GDT_Entries)/16)+1
	jc	@@GDTOld
	mov	ax,dx   		;get segment.
	add	dx,((8*GDT_Entries)/16)+1	;move segment base.
	sub	cx,((8*GDT_Entries)/16)+1	;update space left size.
	jmp	@@GDTGot
;
;Allocate some memory for the GDT.
;
@@GDTOld:	mov	cs:IErrorNumber,5
	mov	bx,((8*GDT_Entries)/16)+1
	mov	ah,48h
	int	21h
	jc	InitError
@@GDTGot:	mov	GDTReal,ax
	mov	es,ax
	movzx	eax,ax
	shl	eax,4
	mov	GDTLinear,eax
	xor	di,di
	mov	cx,(8*GDT_Entries)
	xor	al,al
	cld
	rep	stosb
;
;Allocate some memory for the stack.
;
	mov	cs:IErrorNumber,5
	mov	ebx,RawStackPos
	shr	ebx,4
	mov	ah,48h
	int	21h
	jc	InitError
	mov	RawStackReal,ax
	mov	es,ax
	mov	cx,w[RawStackPos]
	xor	di,di
	xor	al,al
	cld
	rep	stosb

; MED 09/19/96
; Set address for VMM page to disk buffer.
	mov	cs:IErrorNumber,5
	mov	bx,4096/16
	mov	ah,48h
	int	21h
	jc	InitError
	mov	PageBufferReal,ax
	movzx	eax,ax
	shl	eax,4
	mov	PageBufferLinear,eax

;
;Restore DOS memory allocation stratergy.
;
	mov	bx,cs:w[@@OldStrat+2]
	xor	bh,bh
	mov	ax,5803h
	int	21h
	mov	bx,cs:w[@@OldStrat]
	xor	bh,bh
	mov	ax,5801h
	int	21h
;
;Need to initialise 1st page table to map <1meg+64k 1:1.
;
	mov	es,Page1stReal
	xor	di,di
	mov	cx,256+16		;1st 1 meg + 64k.
	mov	esi,111b		;user+write+present
@@0:	mov	es:[di],esi
	add	di,4		;next page table entry.
	add	esi,4096		;next physical page address.
	dec	cx
	jnz	@@0

; MED 09/19/96
COMMENT !
;
;Set address for VMM page to disk buffer.
;
	mov	ax,PageDirReal
	mov	PageBufferReal,ax
	movzx	eax,ax
	shl	eax,4
	mov	PageBufferLinear,eax
END COMMENT !

;
;Set address for DOS INT 21h PM to Real transfer buffer.
;

; MED 09/19/96
;	mov	bx,Page1stReal
	mov	bx,PageDirReal

	push	ds
	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
	mov	TransferReal,bx
	assume ds:_cwRaw
	pop	ds
;
;Allocate the GDT entries.
;
	mov	es,GDTReal
;
;Fill in the null entry just for the hell of it.
;
	xor	esi,esi		;Null entry at 0.
	xor	ecx,ecx
	xor	ax,ax
	xor	di,di
	call	MakeDesc
;
;Fill in the VCPI entries so we don't try to use them later.
;
	xor	esi,esi
	xor	ecx,ecx
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,VCPI_0
	call	MakeDesc
	xor	esi,esi
	xor	ecx,ecx
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,VCPI_1
	call	MakeDesc
	xor	esi,esi
	xor	ecx,ecx
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,VCPI_2
	call	MakeDesc
;
;Allocate 40h descriptor.
;
	mov	esi,400h
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,Kernal40h
	call	MakeDesc
;
;Allocate kernal task descriptors, TSS itself first.
;
	movzx	esi,KernalTSSReal
	shl	esi,4
	mov	ecx,size TSSFields+2
	xor	al,al
	mov	ah,DescPresent+DescPL3+Desc386Tss
	mov	di,KernalTS
	call	MakeDesc
;
;TSS PL0 stack.
;
	movzx	esi,KernalTSSReal
	shl	esi,4
	add	esi,TSSFields.tPL1Stack
	mov	ecx,65535
	mov	al,b[RawSystemFlags]
	xor	al,1
	shl	al,6
	mov	ah,DescPresent+DescPL0+DescMemory+DescRWData
	mov	di,KernalPL0
	call	MakeDesc
;
;Mode switch PL0 stack.
;
	movzx	esi,KernalTSSReal
	shl	esi,4
	add	esi,TSSFields.tPL1Stack
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL0+DescMemory+DescRWData
	mov	di,KernalSwitchPL0
	call	MakeDesc
;
;LDT
;
	xor	esi,esi
	xor	ecx,ecx
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescLDT
	mov	di,KernalLDT
	call	MakeDesc
;
;Kernal (RAW) code seg.
;
	xor	esi,esi
	mov	si,_cwRaw
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	mov	di,KernalCS
	call	MakeDesc
;
;Kernal (RAW) code seg at PL0
;
	xor	esi,esi
	mov	si,_cwRaw
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL0+DescMemory+DescERCode
	mov	di,KernalCS0
	call	MakeDesc
;
;Kernal (RAW) data seg.
;
	xor	esi,esi
	mov	si,_cwRaw
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,KernalDS
	call	MakeDesc
;
;Kernal (RAW) stack seg.
;
	movzx	esi,RawStackReal
	shl	esi,4
	mov	ecx,65535
	mov	al,b[RawSystemFlags]
	xor	al,1
	shl	al,6
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,KernalSS
	call	MakeDesc
;
;Kernal PL3 to PL0 call gate.
;
	xor	ecx,ecx
	mov	esi,KernalCS0
	xor	al,al
	mov	ah,DescPresent+DescPL3+Desc386Call
	mov	di,KernalPL3toPL0
	call	MakeDesc
;
;DPMI emulator code seg.
;
	xor	esi,esi
	mov	si,_cwDPMIEMU
	shl	esi,4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	mov	di,DpmiEmuCS
	call	MakeDesc
;
;DPMI emulator code seg at PL0
;
	xor	esi,esi
	mov	si,_cwDPMIEMU
	shl	esi,4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL0+DescMemory+DescERCode
	mov	di,DpmiEmuCS0
	call	MakeDesc
;
;DPMI emulator data seg.
;
	xor	esi,esi
	mov	si,_cwDPMIEMU
	shl	esi,4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,DpmiEmuDS
	call	MakeDesc
;
;Init PL3 to PL0 call gate.
;
	xor	ecx,ecx
	mov	esi,InitCS0
	xor	al,al
	mov	ah,DescPresent+DescPL3+Desc386Call
	mov	di,InitPL3toPL0
	call	MakeDesc
;
;DPMI emulator PL3 to PL0 call gate.
;
	xor	ecx,ecx
	mov	esi,DpmiEmuCS0
	xor	al,al
	mov	ah,DescPresent+DescPL3+Desc386Call
	mov	di,DpmiEmuPL3toPL0
	call	MakeDesc
;
;Zero to 4G segment.
;
	xor	esi,esi
	or	ecx,-1
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,KernalZero
	call	MakeDesc
;
;Main PSP segment.
;
	push	ds
	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
	movzx	esi,RealPSPSegment
	assume ds:_cwRaw
	pop	ds
	shl	esi,4
	mov	ecx,256
	mov	al,0
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,MainPSP
	call	MakeDesc
;
;Main environment var.
;
	push	ds
	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
	movzx	esi,RealENVSegment
	assume ds:_cwRaw
	pop	ds
	shl	esi,4
	mov	ecx,0FFFFh
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,MainEnv
	call	MakeDesc
;
;Main code seg.
;
	xor	esi,esi
	mov	si,_cwMain
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	mov	di,MainCS
	call	MakeDesc
;
;Main data seg.
;
	xor	esi,esi
	mov	si,_cwMain
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,MainDS
	call	MakeDesc
;
;Main stack.
;
	xor	esi,esi
	mov	si,_cwStack
	shl	esi,4
	mov	ecx,65535
	mov	al,b[RawSystemFlags]
	xor	al,1
	shl	al,6
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,MainSS
	call	MakeDesc
;
;Init code seg.
;
	xor	esi,esi
	mov	si,_cwInit
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	mov	di,InitCS
	call	MakeDesc
;
;Init code seg at PL0
;
	xor	esi,esi
	mov	si,_cwInit
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL0+DescMemory+DescERCode
	mov	di,InitCS0
	call	MakeDesc
;
;Init data seg.
;
	xor	esi,esi
	mov	si,_cwInit
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,InitDS
	call	MakeDesc
;
;Init GDT data alias.
;
	mov	esi,GDTLinear
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,GDTData
	call	MakeDesc
;
;Setup TSS registers to run in protected mode. Setup GDT & IDT values.
;
	mov	es,KernalTSSReal
	xor	di,di
	mov	cx,size TSSFields	;+(4096/8)
	xor	al,al
	cld
	rep	stosb		;clear TSS & IO map.
	or	ax,-1
	stosw			;mark end of IO map.
	mov	es:TSSFields.SS0,KernalPL0
	mov	es:TSSFields.ESP0,tPL0StackSize-4
	mov	es:TSSFields.tLDT,KernalLDT
	mov	es:TSSFields.IOMap,size TSSFields	;set displacement to IO table.
;
;Setup GDT load value.
;
	movzx	eax,GDTReal
	shl	eax,4
	mov	d[GDTVal+2],eax
	mov	w[GDTVal],-1
;
;Setup Windows enhanced mode denial patch.
;
	mov	ax,352fh		;get existing vector.
	int	21h
	mov	w[OldInt2F],bx
	mov	w[OldInt2F+2],es
	mov	ax,252fh
	mov	dx,offset Int2FPatch
	int	21h
;
;Now patch RAW specific calls.
;
	push	ds
	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
	cmp	ProtectedType,1	;VCPI?
	assume ds:_cwRaw
	pop	ds
	jz	@@VCPI
;
;Use RAW method to switch to protected mode.
;
@@RAW:	.386p
;
;Need to initialise 1st entry of page dir & alias.
;
	movzx	eax,Page1stReal
	shl	eax,4
	or	eax,111b		;user+write+present
	mov	es,PageDirReal
	xor	di,di
	mov	es:[di],eax
	mov	es,PageAliasReal
	mov	es:[di],eax
	mov	es,KernalTSSReal
	mov	es:[di].TSSFields.tCR3,eax	;set CR3 in TSS as well.
	;
	;map alias into page dir as well.
	;
	movzx	eax,PageAliasReal	;get para address.
	shl	eax,4		;make linear.
	or	eax,111b		;user+write+present.
	mov	es,PageDirReal
	mov	di,1023*4
	mov	es:[di],eax		;setup in last page dir entry.
	;
;	pushfd
;	pop	eax
;	mov	EFlagsSav,eax
	mov	eax,cr3
	mov	CR3Sav,eax
	mov	eax,cr0
	mov	CR0Sav,eax
	sidt	IDTSav		;save old IDT value for switch back.
	sgdt	GDTSav
	pop	RetAdd
	cli			;Don't want interupts interfering.
	lgdt	GDTVal		;Setup GDT &
	lidt	f[IDTVal]		;IDT.
	mov	eax,VCPI_CR3
	mov	cr3,eax		;set page dir address.
	mov	eax,cr0		;Get machine status &
	or	eax,080000001h	;set PM+PG bits.
	mov	cr0,eax		;/

; MED 10/15/96
	mov	CR0ProtSav,eax	; save protected mode status of CR0

	db 0eah		;Absolute 16-bit jump, to clear
	dw @@RAW0,InitCS0		;instruction pre-fetch & load CS.
@@RAW0:	mov	ax,KernalLDT		;Point to empty LDT descriptor.
	lldt	ax		;and set LDT.
	mov	cx,KernalTS		;Get value for task register.
	ltr	cx		;and set it.
	;
	mov	Protected2Real,offset RAWProt2Real
	mov	Real2Protected,offset RAWReal2Prot
	jmp	@@InProt
;
;Use VCPI method to switch to protected mode.
;
@@VCPI:	mov	cs:IErrorNumber,6
	cli
	push	ds
	xor	di,di		;Page table offset.
	mov	es,Page1stReal	;Page table segment
	mov	si,VCPI_0		;VCPI GDT entries offset.
	and	si,not 3
	mov	ds,GDTReal		;GDT segment.
	mov	ax,0de01h		;Let VCPI server prepare.
	int	67h
	pop	ds
	or	ah,ah
	jnz	InitError
	mov	d[VCPI_Entry],ebx	;Store entry point.

; MED 11/05/96
	mov	FirstUninitPage,di	; VCPI server advanced to first uninitialized page
							; table entry in client's page

	;
	;Now update PHYSICAL addresses of dir & 1st page tables.
	;
	mov	es,Page1stReal
	movzx	edi,Page1stReal	;get linear address.
	shl	edi,4		;/
	shr	edi,12		;page number.
	shl	edi,2		;*4 bytes per entry.
	mov	eax,es:[di]		;get physical address.
	and	eax,not 4095		;clear status bits.
	or	eax,111b		;set our bits.
	mov	es,PageDirReal
	xor	di,di
	mov	es:[di],eax
	mov	es,PageAliasReal
	mov	es:[di],eax
	;
	mov	es,Page1stReal
	movzx	edi,PageDirReal	;get linear address.
	shl	edi,4		;/
	shr	edi,12		;page number.
	shl	edi,2		;*4 bytes per entry.
	mov	eax,es:[di]		;get physical address.
	and	eax,0FFFFFFFFh-4095	;clear status bits.
	mov	VCPI_CR3,eax		;set VCPI CR3 value as well.
	mov	es,KernalTSSReal
	xor	di,di
	mov	es:[di].TSSFields.tCR3,eax	;set CR3 in TSS as well.
	;
	mov	es,Page1stReal
	movzx	edi,PageAliasReal	;get linear address.
	shl	edi,4		;/
	shr	edi,12		;page number.
	shl	edi,2		;*4 bytes per entry.
	mov	eax,es:[di]		;get physical address.
	and	eax,0FFFFFFFFh-4095	;clear status bits.
	or	eax,111b		;user+write+present.
	mov	es,PageDirReal
	mov	di,1023*4
	mov	es:[di],eax		;setup in last page dir entry.
	;
	mov	VCPI_LDT,KernalLDT
	mov	VCPI_EIP,offset @@InProt
	mov	VCPI_TR,KernalTS	;Get value for task register.
	mov	VCPI_CS,InitCS0
	xor	eax,eax
	mov	ax,seg _cwRaw
	shl	eax,4
	add	eax,offset GDTVal
	mov	VCPI_pGDT,eax
	xor	eax,eax
	mov	ax,seg _cwRaw
	shl	eax,4
	add	eax,offset IDTVal
	mov	VCPI_pIDT,eax
	cli
	mov	ax,0de0ch
	mov	si,seg _cwRaw
	movzx	esi,si
	shl	esi,4
	add	esi,offset VCPI_CR3
	int	67h
	;
	mov	ax,_cwStack
	mov	ss,ax
	mov	esp,offset _cwStackEnd-4
	mov	ax,_cwRaw
	mov	ds,ax
	jmp	InitError		;Shouldn't come through here.
;
;We're in protected mode at last, now we just have to move the DPMI emulation
;stuff into extended memory and build a useful IDT.
;
@@InProt:
	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	or	IProtectedMode,-1
	mov	ax,KernalDS		;Get data descriptor.
	mov	ds,ax		;/
	assume ds:_cwRaw
	mov	es,ax		;/
	mov	fs,ax		;/
	mov	gs,ax		;/
	mov	ax,KernalPL0
	mov	ss,ax		;/
	mov	esp,offset tPL0StackSize-4
	;
	pushfd
	pop	eax
	and	ax,1011111111111111b	;clear NT.
	push	eax
	popfd
	cld
	clts
;
;Switch to PL3 code seg for the hell of it.
;
	.386
	mov	edx,offset _cwStackEnd-4
	mov	ecx,MainSS
	push	ecx		;SS
	push	edx		;ESP
	pushfd		;EFlags
	pop	eax
	and	ax,1000111111111111b	;clear NT & IOPL.
	or	ax,0011000000000000b	;force IOPL.
	push	eax
	popfd
	push	eax
	xor	eax,eax
	mov	ax,InitCS
	push	eax		;CS
	mov	eax,offset @@pl3
	push	eax		;EIP
;	db 66h	; the iretd will already have a 66h prefix
	iretd
	;
@@pl3:	push	es
	mov	ax,KernalZero
	mov	es,ax
	mov	esi,GDTLinear
	add	esi,KernalTS-3
	mov	es:b[esi+5],DescPresent+DescPL3+Desc386Tss
	pop	es
;
;Setup initial segment variables.
;
	push	ds
	mov	ax,MainDS
	mov	ds,ax
	assume ds:_cwMain
	or	w[SystemFlags],32768	;Flags us in protected mode.
	mov	RealSegment,KernalZero
	mov	PSPSegment,MainPSP
	mov	ENVSegment,MainEnv
	mov	CodeSegment,MainCS
	mov	DataSegment,MainDS
	mov	StackSegment,MainSS
	assume ds:_cwRaw
	pop	ds
;
;Make sure A20 is enabled.
;
	push	ds
	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	mov	IErrorNumber,7
	assume ds:_cwRaw
	pop	ds
	mov	ax,1
	push	cs
	push	offset @@1
	push	KernalCS
	push	offset A20Handler
	retf
@@1:	jnz	InitError
;
;Now get extended memory sorted out, move the page tables into extended memory
;for a start.
;
	push	ds
	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	mov	IErrorNumber,5
	assume ds:_cwRaw
	pop	ds
	mov	ax,KernalZero
	mov	es,ax
;
;Allocate 2nd page table so we can map extended memory.
;
	call	d[fPhysicalGetPage]
	jc	InitError
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.

	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,1
	mov	esi,PageDirLinear
	mov	es:d[esi+eax*4],edx	;store this tables address.
	mov	esi,PageAliasLinear	;get alias table address.
	mov	es:d[esi+eax*4],edx	;setup in alias table as well.
	call	d[fCR3Flush]
	mov	edi,1024*4096*1023	;base of page alias's.
	mov	eax,1024*4		;get the entry number again.
	add	edi,eax
	mov	ecx,4096/4
	xor	eax,eax
	cld
	db 67h
	rep	stosd
	call	d[fCR3Flush]
	mov	LinearEntry,1024
;
;Setup DET page alias.
;
	call	d[fPhysicalGetPage]	;get page for new page 1st DET.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	xor	eax,eax
	cld
	db 67h
	rep	stosd		;clear it.
	mov	eax,LinearEntry
	shl	eax,12		;get linear address.
	mov	PageDETLinear,eax
	mov	eax,1022
	mov	esi,PageDirLinear
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	mov	es:[esi+eax*4],edx	;put new page into the map.
	mov	esi,PageAliasLinear
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	inc	LinearEntry
;
;Setup DET page 1st.
;
	call	d[fPhysicalGetPage]	;get page for new page 1st.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	eax,LinearEntry	;get the entry number again.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	mov	eax,MEM_FILL
	cld
	db 67h
	rep	stosd		;copy old to new.
	mov	esi,PageDETLinear
	mov	eax,0
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	inc	LinearEntry
;
;Allocate 2nd page DET
;
	call	d[fPhysicalGetPage]	;get page for new page 1st.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	eax,LinearEntry	;get the entry number again.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	mov	eax,MEM_FILL
	cld
	db 67h
	rep	stosd		;copy old to new.
	mov	esi,PageDETLinear
	mov	eax,1
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	inc	LinearEntry
;
;Move page alias into extended memory.
;
	call	d[fPhysicalGetPage]	;get page for new page 1st.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	push	ds
	mov	esi,PageAliasLinear
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	push	es
	pop	ds
	cld
	db 67h
	rep	movsd		;copy old to new.
	pop	ds
	mov	eax,PageAliasLinear
	mov	PageAliasLinear+4,eax
	mov	eax,LinearEntry
	shl	eax,12		;get linear address.
	mov	PageAliasLinear,eax
	mov	esi,PageDirLinear
	mov	eax,1023
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	mov	ecx,es:[esi+eax*4]	;get origional value.
	mov	PageAliasLinear+8,ecx
	mov	es:[esi+eax*4],edx	;put new page into the map.
	mov	esi,PageAliasLinear
	mov	eax,1023
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	inc	LinearEntry

; MED 09/19/96
COMMENT !
;
;Move page 1st into extended memory.
;
	call	d[fPhysicalGetPage]	;get page for new page 1st.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	;
	;Map it into general linear address space.
	;
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	;
	;Copy table to new memory.
	;
	push	ds
	mov	esi,Page1stLinear
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	push	es
	pop	ds
	cld
	db 67h
	rep	movsd		;copy old to new.
	pop	ds
	;
	;Make variables point to new memory.
	;
	mov	eax,Page1stLinear
	mov	Page1stLinear+4,eax	;store old address.
	mov	eax,LinearEntry
	shl	eax,12		;get linear address.
	mov	Page1stLinear,eax	;set new linear address.
	;
	;Set new address in page dir.
	;
	mov	edx,LinearEntry+8	;get physical address again.
	or	edx,111b
	;
	mov	esi,PageDirLinear
	mov	eax,0
	mov	ecx,es:[esi+eax*4]	;get origional value.
	mov	Page1stLinear+8,ecx
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	;
	;Set new address in page dir alias.
	;
	mov	esi,PageAliasLinear
	mov	eax,0
	mov	es:[esi+eax*4],edx	;put new page into the map.
	call	d[fCR3Flush]
	inc	LinearEntry
END COMMENT !

;
;Move page dir into extended memory.
;
	call	d[fPhysicalGetPage]	;get page for new page DIR.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	;
	;Map it into normal linear address space.
	;
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	;
	;Copy table to new memory.
	;
	push	ds
	mov	esi,PageDirLinear
	mov	edi,LinearEntry
	shl	edi,12
	mov	ecx,4096/4
	push	es
	pop	ds
	cld
	db 67h
	rep	movsd		;copy old to new.
	pop	ds
	;
	;Make variables point to new memory.
	;
	mov	eax,PageDirLinear
	mov	PageDirLinear+4,eax	;store old value.
	mov	eax,LinearEntry
	shl	eax,12		;get linear address.
	mov	PageDirLinear,eax	;set new value.
	mov	eax,VCPI_CR3
	mov	PageDirLinear+8,eax	;store old physical address.
	mov	eax,LinearEntry+8
	mov	VCPI_CR3,eax		;set new physical address.
	movzx	edi,KernalTSSReal
	shl	edi,4
	mov	es:[edi].TSSFields.tCR3,eax	;set CR3 in TSS as well.
	call	d[fCR3Flush]
	inc	LinearEntry
;
;Setup IDT.
;
	call	d[fPhysicalGetPage]	;get page for new page DIR.
	jc	InitError
	mov	LinearEntry+8,edx	;store physical address.
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	mov	eax,LinearEntry
	shl	eax,12
	mov	d[IDTVal+2],eax
	mov	w[IDTVal],0+(256*8)-1
	;
	mov	bp,256		;number of vectors.
	mov	ecx,offset InterruptHandler ;code address.
	mov	esi,DpmiEmuCS0	;gate to use.
	mov	al,0
	mov	ah,DescPresent+DescPL3+Desc386Int
	mov	edi,d[IDTVal+2]
@@3:	call	MakeDesc2
	add	edi,8		;next table address.
	add	ecx,8		;/
	dec	bp
	jnz	@@3
	inc	LinearEntry
	;
	;Re-load IDT value.
	;
	push	es
	mov	edi,offset MemIntBuffer
	push	ds
	pop	es
	mov	RealRegsStruc.Real_CS[edi],_cwInit
	mov	RealRegsStruc.Real_IP[edi],offset IDTFlush
	mov	RealRegsStruc.Real_SS[edi],0
	mov	RealRegsStruc.Real_SP[edi],0
	call	d[fRawSimulateFCALL]
	pop	es
;
;Get extended memory for DPMI emulator.
;
	push	ds
	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	mov	IErrorNumber,5
	assume ds:_cwRaw
	pop	ds
	;
	mov	ebp,offset cwDPMIEMUEnd-cwDPMIEMUStart
	add	ebp,4095
	shr	ebp,12		;Get number of pages needed.
	mov	eax,LinearEntry
	shl	eax,12
	mov	LinearEntry+4,eax	;Store start address.
@@2:	call	d[fPhysicalGetPage]	;try to allocate a page.
	jc	InitError
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	inc	LinearEntry		;update pointer.
	dec	ebp
	jnz	@@2
;
;Copy DPMI emulator code into extended memory we just allocated.
;
	mov	edi,LinearEntry+4	;Get the destination.
	mov	si,_cwDPMIEMU
	movzx	esi,si
	shl	esi,4		;Point to the source.
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	shr	ecx,2
	push	ds
	push	es
	pop	ds
	cld
	db 67h
	rep	movsd		;Copy it up their.
	pop	ds
;
;Setup DPMI emulator selectors.
;
	push	es
	mov	ax,GDTData
	mov	es,ax
	mov	esi,LinearEntry+4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	mov	di,DpmiEmuCS
	call	MakeDesc
	mov	esi,LinearEntry+4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL0+DescMemory+DescERCode
	mov	di,DpmiEmuCS0
	call	MakeDesc
	mov	esi,LinearEntry+4
	mov	ecx,offset cwDPMIEMUEnd-cwDPMIEMUStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,DpmiEmuDS
	call	MakeDesc
	pop	es
;
;Should be OK to enable interrupts at last.
;
	mov	ah,1
	int	16h
	sti
;
;Initialise hardware interrupt call-back's.
;
	call	InitHardwareInts
;
;Allocate memory for new GDT/LDT
;
	push	ds
	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	mov	IErrorNumber,5
	assume ds:_cwRaw
	pop	ds
	;
	mov	ebp,(8192*8)+8192
	shr	ebp,12		;Get number of pages needed.
	mov	eax,LinearEntry
	shl	eax,12
	mov	LinearEntry+4,eax	;Store start address.
@@6:	call	d[fPhysicalGetPage]	;try to allocate a page.
	jc	InitError
	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	inc	LinearEntry		;update pointer.
	dec	ebp
	jnz	@@6
	;
	mov	esi,LinearEntry+4
	mov	MDTLinear,esi	;store for allocation code.
	mov	MDTLinear+4,esi
	add	MDTLinear+4,8192*8	;setup bit string address.
	;
	;Clear the memory to zero's.
	;
	mov	edi,MDTLinear
	mov	ecx,(8192*8)+8192
	cld
	xor	al,al
	db 67h
	rep	stosb
	mov	edi,MDTLinear+4
	or	es:d[edi],-1		;Force VCPI values to not used.
	;
	;See which table we want to use.
	;
	test	RawSystemFlags,128	;GDT or LDT?
	jnz	@@LDT
	;
	;Setup a new GDT.
	;
	mov	esi,MDTLinear
	mov	GDTLinear+4,esi
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	mov	di,GDTData
	push	es
	mov	es,di
	call	MakeDesc
	pop	es
	;
	;Copy current GDT to new GDT.
	;
	push	ds
	mov	esi,GDTLinear
	mov	edi,GDTLinear+4
	mov	ax,KernalZero
	mov	ds,ax
	mov	ecx,GDT_Entries*8
	cld
	db 67h
	rep	movsb
	pop	ds
	;
	;Set new GDT values.
	;
	pushf
	cli
	mov	eax,GDTLinear
	mov	GDTLinear+8,eax
	mov	eax,GDTLinear+4
	mov	GDTLinear,eax
	mov	d[GDTVal+2],eax
	mov	d[VCPI_GDT+2],eax
	popf
	;
	;Now mark all used descriptors in allocation control string.
	;
	mov	edi,GDTLinear
	mov	esi,MDTLinear+4
	mov	cx,GDT_Entries
@@4:	test	es:b[edi+5],DescPresent	;this descriptor in use?
	jz	@@5
	or	es:b[esi],-1
@@5:	add	edi,8		;next descriptor.
	inc	esi		;update descriptor number.
	dec	cx
	jnz	@@4
	;
	;Now setup extra GDT descriptors.
	;
	push	es
	mov	ax,GDTData
	mov	es,ax
	;
	mov	di,KernalB000
	mov	esi,0b0000h
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	;
	mov	di,KernalB800
	mov	esi,0b8000h
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	;
	mov	di,KernalA000
	mov	esi,0a0000h
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	pop	es
	;
@@LDT:	;Setup new LDT.
	;
	mov	esi,MDTLinear
	mov	LDTLinear,esi
	;
	;Map LDT into GDT.
	;
	push	es
	mov	ax,GDTData
	mov	es,ax
	mov	di,KernalLDT
	mov	esi,LDTLinear
	mov	ecx,8192*8
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescLDT
	call	MakeDesc
	pop	es
	;
	mov	ah,1
	int	16h		;force GDT/LDT reload with mode switch.
;
;Initialise application memory pool.
;
	mov	eax,LinearEntry
	shl	eax,12
	mov	LinearBase,eax
	add	eax,4096
	mov	LinearLimit,eax
	;
	call	d[fPhysicalGetPage]	;try to allocate a page.
	jc	InitError

	and	ecx,1		;put user bits in useful place.
	shl	ecx,10
	and	edx,not 4095		;lose user bits.
	or	edx,111b		;present+user+write.
	or	edx,ecx		;set use flags.
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1023	;base of page alias's.
	mov	es:d[esi+eax*4],edx	;set physical address.
	call	d[fCR3Flush]
	mov	eax,LinearEntry	;get the entry number again.
	mov	esi,1024*4096*1022
	mov	es:d[esi+eax*4],0	;clear this pages details.
;
;Initialise virtual memory manager stuff.
;
	call	d[fPhysicalGetPages]	;find free pages.
	mov	eax,edx
	shl	eax,12
	add	eax,LinearLimit
	sub	eax,LinearBase
	mov	ebp,eax		;save this for comparisons.
	mov	di,InitDS
	mov	fs,di
	mov	di,offset VMMDrivPath1	;point to start of paths.
	assume fs:_cwInit
	cmp	fs:NoVMSwitch,0
	assume fs:nothing
	jnz	@@v9
	;
	;Work through list of posibles till we find a useful entry.
	;
@@v0:
	cmp	fs:b[di],0
	jz	@@v7
	cmp	fs:b[di],-1
	jz	@@v9
	;
	;Check drive is valid and has enough free space.
	;

COMMENT !
; MED 10/11/96
; if no drivespec, then use default drive
	cmp	BYTE PTR fs:[di+1],':'
	je	isdrive

	movzx	edx,di	; scan to end of current pathspec
defloop:
	inc	dx			; edx known 16-bit value so 16-bit increment is valid
	cmp	BYTE PTR fs:[edx],0
	jne	defloop

; at end of current pathspec, now shift forward two bytes to allow for drive
shiftloop:
	mov	al,fs:[edx]
	mov	fs:[edx+2],al
	dec	dx
	cmp	di,dx		; edx known 16-bit value so 16-bit compare is valid
	jae	shiftloop

	push	edi
	mov	edi,offset PageInt
	push	ds
	pop	es
	mov	[edi].RealRegsStruc.Real_EAX,1900h
	mov	RealRegsStruc.Real_SS[edi],0
	mov	RealRegsStruc.Real_SP[edi],0
	mov	bl,21h
	call	d[fRawSimulateINT]
	mov	al,BYTE PTR [edi].RealRegsStruc.Real_EAX
	pop	edi
	add	al,'A'		; convert to drive
	mov	fs:[di],al
	mov	BYTE PTR fs:[di+1],':'	; add colon to drivespec
END COMMENT !

isdrive:
	mov	dl,fs:[di]		;get swap file drive.
	cmp	dl,61h		; 'a'
	jb	@@v1
	cmp	dl,7Ah		; 'z'
	ja	@@v1
	and	dl,5Fh		;convert to upper case.
@@v1:	sub	dl,'A'		;make it real.
	inc	dl		;adjust for current type selection.

drivefree:
	mov	ah,36h		;get free space.
	push	ebp
	int	21h		;/
	pop	ebp
	cmp	ax,-1		;invalid drive?
	jz	@@v7
	mul	cx		;Get bytes per cluster.
	mul	bx		;Get bytes available.
	shl	edx,16
	mov	dx,ax
	cmp	edx,ebp		;Enough free space.
	jc	@@v7
	;
	;See if we can create a temp file.
	;

; MED 02/25/96, use name specified in CAUSEWAY e-var
	test	DesiredVMMName,-1
	je	med5a			; no VMM name request
	push	di
	mov	si,di
	push	si			; save -> filespec start

; find end of pathspec
mednameloop:
	cmp	BYTE PTR fs:[si],0
	je	medndone
	inc	si
	jmp	mednameloop

; append desired name on filespec
medndone:

; 05/15/98
; check for backslash already existing
	cmp	BYTE PTR fs:[si-1],'\'
	je	medbs

	mov	BYTE PTR fs:[si],'\'
	inc	si

medbs:
	mov	di,offset DesiredVMMName

medtransloop:
	mov	al,[di]
	mov	fs:[si],al
	inc	di
	inc	si
	test	al,al
	jne	medtransloop
	pop	si				; restore si -> filespec start

	mov	edi,offset PageInt
	push	ds
	pop	es
	mov	[edi].RealRegsStruc.Real_DS,_cwInit
	mov	[edi].RealRegsStruc.Real_EDX,esi
	mov	[edi].RealRegsStruc.Real_EAX,3c00h
	mov	[edi].RealRegsStruc.Real_ECX,0
	mov	RealRegsStruc.Real_SS[edi],0
	mov	RealRegsStruc.Real_SP[edi],0
	mov	bl,21h
	call	d[fRawSimulateINT]
	test	RealRegsStruc.Real_Flags[edi],1
	mov	eax,RealRegsStruc.Real_EAX[edi]
	pop	di
	jz	@@v8
	jmp	@@v7
	;

med5a:
	push	di
	mov	si,di
	mov	edi,offset PageInt
	push	ds
	pop	es
	mov	[edi].RealRegsStruc.Real_DS,_cwInit
	mov	[edi].RealRegsStruc.Real_EDX,esi
	mov	[edi].RealRegsStruc.Real_EAX,5a00h
	mov	[edi].RealRegsStruc.Real_ECX,0
	mov	RealRegsStruc.Real_SS[edi],0
	mov	RealRegsStruc.Real_SP[edi],0
	mov	bl,21h
	call	d[fRawSimulateINT]
	test	RealRegsStruc.Real_Flags[edi],1
	mov	eax,RealRegsStruc.Real_EAX[edi]
	pop	di
	jz	@@v8
	;
@@v7:	add	di,128
	jmp	@@v0
	;
@@v8:	;Store the handle and copy the name accross.
	;
	mov	VMMHandle,ax		;store the handle.
	Pushm	ds,fs
	pop	ds
	mov	di,offset VMMName
	mov	cx,128
	rep	movsb
	pop	ds
	mov	al,VMMName
	cmp	al,61h		; 'a'
	jb	@@v2
	cmp	al,7Ah		; 'z'
	ja	@@v2
	and	al,5Fh		;convert to upper case.
@@v2:	mov	VMMName,al

; MED 02/25/96, if pre-allocate, then force write to allocated size
	cmp	PreAllocSize,0
	je	medpre2
	mov	bx,VMMHandle
	mov	ecx,PreAllocSize
	mov	dx,cx
	shr	ecx,16
	mov	ax,4200h	; seek from beginning of file
	int	21h
	xor	cx,cx		; write zero bytes (pre-allocating based on seek)
	mov	ah,40h		; write to file
	int	21h
	mov	ah,68h		; commit file
	int	21h
	xor	dx,dx		; get current size of swap file
	mov	cx,dx
	mov	ax,4202h
	int	21h
	mov	WORD PTR SwapFileLength,ax	; update internal swapfile length variable
	mov	WORD PTR SwapFileLength+2,dx

medpre2:
	;
	;Now patch the exception vector.
	;
	mov	bl,14		;Page fault vector.
	movzx	eax,bl
	mov	ebx,eax
	shl	ebx,1		;*2
	mov	eax,ebx
	shl	ebx,1		;*4
	add	ebx,eax		;*6
	push	ds
	mov	ax,DpmiEmuDS
	mov	ds,ax
	assume ds:_cwDPMIEMU
	add	ebx,offset ExceptionTable
	mov	edx,[ebx]		;get offset.
	mov	cx,4[ebx]		;get segment selector.
	mov	w[OldExcep14+4],cx
	mov	d[OldExcep14],edx	;store 32 bit offset.
	mov	d[ebx],offset VirtualFault	;set offset.
	mov	w[ebx+4],DpmiEmuCS	;set segment selector.
	or	DpmiEmuSystemFlags,1 shl 1 ;flag VMM's presence.
	mov	ax,MainDS
	mov	ds,ax
	assume ds:_cwMain
	or	SystemFlags,1 shl 1	;flag VMM's presence.
	assume ds:_cwRaw
	pop	ds
	or	RawSystemFlags,1 shl 1	;flag VMM's presence.
@@v9:	;
	push	ds
	pop	fs

	mov	ax,InitDS
	mov	ds,ax
	assume ds:_cwInit
	jmp	@@InProtected
;
;Do initialisations needed for DPMI
;
@@InitDPMI:	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
;
;Get some memory for the transfer buffer.
;
	mov	cs:IErrorNumber,5
	mov	bx,8192/16
	mov	ah,48h
	int	21h		;get memory for transfer buffer.
	jc	InitError
	mov	TransferReal,ax
;
;Get some memory for the INT buffer.
;
	mov	bx,((RawStackTotal/2)/16)+1
	mov	ah,48h
	int	21h
	jc	InitError
	mov	DPMIStackSeg,ax
	mov	DPMIStackOff,RawStackTotal/2
;
;Do instalation check and get mode switch address.
;
	mov	cs:IErrorNumber,9
	mov	ax,1687h		;DPMI instalation check.
	int	2fh
	or	ax,ax		;None-zero means its not there.
	jnz	InitError
;
;Check for 32-bit support if needed.
;
	test	SystemFlags,1 shl 14	;Dual mode?
	jnz	@@Use16Bit23
	;
	test	SystemFlags,1
	jz	@@Use32Bit23
	jmp	@@Use16Bit23
@@Use32Bit23:	mov	cs:IErrorNumber,9
	test	bx,1		;Must offer 32 bit support.
	jz	InitError
@@Use16Bit23:	mov	bx,si		;Get DPMI save buffer size.
	mov	ax,si
	or	bx,bx
	jz	@@d0		;No guarante that it'll need it.
;
;Allocate memory for DPMI state save buffer.
;
	mov	cs:IErrorNumber,5
	push	di
	push	es
	mov	ah,48h
	int	21h		;Try and claim memory for it.
	pop	es
	pop	di
	mov	bx,ax
	jc	InitError
@@d0:	Pushm	ax,ds
	mov	ax,_cwInit
	mov	ds,ax
	assume ds:_cwInit
	mov	DPMISwitch,di	;Store the switch call address.
	mov	DPMISwitch+2,es
	mov	es,bx
	Popm	ax,ds
	assume ds:_cwMain
;
;Attempt to switch mode.
;
	test	SystemFlags,1
	jz	@@Use32Bit24
	xor	ax,ax		;16 bit segments for this code.
	jmp	@@Use16Bit24
@@Use32Bit24:	mov	ax,1		;32 bit segments for this code.
@@Use16Bit24:	push	ax
	mov	ax,_cwInit
	mov	ds,ax
	pop	ax
	assume ds:_cwInit
	pusha
	call	d[DPMISwitch]	;Make the switch.
	popa
	jnc	@@DpmiInProtected
	mov	IErrorNumber,9
	push	ax
	mov	ax,_cwMain
	mov	ds,ax
	pop	ax
	assume ds:_cwMain
	test	w[SystemFlags+2],1	;Dual mode?
	jz	InitError
	xor	SystemFlags,1
	xor	ax,1		;toggle the mode.
	push	ax
	mov	ax,_cwInit
	mov	ds,ax
	pop	ax
	assume ds:_cwInit
	call	d[DPMISwitch]	;Make the switch.
	jc	InitError		;really isn't feeling well.
@@DpmiInProtected:
	mov	iDataSegment,ds
	mov	iCodeSegment,cs
	mov	iStackSegment,ss
	mov	iPSPSegment,es
	xor	eax,eax
	mov	ax,sp
	mov	esp,eax
	mov	ax,es:[2ch]
	mov	iENVSegment,ax
;
;Create _cwMain code segment.
;
	mov	IErrorNumber,8
	mov	ax,0000h
	mov	cx,1
	int	31h		;allocate a selector.
	jc	InitError
	mov	mCodeSegment,ax
	mov	bx,ax
	push	ds
	pop	es
	mov	di,offset dpmiSelBuffer
	mov	si,_cwMain
	movzx	esi,si
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	call	MakeDesc
	mov	ax,000ch
	push	ds
	pop	es
	mov	edi,offset dpmiSelBuffer
	int	31h
;
;Create _cwMain data segment.
;
	mov	IErrorNumber,8
	mov	ax,0000h
	mov	cx,1
	int	31h		;allocate a selector.
	jc	InitError
	mov	mDataSegment,ax
	mov	bx,ax
	push	ds
	pop	es
	mov	di,offset dpmiSelBuffer
	mov	si,_cwMain
	movzx	esi,si
	shl	esi,4
	mov	ecx,65535
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	mov	ax,000ch
	push	ds
	pop	es
	mov	edi,offset dpmiSelBuffer
	int	31h
;
;Create a 0-4G selector.
;
	mov	IErrorNumber,8
	mov	ax,0000h
	mov	cx,1
	int	31h		;allocate a selector.
	jc	InitError
	mov	iRealSegment,ax
	mov	bx,ax
	push	ds
	pop	es
	mov	di,offset dpmiSelBuffer
	xor	esi,esi
	or	ecx,-1
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	mov	ax,000ch
	push	ds
	pop	es
	mov	edi,offset dpmiSelBuffer
	int	31h
;
;Make main seg addressable now.
;
	mov	ax,mDataSegment
	mov	es,ax
	assume es:_cwMain
	mov	ax,mCodeSegment
	mov	es:CodeSegment,ax
	mov	es:w[TerminationHandler+4],ax
	mov	ax,mDataSegment
	mov	es:DataSegment,ax
	mov	ax,iStackSegment
	mov	es:StackSegment,ax
	mov	ax,iPSPSegment
	mov	es:PSPSegment,ax
	mov	ax,iENVSegment
	mov	es:ENVSegment,ax
	mov	ax,iRealSegment
	mov	es:RealSegment,ax
	mov	ax,iCodeSegment
	mov	es:CodeSegmenti,ax
	mov	ax,iDataSegment
	mov	es:DataSegmenti,ax
	push	es
	pop	ds
	assume es:nothing
	assume ds:_cwMain


;
;Patch INT 21h exec function to preserve the stack.
;
	if	0
	mov	bl,21h
	mov	ax,200h
	int	31h
	mov	w[OldInt21hExec],dx
	mov	w[OldInt21hExec+2],cx
	mov	dx,offset Int21hExecPatch
	mov	cx,_cwMain
	mov	bl,21h
	mov	ax,201h
	int	31h
	endif
;
;Make right stuff addresable again.
;
	mov	ds,DataSegmenti
	assume ds:_cwInit
	or	IProtectedMode,-1
;
;Now get on with installing the higher level stuff.
;
@@InProtected:	mov	ax,mDataSegment
	mov	ds,ax
	assume ds:_cwMain
	;
	mov	ax,ProtectedType	;Copy protected mode environment type into common
	shl	ax,1+1		;variable for application access. Might become useful
	or	w[SystemFlags],ax	;at some point. Other flags can be added at will.
	mov	ax,ProtectedFlags
	shl	ax,1+1+2
	or	w[SystemFlags],ax
	or	w[SystemFlags],32768	;Flags us in protected mode.
	;
	mov	ax,DataSegmenti
	mov	ds,ax
	assume ds:_cwInit
;
;Add CW API patch to int 31h and 2Fh.
;
	mov	IErrorNumber,5
	xor	bx,bx
	mov	cx,offset _apiCodeEnd-_apiCodeStart
	mov	ax,0501h
	int	31h		;Get memory.
	jc	InitError
	xor	si,si
	mov	di,offset _apiCodeEnd-_apiCodeStart
	mov	ax,0600h
	int	31h		;Lock memory.
	jc	InitError
	shl	ebx,16
	mov	bx,cx
	mov	dpmiSelBase,ebx
;
;Allocate code selector.
;
	mov	IErrorNumber,8
	mov	ax,0000h
	mov	cx,1
	int	31h		;allocate a selector.
	jc	InitError
	mov	dpmiCodeSel,ax
	mov	bx,ax
	push	ds
	pop	es
	mov	di,offset dpmiSelBuffer
	mov	esi,dpmiSelBase
	mov	ecx,offset _apiCodeEnd-_apiCodeStart
	mov	al,1 shl 6
	mov	ah,DescPresent+DescPL3+DescMemory+DescERCode
	call	MakeDesc
	mov	ax,000ch
	push	ds
	pop	es
	mov	edi,offset dpmiSelBuffer
	int	31h
;
;Allocate data selector.
;
	mov	IErrorNumber,8
	mov	ax,0000h
	mov	cx,1
	int	31h		;allocate a selector.
	jc	InitError
	mov	dpmiDataSel,ax
	mov	bx,ax
	push	ds
	pop	es
	mov	di,offset dpmiSelBuffer
	mov	esi,dpmiSelBase
	mov	ecx,offset _apiCodeEnd-_apiCodeStart
	xor	al,al
	mov	ah,DescPresent+DescPL3+DescMemory+DescRWData
	call	MakeDesc
	mov	ax,000ch
	push	ds
	pop	es
	mov	edi,offset dpmiSelBuffer
	int	31h
;
;Copy API code into the new memory.
;
	Pushm	ds,es
	mov	es,dpmiDataSel
	mov	ds,iRealSegment
	xor	edi,edi
	mov	esi,edi
	mov	si,seg _apiCode
	shl	esi,4
	mov	ecx,offset _apiCodeEnd-_apiCodeStart
	db 67h
	rep	movsb
	Popm	ds,es
;
;Setup descriptors in new memory.
;
	Pushm	ds,es
	mov	es,dpmiDataSel
	mov	ds,mDataSegment
	assume ds:_cwMain
	assume es:_apiCode
	mov	es:apiDSeg,ds
	mov	es:apiDDSeg,es
	mov	eax,d[SystemFlags]
	mov	es:d[apiSystemFlags],eax
;
;Set INT vector to bring API code into play.
;
	mov	bl,31h
	mov	ax,204h
	int	31h
	test	SystemFlags,1
	jz	@@Use32
	mov	es:w[OldIntSys],dx
	mov	es:w[OldIntSys+2],cx
	jmp	@@Use0
@@Use32:	mov	es:d[OldIntSys],edx
	mov	es:w[OldIntSys+4],cx
@@Use0:	mov	bl,31h
	mov	edx,offset cwAPIpatch
	Pushm	ax,ds
	mov	ax,DataSegmenti
	mov	ds,ax
	assume ds:_cwInit
	mov	cx,dpmiCodeSel
	assume ds:_cwMain
	Popm	ax,ds
	mov	ax,205h
	int	31h
;
;Copy version through to API.
;
	mov	al,b[VersionMajor]
	sub	al,'0'
	mov	es:cwMajorVersion,al
	mov	al,b[VersionMinor]
	sub	al,'0'
	shl	al,1	;*2
	mov	ah,al
	shl	al,2	;*8
	add	ah,al
	mov	al,b[VersionMinor+1]
	sub	al,'0'
	add	al,ah
	mov	es:cwMinorVersion,al
	assume es:nothing
	assume ds:_cwInit
	Popm	ds,es
;
;Set flag so we know the API is in place.
;
	mov	ax,dpmiDataSel
	mov	apiDataSegi,ax
	mov	bx,dpmiCodeSel
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	mov	apiDataSeg,ax
	mov	apiCodeSeg,bx
	mov	d[TerminationHandler],offset InitError
	mov	w[TerminationHandler+4],cs
	assume ds:_cwInit
	pop	ds
;
;Sort out state save address & size.
;
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	mov	ax,0305h
	int	31h
	jc	@@NoState
	mov	w[DPMIStateSize],ax
	mov	w[DPMIStateSize+2],0
	test	SystemFlags,1
	jz	@@DS_Use32
	mov	w[DPMIStateAddr+2],si
	mov	w[DPMIStateAddr],di
	jmp	@@NoState
@@DS_Use32:	mov	w[DPMIStateAddr+4],si
	mov	d[DPMIStateAddr],edi
	;
@@NoState:	pop	ds
	assume ds:_cwInit
;
;Patch exception vectors to put API handlers in place.
;
	mov	ax,cs
	push	ax
	mov	ax,offset @@pe0
	push	ax
	mov	ax,dpmiCodeSel
	push	ax
	mov	eax,offset PatchExc
	push	ax
	retf
@@pe0:	;
;
;Get memory for new PSP.
;
	mov	IErrorNumber,5
	mov	ecx,(size PSP_Struc)+(size EPSP_Struc)
	Sys	GetMem32
	jc	InitError
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	xchg	bx,PSPSegment
	Pushm	ds,es
	mov	es,PSPSegment
	mov	ds,bx
	xor	si,si
	xor	di,di
	mov	cx,256/4
	rep	movsd
	Popm	ds,es
	assume ds:_cwInit
	pop	ds
;
;Initialise PSP fields.
;
	mov	IErrorNumber,8
	push	ds
	push	es
	mov	ds,mDataSegment
	assume ds:_cwMain
	xor	edx,edx
	mov	es,PSPSegment
	mov	ax,ENVSegment
	mov	es:[PSP_Struc.PSP_Environment],ax	;Setup ENV in PSP.
	mov	ax,RealENVSegment
	mov	es:w[EPSP_Struc.EPSP_RealENV],ax
	mov	ax,offset Int21hExecCount
	mov	es:w[EPSP_Struc.EPSP_ExecCount],ax
	mov	es:w[EPSP_Struc.EPSP_ExecCount+2],ds
	mov	es:d[EPSP_Struc.EPSP_Resource],edx	;Clear memory fields.
	mov	es:d[EPSP_Struc.EPSP_INTMem],edx
	mov	es:d[EPSP_Struc.EPSP_DPMIMem],edx
	mov	es:w[EPSP_Struc.EPSP_Parent],es	;set parent PSP.
	mov	es:w[EPSP_Struc.EPSP_Next],dx
	mov	es:d[EPSP_Struc.EPSP_DTA],80h	;Use default PSP DTA.
	mov	es:w[EPSP_Struc.EPSP_DTA+4],es
	mov	eax,16384-(mcbChunkLen+mcbLen)
	mov	es:d[EPSP_Struc.EPSP_mcbMaxAlloc],eax
	mov	es:d[EPSP_Struc.EPSP_mcbHead],edx
	mov	es:w[EPSP_Struc.EPSP_SegBase],0
	mov	es:w[EPSP_Struc.EPSP_SegSize],0
	mov	es:d[EPSP_Struc.EPSP_LastPSP],0
	mov	es:d[EPSP_Struc.EPSP_NextPSP],0
	mov	es:d[EPSP_Struc.EPSP_Exports],0
	mov	es:d[EPSP_Struc.EPSP_Imports],0
	mov	es:d[EPSP_Struc.EPSP_Links],80000000h
	mov	es:d[EPSP_Struc.EPSP_EntryCSEIP+4],0
	mov	es:w[EPSP_Struc.EPSP_PSPSel],es
	mov	BasePSP,es
	mov	bx,es
	Sys	GetSelDet32
	mov	BasePSPAddress,edx
	;
	Sys	GetSel
	jc	InitError
	movzx	edx,es:w[PSP_Struc.PSP_HandlePtr+2]
	shl	edx,4
	movzx	ecx,es:w[PSP_Struc.PSP_Handles]
	movzx	eax,es:w[PSP_Struc.PSP_HandlePtr]
	add	edx,eax
	Sys	SetSelDet32
	mov	dx,bx
@@normal:	mov	es:w[PSP_Struc.PSP_HandlePtr+2],dx
	mov	es:w[PSP_Struc.PSP_HandlePtr],0
	pop	es
	assume ds:_cwInit
	pop	ds
;
;Setup transfer buffer and selector.
;
	mov	IErrorNumber,8
	Sys	GetSel
	jc	InitError
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	movzx	edx,TransferReal
	shl	edx,4
	mov	ecx,TransferSize
	Sys	SetSelDet32
	jc	InitError
	push	es
	mov	es,PSPSegment
	mov	es:w[EPSP_Struc.EPSP_TransProt],bx
	mov	ax,TransferReal
	mov	es:w[EPSP_Struc.EPSP_TransReal],ax
	mov	eax,TransferSize
	mov	es:d[EPSP_Struc.EPSP_TransSize],eax
	pop	es
	assume ds:_cwInit
	pop	ds
;
;Setup internaly EXPORT'ed symbols.
;
	mov	bx,dpmiDataSel
	Sys	GetSelDet32
	mov	edi,edx
	add	edi,offset apiExports
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	push	es
	mov	es,PSPSegment
	mov	es:d[EPSP_Struc.EPSP_Exports],edi
	mov	es,RealSegment
	mov	ebp,es:[edi]
	add	edi,4
	add	es:d[edi],edx
	add	edi,4
@@exp0:	add	es:d[edi],edx
	mov	esi,es:[edi]
	mov	bx,CodeSegment
	cmp	es:w[esi+4],0
	jz	@@exp1
	mov	bx,DataSegment
	cmp	es:w[esi+4],1
	jz	@@exp1
	mov	bx,apiCodeSeg
	cmp	es:w[esi+4],2
	jz	@@exp1
	mov	bx,apiDataSeg
	cmp	es:w[esi+4],3
	jz	@@exp1
	or	bx,-1
@@exp1:	mov	es:[esi+4],bx
	add	edi,4
	dec	ebp
	jnz	@@exp0
	pop	es
	assume ds:_cwInit
	pop	ds
;
;Initialise extensions. Written in a bit of a hurry but allows
;additional interupt service code to live in other segments.
;
	push	ds
	mov	ds,mDataSegment
	assume ds:_cwMain
	or	mcbAllocations,-1	;Enable MCB code.
	;
	mov	di,offset ExtensionList
@@e0:	cmp	w[di],-1		;end of the list?
	jz	@@e9
	mov	bp,[di]		;get pointer to details.
	movzx	ecx,ds:w[bp+4]	;get patch size.
	Sys	GetMemLinear32	;get some memory.
	push	ds
	mov	ds,DataSegmenti
	assume ds:_cwInit
	mov	IErrorNumber,5
	assume ds:_cwMain
	pop	ds
	jc	InitError
	Sys	LockMem32		;lock the memory.
	jc	InitError
	Sys	GetSel		;get a selector to use for
	push	ds
	mov	ds,DataSegmenti
	assume ds:_cwInit
	mov	IErrorNumber,8	;the code segment.
	assume ds:_cwMain
	pop	ds
	jc	InitError
	mov	edx,esi
	Sys	SetSelDet32		;set it's base and limit.
	jc	InitError
	push	ecx
	mov	cx,ds:[bp+2]		;Get code seg size.
	Sys	CodeSel		;convert to executable.
	pop	ecx
	mov	ds:[bp+8+4],bx	;store it for now.
	mov	ds:[bp+16+4],bx
	mov	ds:[bp+24],bx
	Sys	GetSel		;get a selector to use for data.
	jc	InitError
	Sys	SetSelDet32		;set it's base and limit.
	jc	InitError
	mov	ds:[bp+28],bx	;store it for now.
	Pushm	di,ds,es
	movzx	esi,ds:w[bp+0]	;get real mode segment base.
	shl	esi,4
	mov	ds,RealSegment
	mov	es,bx
	xor	edi,edi
	db 67h
	rep	movsb
	Popm	di,ds,es
	mov	ds:w[bp+32],-1	;flag installed.
	Pushm	di,bp,ds
	mov	es,DataSegment
	mov	ds,ds:[bp+28]

	push	cs
	push	offset @@e1
	push	es:w[bp+12]
	push	es:w[bp+8]
	retf			;call init routine.
@@e1:	Popm	di,bp,ds
	jc	InitError
	add	di,2
	jmp	@@e0
@@e9:	assume ds:_cwInit
	pop	ds

IFDEF DEBUG3
	push	eax
	push	ebx
	push	ecx
	push	es
	push	ds
	mov	ax,KernalZero
	mov	es,ax

	mov	ebx,0b81e0h
	mov	BYTE PTR es:[ebx],'F'
	mov	BYTE PTR es:[ebx+2],'P'
	mov	BYTE PTR es:[ebx+4],' '
	mov	BYTE PTR es:[ebx+6],' '
	mov	BYTE PTR es:[ebx+24],' '
	mov	BYTE PTR es:[ebx+26],' '

	mov	ax,KernalDS		;Get data descriptor.
	mov	ds,ax
	assume ds:_cwRaw
	movzx	edx,FirstUninitPage
	push	edx
	mov	ecx,edx
	xor	edx,edx
deb3_3:
	rol	ecx,4
	mov	al,cl
	and	al,0fh
	cmp	al,10
	jb	deb3_2
	add	al,7
deb3_2:
	add	al,48
	mov	es:[ebx+edx*2+8],al
	inc	edx
	cmp	edx,8
	jb	deb3_3
	pop	edx

	mov	ecx,200000h
deb3_aloop:
	dec	ecx
;	jne	deb3_aloop
	pop	ds
	pop	es
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

COMMENT !
IFDEF DEBUG3
	push	ds
	push	es
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	edi
	push	ebp

	mov	ax,KernalDS		;Get data descriptor.
	mov	ds,ax
	assume ds:_cwRaw
	movzx	edi,Page1stReal	;get linear address.
	shl	edi,4
	mov	ax,KernalZero
	mov	es,ax
	mov	bp,1024
	sub	esp,10
	push	ss
	pop	ds
moo1:
	mov	ecx,es:[edi]
	xor	edx,edx
deb5_3:
	rol	ecx,4
	mov	al,cl
	and	al,0fh
	cmp	al,10
	jb	deb5_2
	add	al,7
deb5_2:
	add	al,48
	mov	BYTE PTR ss:[esp+edx],al
	inc	edx
	cmp	edx,8
	jb	deb5_3

	mov	WORD PTR ss:[esp+8],0a0dh
	mov	edx,esp
	mov	ecx,10
	mov	ebx,1
	mov	ah,40h
	int	21h

	add	di,4		;next page table entry.
	dec	bp
	jnz	moo1

	add	esp,10
	pop	ebp
	pop	edi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	pop	es
	pop	ds
	jmp	debout1

debout1:
ENDIF
END COMMENT !

;
;We're all done here so switch to main code segment for final re-size and run.
;
	push	mCodeSegment
	push	offset cwOpen
	retf

	.286

@@OldStrat:	;
	dw ?,?

@@NumXMSHandles:
	db ?

; MED, 09/10/99, increase max XMS size to dword
@@XMSSize:	;
;	dw ?
	dd	0

Startup	endp


;-------------------------------------------------------------------------------
;
;Something is wrong with this system so print an error message and get out of
;here.
;
InitError	proc	near
;
;Find out if we're in protected mode or not.
;
	.386p
	assume ds:nothing
	cmp	cs:IProtectedMode,0      	; are we in protected mode?

; MED 07/10/97, allow 286 non-crash at this point
;	jz	@@RealMode
	jnz	chkapi
	jmp	@@RealMode
chkapi:

	cmp	cs:apiDataSegi,0	;API installed?
	jz	@@noAPI
	;
	mov	ds,cs:mDataSegment
	assume ds:_cwMain
;
;Remove extension patches.
;
	mov	di,offset ExtensionList	;list of interupt patches.
@@p1:	cmp	w[di+2],-1		;search for the end of the table so we can restore
	jz	@@p0		;vectors in reverse order.
	add	di,2
	jmp	@@p1
	;
@@p0:	mov	bp,w[di]
	cmp	ds:w[bp+32],-1	;installed?
	jnz	@@p2
	Pushm	di,bp
	push	cs
	push	offset @@p3
	push	ds:w[bp+20]
	push	ds:w[bp+16]
	retf
@@p3:	Popm	di,bp
@@p2:	sub	di,2
	cmp	di,offset ExtensionList-2
	jnz	@@p0
;
;Remove api exception patches.
;
	cmp	apiExcepPatched,0
	jz	@@pe0
	mov	ax,cs
	push	ax
	mov	ax,offset @@pe0
	push	ax
	mov	ax,dpmiCodeSel
	push	ax
	mov	eax,offset UnPatchExc
	push	ax
	retf
@@pe0:	;
;
;Remove the API patch.
;
	mov	ds,cs:mDataSegment
	assume ds:_cwMain
	mov	es,apiDataSeg
	assume es:_apiCode
	test	SystemFlags,1
	jz	@@Use32
	mov	dx,es:w[OldIntSys]
	mov	cx,es:w[OldIntSys+2]
	jmp	@@Use0
@@Use32:	mov	edx,es:d[OldIntSys]
	mov	cx,es:w[OldIntSys+4]
@@Use0:	mov	bl,31h
	mov	ax,205h
	int	31h
	mov	es:d[cwIdentity],0
	mov	es:d[cwIdentity+4],0
	assume es:nothing
	;
@@noAPI:	assume ds:nothing
	cmp	cs:IProtectedType,2	;DPMI?
	assume ds:_cwInit
	jz	@@DPMI
;
;Make RAW stuff addressable.
;
	cli			;Don't want interrupts interfering.
	mov	ax,KernalDS		;Get supervisor data descriptor,
	mov	ds,ax		;DS,ES,FS,GS,SS must be data with 64k limit
	assume ds:_cwRaw
	mov	ax,KernalZero
	mov	es,ax
;
;Switch to RAW exit code.
;
	push	_cwInit
	push	offset @@6
	mov	ax,KernalCS
	push	ax
	mov	ax,offset RawVCPIRealMode
	push	ax
	retf
@@6:	jmp	@@RealMode
;
;Remove DPMI stuff.
;
	.386
@@DPMI:

	if	0
	assume ds:nothing
	mov	ds,cs:iDataSegment
	assume ds:_cwInit
	mov	ds,mDataSegment
	assume ds:_cwMain
	cmp	d[OldInt21hExec],0
	jz	@@d0
	mov	bl,21h
	mov	dx,w[OldInt21hExec]
	mov	cx,w[OldInt21hExec+2]
	mov	ax,201h
	int	31h
@@d0:
	endif

	assume ds:nothing
	mov	ds,cs:iDataSegment
	assume ds:_cwInit

	cmp	IErrorNumber,0
	jz	@@NoError
	mov	ax,IErrorNumber	;Get the error number.
	xor	dx,dx
	mov	cx,10
	div	cx
	add	al,'0'
	mov	b[IErrorM00n],al
	add	dl,'0'
	mov	b[IErrorM00n+1],dl
	mov	dx,w[InitErrorList]

	mov	edi,offset DPMIErrRegs
	push	ds
	pop	es
	mov	RealRegsStruc.Real_EDX[edi],edx
	mov	RealRegsStruc.Real_EAX[edi],900h
	mov	RealRegsStruc.Real_DS[edi],_cwInit
	xor	cx,cx		;No stack parameters.
	mov	bh,ch		;no flags.
	mov	ax,0300h
	int	31h		;Use real dpmi service.
;
;Get a pointer to the appropriate error message and print it.
;
	mov	bx,IErrorNumber
	add	bx,bx
	mov	dx,[InitErrorList+bx]

	mov	edi,offset DPMIErrRegs
	push	ds
	pop	es
	mov	RealRegsStruc.Real_EDX[edi],edx
	mov	RealRegsStruc.Real_EAX[edi],900h
	mov	RealRegsStruc.Real_DS[edi],_cwInit
	xor	cx,cx		;No stack parameters.
	mov	bh,ch		;no flags.
	mov	ax,0300h
	int	31h		;Use real dpmi service.

	jmp	@@NoError
	.286
;
;Make sure our data is addressable.
;
@@RealMode:	mov	ax,_cwInit
	mov	ds,ax
	assume ds:_cwInit
;
;Display the "CauseWay error: ??" bit.
;
@@InRealMode:	cmp	IErrorNumber,0
	jz	@@NoError
	mov	ax,IErrorNumber	;Get the error number.
	xor	dx,dx
	mov	cx,10
	div	cx
	add	al,'0'
	mov	b[IErrorM00n],al
	add	dl,'0'
	mov	b[IErrorM00n+1],dl
	mov	dx,w[InitErrorList]
	mov	ah,9
	int	21h
;
;Get a pointer to the appropriate error message and print it.
;
	mov	bx,IErrorNumber
	add	bx,bx
	mov	dx,[InitErrorList+bx]
	mov	ah,9
	int	21h
;
;Now exit with the error number as the DOS "errorlevel".
;
@@NoError:	mov	ax,IErrorNumber
	mov	ah,4ch
	int	21h
	assume ds:_cwMain
InitError	endp


;-------------------------------------------------------------------------------
;
;Initialise real mode hardware interupt vectors so that control is always passed to protected mode
;even if the interupt occurs in real mode. This simulates the DPMI environment and is essential for
;any program that re-programs IRQ-0 frequency.
;
InitHardwareInts proc near
	.386
	Pushm	ds,es
	mov	ax,KernalDS
	mov	ds,ax
	mov	ax,KernalZero
	mov	es,ax
	assume ds:_cwRaw
;	mov	ch,16
;	mov	cl,1ch
;	call	@@0
	mov	ch,17
	mov	cl,23h		;patch ctrl-break.
	call	@@0
	mov	ch,18
	mov	cl,24h		;patch critical error.
	call	@@0
	Popm	ds,es
	ret
	;
@@0:	mov	ax,size CallBackStruc
	movzx	bx,ch
	mul	bx
	mov	bx,ax
	add	bx,offset CallBackTable
	pushf
	cli
	mov	CallBackStruc.CallBackNum[bx],cl	;set interupt number.
	mov	CallBackStruc.CallBackFlags[bx],1+2	;mark call back as used interupt.
	mov	ax,CallBackSize
	movzx	dx,ch
	mul	dx
	mov	si,offset ICallBackList
	add	si,ax		;index list of calls.
	push	bx
	movzx	bx,cl
	shl	bx,2
	mov	dx,es:[bx]
	mov	cx,es:[bx+2]
	mov	es:[bx],si
	mov	es:w[bx+2],seg _cwRaw
	pop	bx
	mov	w[CallBackStruc.CallBackReal+2+bx],cx	;store origional real mode vector.
	mov	w[CallBackStruc.CallBackReal+bx],dx
	popf
	ret
	assume ds:_cwMain
	.286
InitHardwareInts endp


;-------------------------------------------------------------------------------
CheckProcessor	proc	near
	xor	ax,ax		;Clear the flags.
	push	ax		;/
	popf			;/
	pushf			;Get the flags back.
	pop	ax		;/
	and	ax,0F000h		;Get Bits 11-8
	cmp	ax,0F000h		;Exist on this processor?
	je	@@9		;Must be an 8086
	mov	ax,0F000h		;Setup the flags again.
	push	ax		;/
	popf			;/
	pushf			;Get them back.
	pop	ax		;/
	and	ax,0F000h		;Get Bits 11-8
	jz	@@9		;Valid so must be 80286
	clc
	ret
@@9:	stc
	ret
CheckProcessor	endp


;-------------------------------------------------------------------------------
CheckDOSVersion proc near
	mov	ah,30h
	int	21h		;Get DOS version.
	mov	DOSVersion,ax
	cmp	al,3		;3.? or above?
	jc	@@9
	jnz	@@0		;less means trouble.
	cmp	ah,1
	jc	@@9
@@0:	clc
	ret
@@9:	stc
	ret
CheckDOSVersion endp


;-------------------------------------------------------------------------------
;
;Check for "CAUSEWAY" environment variable and fetch any relevent settings.
;
GetENVStuff	proc	near
	.386
	mov	es,RealENVSegment
	xor	si,si
@@0:	mov	eax,es:[si]
	cmp	eax,"SUAC"
	jnz	@@1
	mov	eax,es:[si+4]
	cmp	eax,"YAWE"
	jz	@@2
@@1:	inc	si
	cmp	es:b[si-1],0
	jnz	@@1
	cmp	es:b[si],0		;end of all strings?
	jnz	@@0
	jmp	@@9
	;
@@2:	;Found "CAUSEWAY" so have a look at the settings.
	;
	add	si,8		;skip to "="
	push	si
@@2_0:	mov	al,es:[si]
	cmp	al,61h		; 'a'
	jb	@@2_1
	cmp	al,7Ah		; 'z'
	ja	@@2_1
	and	al,5Fh		;convert to upper case.
@@2_1:	mov	es:[si],al
	inc	si
	or	al,al
	jnz	@@2_0
	pop	si
	;
@@3:	cmp	es:b[si]," "
	jnz	@@4
	inc	si
	jmp	@@3
	;
@@4:	cmp	es:b[si],0		;end?
	jz	@@9
	;
	mov	eax,es:[si]
	cmp	eax,"MVON"		;NOVM?
	jz	@@novm
	cmp	eax,"MXAM"		;MAXMEM?
	jz	@@maxmem
	cmp	eax,"ATXE"		;EXTALL?
	jz	@@extall
	cmp	eax,"IMPD"		;DPMI?
	jz	@@dpmi
	cmp	eax,"PAWS"		;swap?
	jz	@@swap
	cmp	eax,"MWOL"		;lowmem?
	jz	@@lowmem
	cmp	eax,"EMIH"		; himem?
	jz	@@himem
	cmp	eax,"APON"		; nopass?
	jz	@@nopass
	cmp	eax,":ERP"		; pre?
	jz	@@pre
	cmp	eax,"EMAN"		; NAME?
	jz	@@name
	cmp	eax,"1DAP"		; PAD1?
	jz	@@pad1
	cmp	eax,"XEON"		; NOEX?
	jz	@@noex
	cmp	eax,"1GIB"		; BIG1?
	jz	@@big1
	inc	si
	jmp	@@3
	;
@@nopass:	; shut off passing of real mode interrupts to protected mode
	add	si,4
	mov	ax,es:[si]
	cmp	ax,"SS"
	jnz	@@3
	add	si,2
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	or	NoPassFlag,-1
	assume ds:_cwMain
	pop	ds
	jmp	@@3

@@big1:	; specify alternate extended memory size computation
	add	si,4
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	or	Big1Flag,-1
	assume ds:_cwMain
	pop	ds
	jmp	@@3

@@himem:	;Set amount of physical memory to use.
	;
	add	si,4
	mov	al,es:[si]
	cmp	al,"M"
	jnz	@@3
	inc	si
	cmp	es:b[si],":"
	jnz	@@3
	inc	si
	xor	edx,edx
@@hm0:	mov	al,es:[si]
	or	al,al
	jz	@@hm1
	cmp	al," "
	jz	@@hm1
	cmp	al,"0"
	jc	@@hm1
	cmp	al,"9"+1
	jnc	@@hm1
	sub	al,"0"
	movzx	eax,al
	add	edx,edx
	mov	ebx,edx
	add	edx,edx
	add	edx,edx
	add	edx,ebx
	add	edx,eax
	inc	si
	jmp	@@hm0
@@hm1:
	cmp	edx,4096*1024
	jnc	@@3

	shl	edx,10		;turn K into byte's
	shr	edx,12		;get number of pages.
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	mov	d[MaxMemPhys],edx
	assume ds:_cwMain
	pop	ds
	jmp	@@3
	;
@@extall:	;Set flag to use all extended memory.
	;
	add	si,4
	mov	ax,es:[si]
	cmp	ax,"LL"
	jnz	@@3
	add	si,2
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	or	ExtALLSwitch,-1
	assume ds:_cwMain
	pop	ds
	jmp	@@3
	;
@@novm:	;They want to disable VM.
	;
	add	si,4
	push	ds
	mov	bx,_cwInit
	mov	ds,bx
	assume ds:_cwInit
	or	NoVMSwitch,-1
	assume ds:_cwMain
	pop	ds
	jmp	@@3
	;
@@maxmem:	;Want to set maximum linear address space size.
	;
	add	si,4
	mov	ax,es:[si]
	cmp	ax,"ME"
	jnz	@@3
	add	si,2
	cmp	es:b[si],":"
	jnz	@@3
	inc	si
	xor	edx,edx
@@mm0:	mov	al,es:[si]
	or	al,al
	jz	@@mm1
	cmp	al," "
	jz	@@mm1
	cmp	al,"0"
	jc	@@mm1
	cmp	al,"9"+1
	jnc	@@mm1
	sub	al,"0"
	movzx	eax,al
	add	edx,edx
	mov	ebx,edx
	add	edx,edx
	add	edx,edx
	add	edx,ebx
	add	edx,eax
	inc	si
	jmp	@@mm0
@@mm1:
	cmp	edx,4096
	jnc	@@3

	shl	edx,20		;turn Meg into byte's
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	mov	d[MaxMemLin],edx
	assume ds:_cwMain
	pop	ds
	jmp	@@3

@@pre:	;Want to set preallocate amount
	;
	add	si,4
	xor	edx,edx
@@pr0:	mov	al,es:[si]
	or	al,al
	jz	@@pr1
	cmp	al," "
	jz	@@pr1
	cmp	al,"0"
	jc	@@pr1
	cmp	al,"9"+1
	jnc	@@pr1
	sub	al,"0"
	movzx	eax,al
	add	edx,edx
	mov	ebx,edx
	add	edx,edx
	add	edx,edx
	add	edx,ebx
	add	edx,eax
	inc	si
	jmp	@@pr0
@@pr1:
	cmp	edx,4096
	jnc	@@3

	shl	edx,20		;turn Meg into byte's
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	mov	d[PreAllocSize],edx
	assume ds:_cwMain
	pop	ds
	jmp	@@3

@@pad1:
	mov	Pad1Flag,1
	add	si,4
	jmp	@@3

@@noex:
	mov	NoEXECPatchFlag,1
	add	si,4
	jmp	@@3

@@dpmi:	;They want to force DPMI use if possible.
	;
	mov	ProtectedForce,1
	add	si,4
	jmp	@@3
	;
@@swap:	;They want to specify the swap drive.
	;
	add	si,4
	cmp	es:b[si],":"
	jnz	@@3
	inc	si
	mov	di,offset VMMDrivPath1
	push	ds
	mov	bx,_cwInit
	mov	ds,bx
	assume ds:_cwInit
@@s0:	mov	al,es:[si]
	mov	[di],al
	inc	si
	inc	di
	or	al,al
	jz	@@s1

	cmp	al,";"	; MED 02/25/96
	je	@@s1

	cmp	al," "
	jnz	@@s0
@@s1:	mov	b[di-1],0
	assume ds:_cwMain
	pop	ds
	dec	si
	jmp	@@3
	;
@@name:	; Specify the swap name.
	;
	add	si,4
	cmp	es:b[si],":"
	jnz	@@3
	inc	si
	mov	di,offset DesiredVMMName
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	xor	dx,dx
@@n0:	mov	al,es:[si]
	mov	[di],al
	inc	si
	inc	di
	or	al,al
	jz	@@n1
	inc	dx
	cmp	dx,12
	ja	@@n1		; don't allow more than 12 chars in file name

	cmp	al,";"	; MED 02/25/96
	je	@@n1

	cmp	al," "
	jnz	@@n0
@@n1:	mov	b[di-1],0
	assume ds:_cwMain
	pop	ds
	dec	si
	jmp	@@3
	;
@@lowmem:	;They want to specify low memory retention.
	;
	add	si,4
	mov	ax,es:[si]
	cmp	ax,"ME"
	jnz	@@3
	add	si,2
	cmp	es:b[si],":"
	jnz	@@3
	inc	si
	xor	edx,edx
@@lm0:	mov	al,es:[si]
	or	al,al
	jz	@@lm1
	cmp	al," "
	jz	@@lm1
	cmp	al,"0"
	jc	@@lm1
	cmp	al,"9"+1
	jnc	@@lm1
	sub	al,"0"
	movzx	eax,al
	add	edx,edx
	mov	ebx,edx
	add	edx,edx
	add	edx,edx
	add	edx,ebx
	add	edx,eax
	inc	si
	jmp	@@lm0
	;
@@lm1:	shl	edx,10-4		;turn K into para's
	push	ds
	mov	bx,_cwRaw
	mov	ds,bx
	assume ds:_cwRaw
	movzx	ebx,w[CONVSaveSize]
	add	edx,ebx
	cmp	edx,65535
	jc	@@lm2
	mov	edx,65535
@@lm2:	mov	w[CONVSaveSize],dx	;set new size.
	assume ds:_cwMain
	pop	ds
	;
	jmp	@@3
	;
@@9:	;Now look for TEMP.
	;
	xor	si,si		;start at the beginning.
@@temp2:	mov	eax,es:[si]
	cmp	eax,"PMET"
	jz	@@temp0
@@temp1:	inc	si
	cmp	es:b[si-1],0
	jnz	@@temp1
	cmp	es:b[si],0
	jz	@@temp9
	jmp	@@temp2
	;
@@temp0:	add	si,4
	cmp	es:b[si],"="
	jnz	@@temp9
	inc	si
	push	ds
	mov	bx,_cwInit
	mov	ds,bx
	mov	di,offset VMMDrivPath2
@@temp3:	mov	al,es:[si]
	mov	[di],al
	inc	si
	inc	di
	or	al,al
	jz	@@temp4
	cmp	al," "
	jnz	@@temp3
@@temp4:	mov	b[di-1],0
	pop	ds
@@temp9:	;
	;Look for TMP
	;
	xor	si,si		;start at the beginning.
@@tmp2:	mov	eax,es:[si]
	cmp	eax,"=PMT"
	jz	@@tmp0
@@tmp1:	inc	si
	cmp	es:b[si-1],0
	jnz	@@tmp1
	cmp	es:b[si],0
	jz	@@tmp9
	jmp	@@tmp2
	;
@@tmp0:	add	si,4
	push	ds
	mov	bx,_cwInit
	mov	ds,bx
	mov	di,offset VMMDrivPath3
@@tmp3:	mov	al,es:[si]
	mov	[di],al
	inc	si
	inc	di
	or	al,al
	jz	@@tmp4
	cmp	al," "
	jnz	@@tmp3
@@tmp4:	mov	b[di-1],0
	pop	ds
@@tmp9:	;
	ret
	.286
GetENVStuff	endp


;-------------------------------------------------------------------------------
GetEXECName	proc	near
	.386
@@0:	mov	es,RealENVSegment
	xor	si,si
@@1:	mov	al,es:[si]		;Get a byte.
	inc	si		;/
	or	al,al		;End of a string?
	jnz	@@1		;keep looking.
	mov	al,es:[si]		;Double zero?
	or	al,al		;/
	jnz	@@1		;keep looking.
	add	si,3		;Skip last 0 and word count.
	mov	di,offset MainExec
	mov	bx,_cwInit
	mov	fs,bx
	mov	bx,offset VMMDrivPath4
	mov	cx,128
	mov	dx,bx
	Pushm	ds,ds,es
	Popm	es,ds
@@2:	movsb
	mov	al,[si-1]
	mov	fs:[bx],al
	inc	bx
	cmp	b[si-1],"\"
	jnz	@@2_0
	mov	dx,bx
	dec	dx
@@2_0:	cmp	b[si-1],0		;got to the end yet?
	jz	@@3
	dec	cx
	jnz	@@2
@@3:	mov	bx,dx
	mov	fs:b[bx],0		;terminate VMM path.

COMMENT !
; MED 10/10/96
	cmp	bx,offset VMMDrivPath4
	jne	genexit			; non-null path
	mov	BYTE PTR fs:[bx],'.'	; null path, give a valid one of '.\'
	mov	BYTE PTR fs:[bx+1],'\'
	mov	BYTE PTR fs:[bx+2],0
END COMMENT !

genexit:
	pop	ds
	Pushm	ds,ds
	Popm	es,fs
	ret
	.286
GetEXECName	endp


;-------------------------------------------------------------------------------
GetSystemFlags	proc	near
	push	ds
	mov	dx,offset MainExec
	mov	ax,3d00h		;open, read only.
	int	21h
	jc	@@5
	push	cs
	pop	ds
	assume ds:_cwInit
	mov	bx,ax
	mov	dx,offset IExeSignature	;somewhere to put the info.
	mov	cx,1bh		;size of it.
	mov	ah,3fh
	int	21h
	jc	@@4
	cmp	ax,1bh		;did we read right amount?
	jnz	@@4
	cmp	w[IExeSignature],'ZM'	;Normal EXE?
	jnz	@@4
	mov	ax,w[IExeLength+2]	;get length in 512 byte blocks

; MED 01/17/96
	cmp	WORD PTR [IExeLength],0
	je	medexe2		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe2:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,w[IExeLength]	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	xchg	cx,dx		;swap round for DOS.
	mov	ax,4200h		;set absolute position.
	int	21h
	mov	dx,offset INewHeader	;somewhere to put the info.
	mov	cx,size NewHeaderStruc	;size of it.
	mov	ah,3fh
	int	21h
	jc	@@4
	or	ax,ax		;end of the file?
	jz	@@SetRUN
	cmp	ax,size NewHeaderStruc	;did we read right amount?
	jnz	@@4
	cmp	w[INewHeader],'P3'	;ID ok?
	jnz	@@4
	mov	si,offset INewHeader
	mov	ax,w[NewHeaderStruc.NewFlags+si]	;Copy main flags.
	mov	cx,w[NewHeaderStruc.NewFlags+2+si]
	pop	ds
	push	ds
	assume ds:_cwMain
	mov	w[SystemFlags],ax
	mov	w[SystemFlags+2],cx
	mov	dx,_cwRaw
	mov	ds,dx
	assume ds:_cwRaw
	mov	w[RawSystemFlags],ax
	mov	w[RawSystemFlags+2],cx
	.386
	mov	dx,_cwDPMIEMU
	mov	ds,dx
	assume ds:_cwDPMIEMU
	mov	w[DpmiEmuSystemFlags],ax
	mov	w[DpmiEmuSystemFlags+2],cx
	mov	dx,_apiCode
	mov	ds,dx
	assume ds:_apiCode
	mov	w[apiSystemFlags],ax
	mov	w[apiSystemFlags+2],cx
	.286
	assume ds:_cwMain
@@4:	mov	ax,3e00h
	int	21h
	jmp	@@5

;
;Nothing on the end of the extender so replace the exec name with first
;command line argument and shuffle everything else down. Allows CW32 to be used
;to run 32-bit programs not attatched to it from the command line.
;
@@SetRUN:	mov	ax,3e00h		;close file, we don't need it.
	int	21h
	mov	ax,_cwMain
	mov	ds,ax
	assume ds:_cwMain
	push	es
	mov	es,RealPSPSegment
	mov	si,80h
	xor	ch,ch
	mov	cl,es:b[si]
	or	cx,cx
	jz	@@sr5
	inc	si
	mov	di,offset MainExec	;default to storeing program name.
	;
	;Skip white space.
	;
@@sr0:	mov	al,es:[si]
	cmp	al," "
	jnz	@@sr1
	inc	si
	dec	cx
	jnz	@@sr0
	jmp	@@sr3
	;
	;Get program name.
	;
@@sr1:	mov	al,es:[si]
	cmp	al," "
	jz	@@sr2
	mov	es:b[si],' '
	mov	[di],al
	inc	si
	inc	di
	dec	cx
	jnz	@@sr1
@@sr2:	mov	b[di],0
@@sr3:	pop	es
;
;Clean up the command line, ie, remove any spaces created by removeing name.
;
	push	es
	mov	es,RealPSPSegment
	mov	si,80h
	xor	ch,ch
	mov	cl,es:b[si]
	or	cx,cx
	jz	@@cl3
	inc	si
	mov	di,si
@@cl0:	cmp	es:b[si],' '
	jnz	@@cl1
	inc	si
	dec	cx
	jnz	@@cl0
@@cl1:	jcxz	@@cl2
	push	cx
	push	ds
	push	es
	pop	ds
	rep	movsb		;Copy it down.
	pop	ds
	pop	cx
@@cl2:	mov	es:b[80h],cl		;Store new length.
@@cl3:	xor	ch,ch
	add	cx,81h
	mov	si,cx
	mov	es:b[si],13		;Terminate it correctly.
@@sr5:	pop	es
	;
	assume ds:_cwMain
@@5:	pop	ds
	ret
GetSystemFlags	endp


;-------------------------------------------------------------------------------
GetProtectedType proc near
;
;Find out what protected mode environments are available.
;
	call	ChkDPMI		;32 bit DPMI server present?
	jc	@@0
	or	ProtectedFlags,1
@@0:	call	ChkVCPI		;VCPI >= v1.0 present?
	jc	@@1
	or	ProtectedFlags,2
@@1:	call	ChkRAW		;Running in real mode?
	jc	@@2
	or	ProtectedFlags,4
@@2:	ret
GetProtectedType endp


;-------------------------------------------------------------------------------
SetProtectedType proc near
	cmp	ProtectedForce,0
	jz	@@NoDPMIForce
	test	ProtectedFlags,1
	jnz	@@2
	;
@@NoDPMIForce:	test	ProtectedFlags,4
	jz	@@1
	mov	ProtectedType,0	;Use real mode.
	jmp	@@3
@@1:	test	ProtectedFlags,2
	jz	@@2
	mov	ProtectedType,1	;Use VCPI.
	jmp	@@3
@@2:	mov	ProtectedType,2	;Use DPMI.
@@3:	push	es
	mov	ax,_cwInit
	mov	es,ax
	assume es:_cwInit
	mov	ax,ProtectedType
	mov	es:IProtectedType,ax
	assume es:nothing
	pop	es
	ret
SetProtectedType endp


;-------------------------------------------------------------------------------
ChkDPMI	proc	near
;
;See if DPMI server present.
;
	mov	ax,1687h		;DPMI instalation check.
	int	2fh
	or	ax,ax		;None-zero means its not there.
	jnz	@@9
	test	w[SystemFlags],1
	jz	@@Use32Bit21
	jmp	@@Use16Bit21
@@Use32Bit21:	test	bx,1		;Must offer 32 bit support.
	jz	@@9
@@Use16Bit21:	clc
	ret
	;
@@9:	stc
	ret
ChkDPMI	endp


;-------------------------------------------------------------------------------
;
; The following routine checks to see if a VCPI master program is installed.
; If one is not, the carry flag is set on return
; If one is, the version info is stored and the carry flag is cleared on return
;
ChkVCPI	proc	near
	Pushm	ax,bx,es
	xor	ax,ax
	mov	es,ax
	mov	si,es:[67h*4]	;Check a handler exists.
	mov	di,es:[(67h*4)+2]
	mov	ax,si
	or	ax,di
	jnz	@@IsHandler
	cli
	mov	es:w[67h*4],offset @@DummyIRET
	mov	es:w[(67h*4)+2],cs
@@IsHandler:	Pushm	si,di
	mov	ax,0DE00h		;Get VCPI installed state
	int	67h
	Popm	si,di
	mov	es:[67h*4],si	;Check a handler exists.
	mov	es:[(67h*4)+2],di
	sti
	;
	cmp	al,0
	jne	@@NotThere
	or	bx,3030h		;Turn to ASCII
	cmp	bh,'1'
	jc	@@NotThere
	mov	ax,si
	or	ax,di		;Only pretending to be their?
	jz	@@HopeThere
	;
	call	@@ChkEMS		;Make sure EMS is in first
	jc	@@HopeThere		;EMS not in.
	call	@@GrabPage		;Make sure EMS initiated
@@HopeThere:	clc			;Set for no error
	jmp	@@Done
@@NotThere:	stc
@@Done:	Popm	ax,bx,es
	ret
@@DummyIRET:	iret
;
; The following routine checks to see if an EMM is installed.
; If one is not, the carry flag is set on return
; If one is, the carry flag is cleared on return
;
@@ChkEMS:	Pushm	ax,bx,dx,es
	Pushm	ds,cs
	pop	ds
	mov	dx,offset @@EMSName	;Device driver name
	mov	ah,3Dh		;Open file
	mov	al,0		;Access/file sharing mode
	int	21h
	pop	ds
	jc	@@NotThere2
	mov	bx,ax		;Put handle in proper place
	mov	ah,44h		;IOCTL
	mov	al,07h		;Get output status
	int	21h
	jc	@@NotThere1
	cmp	al,0FFh
	jne	@@NotThere1
	mov	ah,3Eh		;Close file
	int	21h
	clc			;Set for no error
	jmp	@@Done1
@@NotThere1:	mov	ah,3Eh		;Close file
	int	21h
@@NotThere2:	stc
@@Done1:	Popm	ax,bx,dx,es
	ret
;
; This function allocates an EMS page, and then releases it.  This is
; done to make sure the EMS driver has switched the CPU to V86 mode.
; On return, the carry is set if there was any problem using the EMS
; functions.  Carry is clear otherwise.
;
@@GrabPage:	mov	ah,43h		;Allocate pages
	mov	bx,1		;Get 1 page (16K)
	int	67h
	cmp	ah,0		;Was there an error?
	jne	@@GPErr		;Yes, so exit
	mov	ah,45h		;Release EMS handle
	int	67h
	cmp	ah,0		;Was there an error?
	jne	@@GPErr		;Yes, so exit
	clc			;Mark for no error
	jmp	@@GPEnd
@@GPErr:	stc
@@GPEnd:	ret
;
@@EMSName	DB 'EMMXXXX0',0
ChkVCPI	endp


;-------------------------------------------------------------------------------
ChkRAW	proc	near
;
;Can we run on this machine.
;
	.286
	smsw	ax
	and	ax,1         	; are we in protected mode?
	jnz	@@9
	clc
	ret
@@9:	stc
	ret
ChkRAW	endp


;-------------------------------------------------------------------------------
MakeDesc	proc	near
;
;Build a segment descriptor.
;
;On Entry:-
;
;ES:DI	- Descriptor entry to use.
;ESI	- Linear base to set.
;ECX	- limit in bytes.
;AL	- Code size bit.
;AH	- Present/PL/memory|system/type bits.
;
	.386
	pushad
	and	di,not 7		;lose RPL & TI
	cmp	ecx,0100000h  	; see if we need to set g bit
	jc	@@0
	shr	ecx,12       	; div by 4096
	or	al,80h       	; set g bit
@@0:	mov	es:[di],cx		;store low word of limit.
	shr	ecx,16
	or	cl,al
	mov	es:[di+6],cl		;store high bits of limit and gran/code size bits.
	mov	es:[di+2],si		;store low word of linear base.
	shr	esi,16
	mov	bx,si
	mov	es:[di+4],bl		;store mid byte of linear base.
	mov	es:[di+7],bh		;store high byte of linear base.
	mov	es:[di+5],ah		;store pp/dpl/dt/type bits.
	popad
	ret
MakeDesc	endp


;-------------------------------------------------------------------------------
;
;Build a segment descriptor.
;
;On Entry:-
;
;ES:DI	- Descriptor entry to use.
;ESI	- Linear base to set.
;ECX	- limit in bytes.
;AL	- Code size bit.
;AH	- Present/PL/memory|system/type bits.
;
MakeDesc2	proc	near
	.386
	pushad
	and	edi,not 7		;lose RPL & TI
	cmp	ecx,0100000h  	; see if we need to set g bit
	jc	@@0
	shr	ecx,12       	; div by 4096
	or	al,80h       	; set g bit
@@0:	mov	es:[edi],cx		;store low word of limit.
	shr	ecx,16
	or	cl,al
	mov	es:[edi+6],cl	;store high bits of limit and gran/code size bits.
	mov	es:[edi+2],si	;store low word of linear base.
	shr	esi,16
	mov	bx,si
	mov	es:[edi+4],bl	;store mid byte of linear base.
	mov	es:[edi+7],bh	;store high byte of linear base.
	mov	es:[edi+5],ah	;store pp/dpl/dt/type bits.
	popad
	ret
MakeDesc2	endp


;-------------------------------------------------------------------------------
;
;Dummy routine to call so that IDT value is re-loaded.
;
IDTFlush	proc	far
	ret
IDTFlush	endp


;-------------------------------------------------------------------------------
Bordi	proc	near
	Pushm	ax,dx
	mov	ah,al
	mov	dx,3dah
	in	al,dx
	mov	dl,0c0h
	mov	al,11h
	out	dx,al
	mov	al,ah
	out	dx,al
	mov	al,20h
	out	dx,al
	Popm	ax,dx
	ret
Bordi	endp


;-------------------------------------------------------------------------------
;
;Initialisation specific data.
;
apiDataSegi	dw 0
IProtectedMode	db 0
IProtectedType	dw 0
DPMISwitch	dw ?,?
dpmiSelBase	dd 0
dpmiCodeSel	dw ?
dpmiDataSel	dw ?
;
iCodeSegment	dw InitCS
mCodeSegment	dw MainCS
iDataSegment	dw InitDS
mDataSegment	dw MainDS
iStackSegment	dw MainSS
iPSPSegment	dw MainPSP
iENVSegment	dw MainEnv
iRealSegment	dw KernalZero
;
INewHeader	NewHeaderStruc <>	;make space for a header.
;
IExeSignature	db ?	;00 Identifier text.
	db ?	;01 /
IExeLength	dw ?	;02 Length of file MOD 512
	dw ?	;04 Length of file in 512 byte blocks.
IExeRelocNum	dw ?	;06 Number of relocation items.
IExeHeaderSize	dw ?	;08 Length of header in 16 byte paragraphs.
IExeMinAlloc	dw ?	;0A Minimum number of para's needed above program.
IExeMaxAlloc	dw ?	;0C Maximum number of para's needed above program.
IExeStackSeg	dw ?	;0E Segment displacement of stack module.
IExeEntrySP	dw ?	;10 value for SP at entry.
IExeCheckSum	dw ?	;12 Check sum...
IExeEntryIP	dw ?	;14 Contents of IP at entry.
IExeEntryCS	dw ?	;16 Segment displacement of CS at entry.
IExeRelocFirst	dw ?	;18 First relocation item offset.
IExeOverlayNum	db ?	;1A Overlay number.
;
IErrorNumber	dw 0
InitErrorList	dw IErrorM00,IErrorM01,IErrorM02,IErrorM03,IErrorM04,IErrorM05,IErrorM06,IErrorM07
	dw IErrorM08,IErrorM09
IErrorM00	db 'CauseWay error '
IErrorM00n	db '00 : $'
IErrorM01	label byte
	if ENGLISH
	db 'Unable to re-size program memory block.',13,10,'$'
	elseif SPANISH
	db "Incapaz de redimensionar el bloque de memoria del programa",13,10,"$"
	endif
IErrorM02	label byte
	if ENGLISH
	db '386 or better required.',13,10,'$'
	elseif SPANISH
	db "Se requiere un 386 o superior.",13,10,"$"
	endif
IErrorM03	label byte
	if ENGLISH
	db 'Non-standard protected mode program already active.',13,10,'$'
	elseif SPANISH
	db "Programa en modo protegido no estandar activado.",13,10,"$"
	endif
IErrorM04	label byte
	if ENGLISH
	db 'DOS 3.1 or better required.',13,10,'$'
	elseif SPANISH
	db "DOS 3.1 o superior requerido.",13,10,"$"
	endif
IErrorM05	label byte
	if ENGLISH
	db 'Not enough memory for CauseWay.',13,10,'$'
	elseif SPANISH
	db "Memoria insuficiente para CauseWay.",13,10,"$"
	endif
IErrorM06	label byte
	if ENGLISH
	db 'VCPI failed to switch into protected mode.',13,10,'$'
	elseif SPANISH
	db "VCPI fallo al cambiar a modo protegido.",13,10,"$"
	endif
IErrorM07	label byte
	if ENGLISH
	db 'Unable to control A20.',13,10,'$'
	elseif SPANISH
	db "Capaz de controlar A20.",13,10,"$"
	endif
IErrorM08	label byte
	if ENGLISH
	db 'Selector allocation error.',13,10,'$'
	elseif SPANISH
	db "Error de asignacion del selector.",13,10,"$"
	endif
IErrorM09	label byte
	if ENGLISH
	db 'DPMI failed to switch to protected mode.',13,10,'$'
	elseif SPANISH
	db "DPMI fallo al cambiar a modo protegido.",13,10,"$"
	endif
;
IFDEF PERMNOVM
NoVMSwitch	db 1
ELSE
NoVMSwitch	db 0
ENDIF

VMMDrivPath1	db 128 dup (0)	;used by CAUSEWAY=SWAP:?:\??
VMMDrivPath2	db 128 dup (0)	;used by TEMP=
VMMDrivPath3	db 128 dup (0)	;used by TMP=
VMMDrivPath4	db 128 dup (0)	;used by current path.
VMMDrivPath5	db 128 dup (0)	;used by boot drive.
	db -1
;
DPMIErrRegs	db size RealRegsStruc dup (0)
;
_cwInit	ends


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;End marker so we know what to resize program memory size to initialy.
;
_cwEnd	segment para public 'end marker'
_cwEnd	ends

	end	Startup
