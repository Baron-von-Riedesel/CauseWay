;
;CauseWay v2 main file.
;
;
;Things to note:
;
;The IDT has a page to itself even though it only needs 2k. For RAW/VCPI systems
;the top 2k could be used for something else (the TSS is using it now).
;
;there are some DOS memory blocks allocated:
; 1. PSP + cw segments _cwMain, _cwStack, Raw/VCPI: _cwRaw
; 2. environment
; 3. stacks; Raw/VCPI: size 2400h (=RawStackTotal); DPMI: 1200h+10h
; 4. Raw/VCPI: page tables (12k+x), later reused: 8k for transfer buffer, 4k for VMM buffer 
;
        option proc:private

        include equates.inc
        include strucs.inc
        include cw.inc

ifndef SPANISH
SPANISH equ 0
endif
ifndef ENGLISH
ENGLISH equ not SPANISH and 1
endif

b       equ <byte ptr>
w       equ <word ptr>
d       equ <dword ptr>
;f       equ <fword ptr>

PT_RAWXMS equ 0
PT_VCPI   equ 1
PT_DPMI   equ 2

PTMAPADDR   equ 0FFC00000h ;=1024*4096*1023, last page directory entry
PDEMAPDET   equ 1022       ;entry in page dir, address range FF800000-FFBFFFFF 
MAINSTKSIZE equ 2048       ;stack size of PL3 kernal (segment _cwStack)

SMARTRMSTKALLOC  equ 1	;1=if kernel stack couldn't be alloc'd in an UMB, it will use space behind _cwRaw
MOVEPAGE1STTOEXT equ 1	;1=move page table for region 0-3fffff to extended memory
MOVETSS          equ 1	;1=move TSS to extended memoy (behind IDT)
RELXMSINRM       equ 1	;1=release xms memory handles after final switch to real-mode
VCPIPMCALL       equ 1	;1=alloc/release vcpi pages via protected-mode VCPI call
ifndef EARLYKDINIT
EARLYKDINIT      equ 1	;1=init KD very early after switch to protected-mode
endif

GROUP16 group _cwMain, _cwRaw, _cwInit
GROUP32 group _TEXT32, _DATA32

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Main code segment. This takes care of calling the right initialisation routines
;and generaly getting everything rolling.
;
_cwMain segment para public 'Main code' use16
;
;Want a copyright message embedded first.
;
Copyright       label byte
        db 'CauseWay DOS Extender v'
VersionMajor    db '5.'
VersionMinor    db '00'
        db " No copyright. Public domain software.",13,10,"No rights retained. ",13,10,0
SizeCopyright equ $ - offset Copyright - 1

	align 2

;-------------------------------------------------------------------------------
;
;Some global data.
;
RealPSPSegment  dw ?            ;Real mode PSP segment.
RealENVSegment  dw ?            ;Real mode environment segment.
ProtectedFlags  dw 0            ;Bit significant, 0-DPMI,1-VCPI,2-RAW.
ProtectedType   dw 0            ;0-RAW/XMS,1-VCPI,2-DPMI.
ProtectedForce  db 0            ;CAUSEWAY environment setting "DPMI"
	align 2
DOSVersion      dw 0
SystemFlags     dd 0
grp32Proc label fword
grp32Ofs        dd 0
Group32CS       dw 0            ;code selector GROUP32
Group32DS       dw 0            ;data selector GROUP32
;
CodeSegment     dw MainCS       ;CS selector for GROUP16.
DataSegment     dw MainDS       ;DS selector for GROUP16
StackSegment    dw MainSS       ;that's _cwStack, the stack for PL3 protected-mode
RealSegment     dw KernalZero   ;4G zero-based data selector
PSPSegment      dw MainPSP
ENVSegment      dw MainEnv
BasePSP         dw 0
BasePSPAddress  dd 0
;
TSRSize dw 0
;
ErrorNumber     dw 0
ErrorLevel      dw 0
ErrorList       dw ErrorM00,ErrorM01,ErrorM02,ErrorM03,ErrorM04,ErrorM05,ErrorM06,ErrorM07
        dw ErrorM08,ErrorM09,ErrorM10,ErrorM11,ErrorM12,ErrorM13,ErrorM14,ErrorM15
        dw ErrorM16
ErrorM00        db 'CauseWay error '
ErrorM00n       db '00 : $'
ErrorM01        label byte
        if ENGLISH
        db 'Unable to re-size program memory block.',13,10,'$'
        elseif SPANISH
        db "Incapaz de redimensionar el bloque de memoria del programa.",13,10,"$"
        endif
ErrorM02        db "$"
ErrorM03        db "$"
ErrorM04        db "$"
ErrorM05        label byte
        if ENGLISH
        db 'Not enough memory for CauseWay.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para CauseWay.",13,10,"$"
        endif
ErrorM06        db "$"
ErrorM07        db "$"
ErrorM08        db "$"
ErrorM09        label byte
        if ENGLISH
        db 'Unrecoverable exception. Program terminated.',13,10,'$'
        elseif SPANISH
        db "Excepcion irrecuperable. Programa terminado.",13,10,"$"
        endif
ErrorM10        label byte
        if ENGLISH
        db 'Unable to find '
ErrorM10_T      db 'application to load.',13,10,'$',32 dup (0)
        elseif SPANISH
        db "Incapaz de encontrar '
ErrorM10_T      db 'aplicacion a cargar.",13,10,"$",32 dup (0)
        endif
ErrorM11        label byte
        if ENGLISH
        db 'DOS reported an error or corrupt file found.'
        elseif SPANISH
        db "DOS informo de un error o de un fichero corrupto."
        endif
ErrorM11_0      db 13,10,'$'
        db "No:"
ErrorM11_1      db "00000000",13,10,"$"
ErrorM12        label byte
        if ENGLISH
        db 'Not enough memory to load application.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para cargar la aplicacion.",13,10,"$"
        endif
ErrorM13        db "$"
ErrorM14        label byte
        if ENGLISH
        db 'Memory structures destroyed. Program terminated.',13,10,'$'
        elseif SPANISH
        db "Estructuras de memoria destruidas. Programa terminado.",13,10,"$"
        endif
ErrorM15        label byte
        if ENGLISH
        db 'DOS reported an error while accessing swap file. Program terminated.',13,10,'$'
        elseif SPANISH
        db "DOS informa de un error mientras se accede al fichero de swap. Programa terminado.",13,10,"$"
        endif
ErrorM16        label byte
        if ENGLISH
        db "Unsupported DOS function call, program terminated.",13,10,"$"
        elseif SPANISH
        db "Llamada a funcion no soportada del DOS, programa terminado.",13,10,"$"
        endif
;
ALIGN 4
MainExec        db 128 dup (0)
;
DtaBuffer       db 128 dup (0)
;
;TransferSize    dd 8192
TransferSize    equ 8192
TransferReal    dw ?       ; 8k transfer buffer real-mode segment address
;
MouseETarget    dd 0,0
ResourceTracking dw 0
ForcedFind      dd 0,0
mcbAllocations  dw 0
LinearAddressCheck db 0    ; for swapfile
;
	align 4
TerminationHandler label fword
	dd offset InitError
	dw MainCS, 0

;--- segment _Excep is now 32-bit; this requires a proper definition of a 16-bit far pointer
;--- called from within a 32-bit code segment.
PF16 typedef far16 ptr

UserTermRoutine16 label PF16
UserTermRoutine DF 0       ; user termination address ( int 31h, ax=ff31h )
UserTermDump    DF 0       ; dump location for register info ( int 31h, ax=ff31h )
;
DPMIStateAddr   df 0
DPMIStateSize   dd 0
DPMIStackOff    dw ?       ; current offset of Main stack in DPMI mode
DPMIStackSeg    dw ?       ; Main stack in DPMI mode (size 1200h)
;
DebugDump       db 0       ; flag to display a state dump on exit
EnableDebugDump DB 1       ; may be set/reset by int 31h, ax=0ff30h
UserTermFlag    DB 0       ; modified by int 31h, ax=0ff31h (set user termination proc)
Pad1Flag        DB 0       ; CAUSEWAY environment setting "PAD1"

IFDEF PERMNOEX
NoEXECPatchFlag DB 1       ; CAUSEWAY environment setting "NOEX"
ELSE
NoEXECPatchFlag DB 0
ENDIF

; MED, 11/11/99
; used to flag checking XMS total memory because EMM386 lies and acts as VCPI
;  host when NOVCPI set, but provides no memory
VCPIHasNoMem    DB      0

NewCWErrName    DB      81 DUP (0)    ; error file name set with cw function 0xff32
DOS4GFlag       db 0
	align 4
;
Int21Buffer     RealRegsStruc <?>
Int10Buffer     RealRegsStruc <?>
Int33Buffer     RealRegsStruc <?>
;
apiExcepPatched db 0
;
	align 2
ExtensionList   label word
        dw ExceptionExtension
        dw Int21hExtension,Int10hExtension,Int33hExtension
ExtensionListEnd label word

EXTENSION struct
pInit   df ?            ;+4  init code.
pExit   df ?            ;+10 remove code.
wFlgs   dw ?            ;+20 installed flag.
EXTENSION ends

;
ExceptionExtension EXTENSION < offset ExcepOpen,  offset ExcepClose,  0>
Int21hExtension    EXTENSION < offset Int21hOpen, offset Int21hClose, 0>
Int10hExtension    EXTENSION < offset Int10hOpen, offset Int10hClose, 0>
Int33hExtension    EXTENSION < offset Int33hOpen, offset Int33hClose, 0>
;
;Temp0_  dd ?
;Temp1_  dd ?
;
DebugUserOff    DD      ?
DebugUserSel    DW      ?
DebugUserCount  DW      0       ; must be initialized, nonzero value flags operation
DebugAsciiFlag  DB      ?

        .386
;-------------------------------------------------------------------------------
;
;Final init stuff.
;
cwOpen  proc    near
        mov     ds,cs:DataSegment
        assume ds:GROUP16
        ;
        mov     d[TerminationHandler+0],offset cwClose
        mov     w[TerminationHandler+4],cs
;
;Now we know the machines details, re-size the program
;block again to release as much memory as possible.
;
        mov     edi,offset Int21Buffer  ;Int21Buffer is 16-bit, but hiword(edi) must be cleared
        push    ds
        pop     es
        mov     ax,RealPSPSegment
        mov     es:RealRegsStruc.Real_ES[di],ax
        mov     bx,_cwRaw                ;DPMI: just keep _cwMain, _cwStack
        cmp     ProtectedType,PT_DPMI
        jz      cw1_KeepStack
        mov     bx,_cwInit               ;Raw/VCPI: keep _cwMain, _cwStack, _cwRaw
if SMARTRMSTKALLOC
        cmp     RawStackReal, _cwInit    ;could main stack be allocated in an UMB?
        jnz     @F
        mov     RawStackPos, RawStackTotal ;if no, stack is in _cwInit, but too small yet, so
        add     bx,RawStackTotal/16        ;increase size to normal.
@@:
 if 0 ; activate for debugging ( clears the stack space with -1 )
        mov cx, _cwInit
        mov dx, _cwMain
        sub cx, dx
        shl cx, 4   ; cx=offset for _cwInit in GROUP16
        push di
        push eax
        mov di, cx
        mov cx, RawStackTotal / 4
        or eax, -1
        rep stosd
        pop eax
        pop di
 endif
endif
cw1_KeepStack:
        sub     bx,ax                   ;Size program.
        inc     bx
        mov     TSRSize,bx
        mov     w es:[di].RealRegsStruc.Real_EBX,bx
        mov     w es:[di].RealRegsStruc.Real_EAX,4a00h
        mov     bl,21h
        mov     ErrorNumber,1
        Sys     IntXX
        test    es:[di].RealRegsStruc.Real_Flags,1
        jnz     cw1_9
        mov     ErrorNumber,0           ;clear error number.
;
;Force accurate memory values.
;
        or      ecx,-1
        Sys     GetMemLinear32
;
;Enable resource tracking and MCB allocations.
;
        or      ResourceTracking,-1     ;Enable resource tracking.
;
;Run the main program.
;
        mov     edx,offset MainExec     ;name to exec.
        mov     esi,80h
        mov     es,PSPSegment
        xor     cx,cx
        Sys     cwExec                  ;run the bugger.
        jnc     cw1_8
        add     ax,10-1                 ;convert error number.
        mov     ErrorNumber,ax
        jmp     cw1_9
cw1_8:  cmp     DebugDump,0
        jnz     cw1_9
        mov     ErrorLevel,ax           ;store programs error level.
        mov     ErrorNumber,0           ;clear error number.
cw1_9:  jmp     cwClose
cwOpen  endp


;-------------------------------------------------------------------------------
;
;Shut everything down.
;
cwClose proc    near

        mov     ds,cs:DataSegment
        assume ds:GROUP16
        ;
        mov     ResourceTracking,0
        mov     mcbAllocations,0
        sti
        mov     w[TerminationHandler+4],0
        ;

;--- DebugDisplay is in segment _Excep, which is now 32-bit

        pushw   0
        push    cs
        pushd   offset cw2_dd0
        push    d[ExceptionExtension.pInit+4]
        push    offset DebugDisplay
        retd
        ;
cw2_dd0:
        cmp     ErrorNumber,0
        jz      cw2_NoError

        mov     ax,ErrorNumber          ;Get the error number.
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[ErrorM00n],al
        add     dl,'0'
        mov     b[ErrorM00n+1],dl

        cmp     EnableDebugDump,0       ; if debug dump turned off, no screen i/o
        je      cw2_NoError

        xor     edx,edx
        mov     dx,w[ErrorList]
        mov     ah,9
        int     21h
;
;Get a pointer to the appropriate error message and print it.
;
        mov     bx,ErrorNumber
        add     bx,bx
        xor     edx,edx
        mov     dx,[ErrorList+bx]
        mov     ah,9
        int     21h
;
;Now exit with the error number as the DOS "errorlevel".
;
cw2_NoError:
;
;Remove extension patches.
;
        mov     di,offset ExtensionListEnd-2
cw2_p0:
        push    di
        mov     di,[di]
        cmp     [di].EXTENSION.wFlgs,-1  ;installed?
        jnz     cw2_p2
        call    [di].EXTENSION.pExit
cw2_p2:
        pop     di
        sub     di,2
        cmp     di,offset ExtensionList-2
        jnz     cw2_p0
;
;Remove api exception patches.
;
        cmp     apiExcepPatched,0
        jz      cw2_pe0
        mov     grp32Ofs, offset UnPatchExc
        call    [grp32Proc]
cw2_pe0:
;
;Remove the API patch.
;
        mov     es,Group32DS
        assume es:GROUP32
        mov     edx,DWORD PTR es:[OldInt31+0]
        mov     cx,WORD PTR es:[OldInt31+4]
        mov     bl,31h
        mov     ax,205h
        int     31h
        mov     DWORD PTR es:[cwIdentity+0],0
        mov     DWORD PTR es:[cwIdentity+4],0
        assume es:nothing
        ;
cw2_noAPI:
        cmp     ProtectedType,PT_DPMI
        jz      cw2_DPMI
;
;Make RAW stuff addressable.
;
        cli                             ;Don't want interrupts interfering.
        mov     ax,KernalDS             ;Get supervisor data descriptor,
        mov     ds,ax                   ;DS,ES,FS,GS,SS must be data with 64k limit
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,ax
;
;Switch to RAW exit code.
;
        push    GROUP16                 ; push a real-mode segment as return address!
        push    offset cw2_InRealMode
        push    KernalCS
        push    offset RawVCPIRealMode
        retf
;
;Remove DPMI stuff.
;
cw2_DPMI:
if 0
        cmp     [OldInt21hExec],0
        jz      cw2_d0
        mov     bl,21h
        mov     dx,w[OldInt21hExec+0]
        mov     cx,w[OldInt21hExec+2]
        mov     ax,201h
        int     31h
cw2_d0:
endif
;
;Display the "CauseWay error: ??" bit.
;
cw2_InRealMode:

if 0
        cmp     ErrorNumber,0
        jz      cw2_NoError

        push    ax
        mov     al,3
        call    bordm
        pop     ax

        mov     ax,ErrorNumber          ;Get the error number.
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[ErrorM00n],al
        add     dl,'0'
        mov     b[ErrorM00n+1],dl

        cmp     EnableDebugDump,0       ; if debug dump turned off, no screen i/o
        je      cw2_NoError

        xor     edx,edx
        mov     dx,w[ErrorList]
        mov     ah,9
        int     21h
;
;Get a pointer to the appropriate error message and print it.
;
        mov     bx,ErrorNumber
        add     bx,bx
        mov     dx,[ErrorList+bx]
        mov     ah,9
        int     21h
;
;Now exit with the error number as the DOS "errorlevel".
;
cw2_NoError:
endif

; MED, 12/24/99, coalesce free memory by attempting to allocate largest possible
;  with upper memory in the chain
        mov     ax,5800h
        int     21h
        push    ax
        mov     ax,5802h
        int     21h
        push    ax
        mov     bx,1
        mov     ax,5803h
        int     21h
        mov     bx,81h
        mov     ax,5801h
        int     21h

        mov     ah,48h
        mov     bx,-1
        int     21h

        pop     bx
        mov     ax,5803h
        int     21h
        pop     bx
        mov     ax,5801h
        int     21h

        mov     ax,ErrorNumber
        or      ax,ax
        jnz     cw2_Exit
        mov     ax,ErrorLevel
cw2_Exit:

        mov     ah,4ch
        int     21h
cwClose endp


;-------------------------------------------------------------------------------
;--- int 21h real-mode handler, installed by int21h.inc extension

Int21hExecPatch proc    near
        assume ds:nothing
        pushf
        cmp     ax,4b00h
        jnz     cw3_Old

        inc     cs:Int21hExecCount
        popf
        pushf
        call    cs:[OldInt21hExec]   ; real-mode FAR16 address
        dec     cs:Int21hExecCount   ; INC/DEC don't affect CF
        push bp
        mov bp,sp
        jc @F
        and w[bp+2+2+2],not 1
        pop bp
        iret
@@:
        or w[bp+2+2+2],1
        pop bp
        iret
cw3_Old:
        popf
        jmp     cs:[OldInt21hExec]

	align 2
OldInt21hExec   dd 0        ; real-mode address
Int21hExecCount db 0

        assume ds:GROUP16

Int21hExecPatch endp

        .386p

if 0
;-------------------------------------------------------------------------------
Bordm   proc    near
        push    ax
        push    dx
        mov     ah,al
        mov     dx,3dah
        in      al,dx
        mov     dl,0c0h
        mov     al,11h
        out     dx,al
        mov     al,ah
        out     dx,al
        mov     al,20h
        out     dx,al
        pop     dx
        pop     ax
        ret
Bordm   endp
endif

_cwMain ends

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Default stack (selector MainSS) used during startup and exit.
;
_cwStack        segment para stack 'stack' use16
        db MAINSTKSIZE dup (?)
_cwStackEnd     label byte
_cwStack        ends


        include rawvcpi1.inc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;The initialiseation code seg. Takes care of things like checking for the right
;processor and determining how we're going to get into protected mode.
;
_cwInit segment para public 'init code' use16
        assume ds:GROUP16

dpmiSelBuffer   db 8 dup (0)

;-------------------------------------------------------------------------------
;
;Initialisation specific data.
;
dpmiSelBase     dd 0
CurrPhysPage    dd ?
GDTReal         dw ?            ;Real mode segment for GDT.
;IDTReal         dw ?            ;Real mode segment for IDT.
Page1stReal     dw ?            ;Real mode segment for 1st page table entry.
PageDIRReal     dw ?            ;Real mode segment for page directory: later used as first 4k of transfer buffer.
PageAliasReal   dw ?            ;Real mode segment for page table alias; later used as second 4k of transfer buffer.
KernalTSSReal   dw ?            ;Real mode segment for kernal TSS.

LowMemory label dword         ; Set equal to 0000:0080
                dw 00080h
                dw 00000h
HighMemory label dword
                dw 00090h     ; Set equal to FFFF:0090
                dw 0FFFFh
;
INewHeader      NewHeaderStruc <>   ;make space for a header.
;DPMIErrRegs     RealRegsStruc <>

;--- there's also a MZHeader struct in api.inc, size 64!
MZHdr struct
Signature	dw ?	;00 Identifier text 'MZ', '3P'.
_Length		dw ?	;02 Length of file MOD 512
			dw ?	;04 Length of file in 512 byte blocks.
RelocNum	dw ?	;06 Number of relocation items.
HeaderSize	dw ?	;08 Length of header in 16 byte paragraphs.
MinAlloc	dw ?	;0A Minimum number of para's needed above program.
MaxAlloc	dw ?	;0C Maximum number of para's needed above program.
StackSeg	dw ?	;0E Segment displacement of stack module.
EntrySP		dw ?	;10 value for SP at entry.
CheckSum	dw ?	;12 Check sum...
EntryIP		dw ?	;14 Contents of IP at entry.
EntryCS		dw ?	;16 Segment displacement of CS at entry.
RelocFirst	dw ?	;18 First relocation item offset.
OverlayNum	db ?	;1A Overlay number.
MZHdr ends

IExeHdr MZHdr <>

	align 2
;
IErrorNumber    dw 0
InitErrorList   dw IErrorM00,IErrorM01,IErrorM02,IErrorM03,IErrorM04,IErrorM05,IErrorM06,IErrorM07
        dw IErrorM08,IErrorM09
IErrorM00       db 'CauseWay error '
IErrorM00n      db '00 : $'
IErrorM01       label byte
        if ENGLISH
        db 'Unable to re-size program memory block.',13,10,'$'
        elseif SPANISH
        db "Incapaz de redimensionar el bloque de memoria del programa",13,10,"$"
        endif
IErrorM02       label byte
        if ENGLISH
        db '386 or better required.',13,10,'$'
        elseif SPANISH
        db "Se requiere un 386 o superior.",13,10,"$"
        endif
IErrorM03       label byte
        if ENGLISH
        db 'Non-standard protected mode program already active.',13,10,'$'
        elseif SPANISH
        db "Programa en modo protegido no estandar activado.",13,10,"$"
        endif
IErrorM04       label byte
        if ENGLISH
        db 'DOS 3.1 or better required.',13,10,'$'
        elseif SPANISH
        db "DOS 3.1 o superior requerido.",13,10,"$"
        endif
IErrorM05       label byte
        if ENGLISH
        db 'Not enough memory for CauseWay.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para CauseWay.",13,10,"$"
        endif
IErrorM06       label byte
        if ENGLISH
        db 'VCPI failed to switch into protected mode.',13,10,'$'
        elseif SPANISH
        db "VCPI fallo al cambiar a modo protegido.",13,10,"$"
        endif
IErrorM07       label byte
        if ENGLISH
        db 'Unable to control A20.',13,10,'$'
        elseif SPANISH
        db "Capaz de controlar A20.",13,10,"$"
        endif
IErrorM08       label byte
        if ENGLISH
        db 'Selector allocation error.',13,10,'$'
        elseif SPANISH
        db "Error de asignacion del selector.",13,10,"$"
        endif
IErrorM09       label byte
        if ENGLISH
        db 'DPMI failed to switch to protected mode.',13,10,'$'
        elseif SPANISH
        db "DPMI fallo al cambiar a modo protegido.",13,10,"$"
        endif
;
IFDEF PERMNOVM
NoVMSwitch      db 1
ELSE
NoVMSwitch      db 0
ENDIF

VMMDrivPath1    db 128 dup (0)  ;used by CAUSEWAY=SWAP:?:\??
VMMDrivPath2    db 128 dup (0)  ;used by TEMP=
VMMDrivPath3    db 128 dup (0)  ;used by TMP=
VMMDrivPath4    db 128 dup (0)  ;used by current path.
VMMDrivPath5    db 128 dup (0)  ;used by boot drive.
                db -1           ;end marker

	align 2

cw5_OldStrat      dw ?,?
cw5_XMSSize       dd 0
wUMB              dw 0          ;UMB used 1) for paging tables 2) for transfer buffer + swapfile buffer
cw5_NumXMSHandles db ?
IProtectedMode    db 0

;
;
;-------------------------------------------------------------------------------
Startup proc    near
;
;Make global data addresable.
;
        .286
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16

; MED, 12/30/99
; works around weird bug in some machines due to Windows/hardware/BIOS??? error
;  where a CauseWay application in AUTOEXEC.BAT which uses DOS function 8
;  to get a keystroke when no other application in AUTOEXEC.BAT gets a
;  keystroke will cause failure in Windows 98 (and 95?) when opening a DOS
;  box after Windows startup.  Caused by A20 or keyboard port stabilization?
IFNDEF SUN
        mov     cx,127
kloop:
        mov     ax,0b00h
        int     21h
        test    al,al
        loope   kloop
ELSE
        mov     ax,3
        int     10h
ENDIF

;
;Stow real mode PSP and environment values, we'll need them later.
;
        mov     RealPSPSegment,es
        mov     ax,WORD PTR es:[02ch]
        mov     RealENVSegment,ax       ;Stow ENV for later.
;
;Re-size memory so we can allocate what we want high.
;
        mov     IErrorNumber,1
        mov     ax,es
        mov     bx,_cwEnd               ;Get program end segment.
        sub     bx,ax                   ;Size program.
        mov     ah,4ah
        int     21h                     ;Re-size memory block.

; MED 06/16/97
;       jc      InitError
        jnc     chk386

toiniterr:
        jmp     InitError

;
;Check we're on at least a 386 system.
;
chk386:
        mov     IErrorNumber,2
        call    CheckProcessor

;       jc      InitError
        jc      toiniterr

        .386
;
;Check DOS version is high enough.
;
        mov     IErrorNumber,4
        call    CheckDOSVersion
        jc      InitError
;
;Get execution name from environment.
;
        call    GetEXECName
;
;Retrieve setup info from 3P header.
;
        call    GetSystemFlags
;
;Check if a suitable method for switching to protected mode exists.
;
        call    GetProtectedType        ;set variable ProtectedFlags
        mov     IErrorNumber,3
        cmp     ProtectedFlags,0        ;Any types available?
        jz      InitError
;
;Get CAUSEWAY environment variable settings.
;
        call    GetENVStuff
;
;Decide which environment to use.
;
        call    SetProtectedType
;
;Move the DTA to where we can get at it in the future.
;
        mov     dx,offset DtaBuffer
        mov     ah,1ah
        int     21h
;
;Change DOS allocation stratergy to highest so we'll get UMB's if available.
;
        mov     ax,5800h                ;get allocation strategy
        int     21h
        mov     [cw5_OldStrat+0],ax
        mov     ax,5802h                ;get UMB link state
        int     21h
        mov     [cw5_OldStrat+2],ax
        mov     bx,1
        mov     ax,5803h                ;set UMB link state
        int     21h
        mov     bx,81h                  ;best fit, UMB first, then low
        mov     ax,5801h                ;set allocation strategy
        int     21h
;
;now see about type specific initialisations.
;
        cmp     ProtectedType,PT_DPMI
        jz      cw5_InitDPMI
;
;Get SDA address so VMM can change BREAK state.
;
        push    ds
        mov     ax,5d06h
        int     21h
        mov     ax,ds
        pop     ds
        add     si,17h
        movzx   eax,ax
        shl     eax,4
        movzx   esi,si
        add     eax,esi
        mov     BreakAddress,eax
;
;Find out if XMS is present.
;
        mov     ax,4300h                ;XMS install check.
        int     2fh
        cmp     al,80h                  ;XMS present?
        jnz     cw5_NoXMS
;
;XMS detected so work out max block size and entry point.
;
        mov     ax,4310h                ;Get XMS API.
        int     2fh
        mov     w[XMSControl+0],bx
        mov     w[XMSControl+2],es
        mov     XMSPresent,1            ;flag XMS is available.

; MED, 09/10/99, support extended XMS API to calculate XMS available to CauseWay
;  (maximum of 2G-32K, i.e. 32 handles/entries of 64K-1)
        mov     ah,0
        call    [XMSControl]            ; get info
        cmp     ah,3
        jb      xms2
        cmp     bh,3
        jb      xms2
        cmp     bl,8
        jb      xms2                    ; treat early 3.x drivers < 3.08 as 2.x

; use extended XMS API
        mov     XMSVer3Present,1        ; flag XMS 3.x driver present
        mov     ah,88h
        call    [XMSControl]            ;get size of biggest block free.
        mov     edx,eax
        test    eax,eax
        jz      cw5_YesXMS              ;no memory available.
        mov     [cw5_XMSSize],edx
        mov     ah,89h
        call    [XMSControl]            ;claim biggest block to force XMS
        cmp     ax,1                    ;to stake a claim on int 15h.
        jnz     cw5_YesXMS
        mov     [cw5_NumXMSHandles],1
        push    dx
        mov     ah,8eh
        call    [XMSControl]            ; get handle information
        cmp     ax,1
        jnz     cw5_NoHandles3
        cmp     cx,4
        jc      cw5_NoHandles3
        sub     cx,2
        mov     [cw5_NumXMSHandles],32
        cmp     cx,32
        jnc     cw5_NoHandles3
        mov     [cw5_NumXMSHandles],cl  ; cx known 8-bit value

cw5_NoHandles3:
        pop     dx
        mov     ah,0ah
        call    [XMSControl]            ;now free it.

        movzx   eax,[cw5_NumXMSHandles]
        mov     edx,eax
        shl     eax,16
        sub     eax,edx                 ; eax == handles (up to 32) * 65535
        cmp     eax,[cw5_XMSSize]
        jae     cw5_ComputeSize
        mov     [cw5_XMSSize],eax    ; throttle maximum size

cw5_ComputeSize:
        push    eax
        xor     edx,edx
        movzx   ebx,[cw5_NumXMSHandles]
        div     ebx
        pop     ebx
        cmp     ax,4                    ; eax known 16-bit value
        jnc     cw5_SizeOK3
        mov     ax,bx                   ; ebx == maximum size, known 16-bit value here

cw5_SizeOK3:
        mov     XMSBlockSize,ax
        jmp     cw5_YesXMS

xms2:
        mov     ah,8
        call    [XMSControl]            ;get size of biggest block free.
        mov     dx,ax
        or      ax,ax
        jz      cw5_YesXMS              ;no memory available.
        mov     WORD PTR [cw5_XMSSize],dx
        mov     ah,9
        call    [XMSControl]            ;claim biggest block to force XMS
        cmp     ax,1                    ;to stake a claim on int 15h.
        jnz     cw5_YesXMS
        mov     [cw5_NumXMSHandles],1
        push    dx
        mov     ah,0eh
        call    [XMSControl]            ;now free it.
        cmp     ax,1
        jnz     cw5_NoHandles
        cmp     bl,4
        jc      cw5_NoHandles
        sub     bl,2
        mov     [cw5_NumXMSHandles],32
        cmp     bl,32
        jnc     cw5_NoHandles
        mov     [cw5_NumXMSHandles],bl
cw5_NoHandles:
        pop     dx
        mov     ah,0ah
        call    [XMSControl]            ;now free it.
        mov     ax,WORD PTR [cw5_XMSSize]
        push    ax
        xor     dx,dx
        xor     bh,bh
        mov     bl,[cw5_NumXMSHandles]
        div     bx
        pop     bx
        cmp     ax,4
        jnc     cw5_SizeOK
        mov     ax,bx
cw5_SizeOK:
        mov     XMSBlockSize,ax
        jmp     cw5_YesXMS
;
;Install raw A20 handler.
;
cw5_NoXMS:
        call    InstallA20
;
;Get A20 state.
;
cw5_YesXMS:
        push    ds
        les     di,HighMemory           ;   with the four at FFFF:0090
        lds     si,LowMemory            ; Compare the four words at 0000:0080
        mov     cx,4
        cld
        repe  cmpsw
        pop     ds
        xor     ax,ax
        jcxz    cw5_A20OFF              ; Are the two areas the same?
        inc     ax                      ; No, return A20 Enabled
cw5_A20OFF:
        mov     A20Flag,al
;
;Grab memory for page dir, page alias & first page table entry.
;
        mov     bx,(4096*3)/16          ;smallest allocation possible.
        mov     ah,48h
        int     21h
        jc      cw5_OldWay
        push    ax
        movzx   eax,ax
        shl     eax,4                   ;linear address.
        mov     ebx,eax
        add     eax,4096-1
        and     ax,0f000h               ;round up to next page.
        sub     eax,ebx
        shr     eax,4
        mov     bx,ax
        mov     cx,ax
        add     bx,(4096*3)/16
        pop     es

        push    bx
        push    cx
        mov     ah,4ah
        int     21h                     ;re-size the block.
        pop     cx
        pop     bx
        jnc     cw5_NewWay
        mov     ah,49h
        int     21h                     ;release this block.
        ;
cw5_OldWay:
        mov     IErrorNumber,5
        mov     bx,(4096*4)/16          ;need space for 3 page tables on
        mov     ah,48h                  ;4k boundary.
        int     21h
        jc      InitError
        mov     dx,ax
        movzx   eax,ax                  ;get segment address.
        shl     eax,4                   ;make linear.
        mov     ebx,eax
        add     eax,4096-1              ;round up to next page.
        and     ax,0F000h
        mov     ecx,eax
        sub     ecx,ebx
        shr     ecx,4
        shr     eax,4                   ;Get segment value again.
        jmp     cw5_GotSeg
        ;
cw5_NewWay:
        mov     ax,es
        mov     dx,ax
        add     ax,cx                   ;move to real start.
        ;
cw5_GotSeg:
;--- dx=segment addr of block
        mov     wUMB,dx
        push    cx
        push    dx
        mov     es,ax

        mov     Page1stReal,ax          ;setup 1st page table address.
        add     ax,4096/16
        mov     PageDIRReal,ax          ;setup page directory address.
        add     ax,4096/16
        mov     PageAliasReal,ax        ;setup alias table address.
        xor     di,di
        mov     cx,4096*3
        xor     al,al
        cld
        rep     stosb                   ;clear it.
        movzx   eax,PageDIRReal
        shl     eax,4
        mov     PageDirLinear,eax
        mov     vcpi._CR3,eax
        movzx   eax,PageAliasReal
        shl     eax,4
        mov     PageAliasLinear,eax
        movzx   eax,Page1stReal
        shl     eax,4
        mov     Page1stLinear,eax
        pop     dx
        pop     cx
;
;See if enough wasted space to squeeze TSS into.
;
        cmp     cx,(((size TSSFields)+2+16)/16)
        jc      cw5_TSSOld
        mov     ax,dx                   ;get segment.
        add     dx,(((size TSSFields)+2+16)/16) ;move segment base.
        sub     cx,(((size TSSFields)+2+16)/16) ;update space left size.
        jmp     cw5_TSSGot
;
;Allocate memory for Kernal TSS.
;
cw5_TSSOld:
        mov     IErrorNumber,5
        mov     bx,(((size TSSFields)+2+15)/16) ;(4096/2)+2+16)/16
        mov     ah,48h
        int     21h
        jc      InitError
cw5_TSSGot:
        mov     KernalTSSReal,ax
;
;See if enough wasted space to squeeze GDT into.
;
        cmp     cx,((8*GDT_Entries)/16)+1
        jc      cw5_GDTOld
        mov     ax,dx                       ;get segment.
        add     dx,((8*GDT_Entries)/16)+1   ;move segment base.
        sub     cx,((8*GDT_Entries)/16)+1   ;update space left size.
        jmp     cw5_GDTGot
;
;Allocate some memory for the GDT.
;
cw5_GDTOld:
        mov     IErrorNumber,5
        mov     bx,((8*GDT_Entries)/16)+1
        mov     ah,48h
        int     21h
        jc      InitError
cw5_GDTGot:
        mov     GDTReal,ax
        mov     es,ax
        movzx   eax,ax
        shl     eax,4
        mov     d GDTVal+2,eax
        xor     di,di
        mov     cx,(8*GDT_Entries)
        xor     al,al
        cld
        rep     stosb
;
;Allocate some memory for the stack.
;
        mov     IErrorNumber,5
;        mov     ebx,RawStackPos
;        shr     ebx,4
        mov     bx,RawStackTotal/16
        mov     ah,48h
        int     21h
        jc      InitError
        mov     es,ax
if SMARTRMSTKALLOC
;--- if allocation is in low memory, skip it
;--- and use part of _cwInit as stack.
        cmp ax, 0A000h
        jae @F
        mov ah, 49h
        int 21h
        mov RawStackReal, _cwInit
        mov ax, offset _cwInit:cw_safesp
        and ax, 0fffch
        mov w RawStackPos, ax
        jmp nostack
@@:
endif        
        mov     RawStackReal,ax
;        mov     cx,w[RawStackPos]
        mov     cx,RawStackTotal
        xor     di,di
        xor     al,al
        cld
        rep     stosb
nostack:

; MED 09/19/96
; Set address for VMM page to disk buffer.
if 0
        mov     IErrorNumber,5
        mov     bx,4096/16
        mov     ah,48h
        int     21h
        jc      InitError
else
        mov     ax,wUMB                 ; use the first 4k of the UMB for swapfile access
endif
        mov     PageBufferReal,ax
        movzx   eax,ax
        shl     eax,4
        mov     PageBufferLinear,eax
;
;Need to initialise 1st page table to map <1meg+64k 1:1.
;
        mov     es,Page1stReal
        xor     di,di
        mov     cx,256+16               ;1st 1 meg + 64k.
        mov     eax,111b                ;user+write+present
        cld
cw5_0:
        stosd                   
        add     eax,4096                ;next physical page address.
        loop    cw5_0

;
;Set address for DOS INT 21h PM to Real transfer buffer.
;

; MED 09/19/96
;       mov     bx,Page1stReal
;        mov     bx,PageDIRReal
        mov     bx,wUMB
        add     bx,100h                 ;skip the swapfile buffer
        mov     TransferReal,bx
;
;setup the GDT entries.
;
        mov     es,GDTReal
;
;Fill in the null entry just for the hell of it.
;
        xor     esi,esi                 ;Null entry at 0.
        xor     ecx,ecx
        xor     ax,ax
        xor     di,di
        call    MakeDesc
;
;Fill in the VCPI entries so we don't try to use them later.
;most likely not needed, since descriptors 00,08,10h,18h are off-limits.
;
if 0
        xor     esi,esi
        xor     ecx,ecx
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,VCPI_0
        call    MakeDesc
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,VCPI_1
        call    MakeDesc
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,VCPI_2
        call    MakeDesc
endif
;
;Allocate 40h descriptor.
;
        mov     esi,400h
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,Kernal40h
        call    MakeDesc
;
;Allocate kernal task descriptors, TSS itself first.
;
        movzx   esi,KernalTSSReal
        shl     esi,4
        mov     ecx,size TSSFields+2-1  ; uses an extra word for end of IOPB, but this is used only if IOPL=0
        xor     al,al
        mov     ah,DescPresent+DescPL3+Desc386Tss
        mov     di,KernalTS
        call    MakeDesc
;
;TSS PL0 stack.
;
;        movzx   esi,KernalTSSReal
;        shl     esi,4
        add     esi,TSSFields.tPL1Stack

;        mov     ecx,65535
        mov     ecx,tPL0StackSize-1
        mov     al,b[RawSystemFlags]
        xor     al,1
        shl     al,6
        mov     ah,DescPresent+DescPL0+DescMemory+DescRWData
        mov     di,KernalSS0
        call    MakeDesc
if 0
;
;Mode switch PL0 stack (must be a 64k - value of SS when switching to real-mode!)
;
;        movzx   esi,KernalTSSReal
;        shl     esi,4
;        add     esi,TSSFields.tPL1Stack
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL0+DescMemory+DescRWData
        mov     di,KernalSS0Switch
        call    MakeDesc
endif
;
;LDT
;
        xor     esi,esi
        xor     ecx,ecx
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescLDT
        mov     di,KernalLDT
        call    MakeDesc
;
;KernalCS0 - Kernel (RAW) code seg at PL0 (must be 64k!)
;KernalCS  - Kernel (RAW) code seg.
;KernalDS  - Kernel (RAW) data seg (must be 64k!).
;
        mov     esi,GROUP16
        shl     esi,4
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,KernalCS0
        call    MakeDesc

        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,KernalCS
        call    MakeDesc

        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,KernalDS
        call    MakeDesc
;
;KernalSS - (RAW) stack seg PL3.
;
        movzx   esi,RawStackReal
        shl     esi,4
;        mov     ecx,[]65535
        mov     ecx,RawStackTotal-1
        mov     al,b[RawSystemFlags]
        xor     al,1
        shl     al,6
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,KernalSS
        call    MakeDesc
;
;Kernal PL3 to PL0 call gate.
;
        xor     ecx,ecx
        mov     esi,KernalCS0
        xor     al,al
        mov     ah,DescPresent+DescPL3+Desc386Call
        mov     di,KernalPL3toPL0
        call    MakeDesc
;
;DPMI emulator code seg.
;DPMI emulator code seg at PL0
;DPMI emulator data seg.
;DPMI emulator PL3 to PL0 call gate.
;--- the emulator code will be moved to extended memory
;
        mov     esi,_cwDPMIEMU
        shl     esi,4
        mov     ecx,cwDPMIEMUEnd - cwDPMIEMUStart - 1
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,DpmiEmuCS
        call    MakeDesc
;
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,DpmiEmuCS0
        call    MakeDesc
;
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,DpmiEmuDS
        call    MakeDesc
;
        xor     ecx,ecx
        mov     esi,DpmiEmuCS0
        xor     al,al
        mov     ah,DescPresent+DescPL3+Desc386Call
        mov     di,DpmiEmuPL3toPL0
        call    MakeDesc
;
;Zero to 4G segment.
;
        xor     esi,esi
        or      ecx,-1
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,KernalZero
        call    MakeDesc
;
;Main PSP segment.
;
        movzx   esi,RealPSPSegment
        shl     esi,4
        mov     ecx,256-1
        mov     al,0
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,MainPSP
        call    MakeDesc
;
;Main environment var.
;
        movzx   esi,RealENVSegment
        shl     esi,4
        mov     ecx,0FFFFh
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,MainEnv
        call    MakeDesc
;
;Main stack.
;
        mov     esi,_cwStack
        shl     esi,4
        mov     ecx,MAINSTKSIZE-1
        mov     al,b[RawSystemFlags]
        xor     al,1
        shl     al,6
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,MainSS
        call    MakeDesc

;--- use the main SS also for mode switches
;--- it needs no space in raw mode, and just a few dwords in vcpi mode
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL0+DescMemory+DescRWData
        mov     di,KernalSS0Switch
        call    MakeDesc
;
;Init GDT data alias.
;
        mov     esi,d GDTVal+2
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,GDTData
        call    MakeDesc
;
;Setup TSS registers to run in protected mode. Setup GDT & IDT values.
;
        mov     es,KernalTSSReal
        xor     di,di
        mov     cx,size TSSFields       ;+(4096/8)
        xor     al,al
        cld
        rep     stosb                   ;clear TSS & IO map.
        or      ax,-1
        stosw                           ;mark end of IO map.
        mov     es:TSSFields.ESP0,tPL0StackSize
        mov     es:TSSFields.SS0,KernalSS0
;        mov     es:TSSFields.tLDT,KernalLDT
        mov     es:TSSFields.IOMap,size TSSFields       ;set displacement to IO table.
;
;Setup GDT load value.
;
        mov     w[GDTVal+0],-1

ifdef KRNLDBG
 if EARLYKDINIT
;for init phase, locate IDT to stack (first 1kb)
        mov     eax, _cwStack
        mov     es, ax
        shl     eax, 4
        mov     w[IDTVal+0], 128*8-1
        mov     d[IDTVal+2], eax
        mov     cx,128
        xor     di,di
        mov     bx, lowword offset InterruptHandler
nextgate:
        mov     ax, bx
        stosw
        mov     ax, DpmiEmuCS0
        stosw
        mov     ax, (DescPresent or DescPL3 or Desc386Int) shl 8
        stosw
        xor     ax, ax
        stosw
        add     bx, INTCALLSIZE
        loop    nextgate
 endif
endif
;
;Setup Windows enhanced mode denial patch.
;
        mov     ax,352fh                ;get existing vector.
        int     21h
        mov     w[OldInt2Fr+0],bx
        mov     w[OldInt2Fr+2],es
        mov     ax,252fh
        mov     dx,offset Int2FPatch
        int     21h

ifdef KRNLDBG

;--- check for kernel debugger (wdeb386 API)

D386_Id            equ 0F386h
D386_Identify      equ 43h
D386_Prepare_PMode equ 44h

PMINIT_INIT_IDT    equ 0
DS_DebLoaded       equ 4Fh  ;value for AX calling int 41h
DS_CondBP          equ 0F001h  ;BP if debugger is launched with /b

    push es
    push 0
    pop es
    cmp dword ptr es:[68h*4],0     ; int 68h != 0?
    jz nokd
    mov ah, D386_Identify
    int 68h
    cmp ax, D386_Id                ; kernel debugger waiting?
    jnz nokd
    push ds
    mov ax, D386_Prepare_PMode shl 8
    mov cx, KDbgCS     ; first of 3 selectors for debugger (code/data/scratch)
    mov bx, KernalZero ; flat 4G selector
    mov dx, GDTData    ; GDT selector
    mov si, 0          ; DS:SI: GDT
    mov ds, GDTReal
    mov di, -1         ; ES:DI: IDT
    int 68h
    or [si+KDbgCS+2*8].Desc.Access,DescPresent	; ensure the scratch descriptor is marked as 'used'
    pop ds
    ; returns FAR32 address of init func in ES:EDI
    mov dword ptr [pminit+0], edi
    mov word ptr [pminit+4], es
nokd:
    pop es
endif
;
;Now patch RAW specific calls.
;
        cmp     ProtectedType,PT_VCPI
        jz      cw5_VCPI
;
;Use RAW method to switch to protected mode.
;
cw5_RAW:
        .386p
;
;Need to initialise 1st entry of page dir & alias.
;
        movzx   eax,Page1stReal         ;segment address with bits 0-7 cleared!
        shl     eax,4
        or      ax,111b                 ;user+write+present
        mov     es,PageDIRReal
        xor     di,di
        mov     es:[di],eax
        mov     es,PageAliasReal
        mov     es:[di],eax
        mov     es,KernalTSSReal
;--- eax doesn't hold the value for CR3 ( it's PDE for page table 0 )!
;        mov     es:[di].TSSFields.tCR3,eax  ;set CR3 in TSS as well.
        ;
        ;map alias into page dir as well.
        ;
        movzx   eax,PageAliasReal       ;get para address (bits 0-7 cleared).
        shl     eax,4                   ;make linear.
        or      ax,111b                 ;user+write+present.
        mov     es,PageDIRReal
        mov     di,1023*4
        mov     es:[di],eax             ;setup in last page dir entry.
        ;
;       pushfd
;       pop     eax
;       mov     EFlagsSav,eax

        mov     Protected2Real,offset RawProt2Real
        mov     Real2Protected,offset RawReal2Prot
        mov     eax,cr0
        or      eax,080000001h          ;set PM+PG bits.
        mov     CR0ProtSav,eax          ; save protected mode status of CR0
        jmp     cw5_InProt
;
;Use VCPI method to switch to protected mode.
;
cw5_VCPI:
        mov     IErrorNumber,6
;        cli
        push    ds
        xor     di,di                   ;Page table offset.
        mov     es,Page1stReal          ;Page table segment
        mov     si,VCPI_0 and 0fff8h    ;VCPI GDT entries offset.
        mov     ds,GDTReal              ;GDT segment.
        mov     ax,0de01h               ;Let VCPI server prepare.
        int     67h
        pop     ds
        or      ah,ah
        jnz     InitError
        mov     d[VCPI_Entry+0],ebx     ;Store entry point.

; MED 11/05/96
        mov     FirstUninitPage,di      ; VCPI server advanced to first uninitialized page
                                        ; table entry in client's page

        ;
        ;Now update PHYSICAL addresses of dir & 1st page tables.
        ;
        mov     es,Page1stReal
        mov     di,es
        shr     di,8-2                  ;convert to offset (0-3FCh) for PT 0
        mov     eax,es:[di]             ;get physical address.
        and     ax,0f000h               ;clear status bits 0-11.
        or      ax,111b                 ;set our bits.
        mov     es,PageDIRReal
        xor     di,di
        mov     es:[di+0],eax           ;set first PDE in page dir
        mov     es,PageAliasReal
        mov     es:[di+0],eax
        ;
        mov     es,Page1stReal
        mov     di,PageDIRReal
        shr     di,8-2                  ;convert to offset (0-3FCh) for PT 0
        mov     eax,es:[di]             ;get physical address.
        and     ax,0F000h               ;clear status bits.
        mov     vcpi._CR3,eax           ;set VCPI CR3 value
        ;
        mov     es,Page1stReal
        mov     di,PageAliasReal
        shr     di,8-2                  ;convert to offset (0-3FC) for PT 0
        mov     eax,es:[di]             ;get physical address.
        and     ax,0F000h               ;clear status bits.
        or      ax,111b                 ;user+write+present.
        mov     es,PageDIRReal
        mov     di,1023*4
        mov     es:[di],eax             ;setup in last page dir entry (address range FFC00000-FFFFFFFF)
        ;
        mov     vcpi._LDT,KernalLDT
        mov     vcpi._TR,KernalTS

        mov     esi,GROUP16
        shl     esi,4
        mov     eax,offset GDTVal
        add     eax,esi
        mov     vcpi._pGDT,eax
        mov     eax,offset IDTVal
        add     eax,esi
        mov     vcpi._pIDT,eax
cw5_InProt:
;
;Raw/VCPI: ready to switch to protected mode at last
;
        cli
        mov     cx,MainSS
        mov     edx,offset _cwStackEnd
        call    [Real2Protected]
        or      IProtectedMode,-1

;--- set a few bits in CR0 and CR4

        mov     ax,offset setcr0cr4
        call    CallPL0Proc

ifdef KRNLDBG
 if EARLYKDINIT
;--- a temp IDT (1 kb) has been created in _cwStack, so the KD may start
        cmp     w[pminit+4],0
        jz      @F
        push    es
        mov     ax,KernalZero
        mov     es,ax
        mov     edi,d[IDTVal+2]
        mov     ax,setupkd
        call    CallPL0Proc
        pop     es
        jmp     @F
setupkd:        
        mov     ax, PMINIT_INIT_IDT
        call    [pminit]
        or      esi,-1
        mov     ax, DS_CondBP           ; stop in kd if loaded with /b
        int     41h
        retd
@@:        
 endif
endif
;
;Setup initial segment variables.
;
        or      w[SystemFlags],32768    ;Flags us in protected mode.
;
;Make sure A20 is enabled.
;--- this makes the first switch to real-mode!
;
        mov     IErrorNumber,7
        mov     ax,1
        call    A20Handler
cw_safesp:                  ;space up to this point may be used for rm stack while _cwInit code is still running
        jnz     InitError
;
;Now get extended memory sorted out, move the page tables into extended memory
;for a start.
;
        mov     IErrorNumber,5
        mov     ax,KernalZero
        mov     es,ax
;
;Allocate 2nd page table so we can map extended memory.
;
        call    fPhysicalGetPage
        jc      InitError
        and     cx,1                    ;put user bits in useful place.
        shl     cx,10
        and     dx,0F000h               ;clear bits 0-11.

        or      dx,111b                 ;present+user+write.
        or      dx,cx                   ;set use flags.
        mov     eax,1                   ;use 2. entry (range 400000-7fffff)
        mov     esi,PageDirLinear
        mov     es:[esi+eax*4],edx      ;store this tables address.
        mov     esi,PageAliasLinear     ;get alias table address.
        mov     es:[esi+eax*4],edx      ;setup in alias table as well, same range.
;        call    CR3Flush
        mov     edi,PTMAPADDR+1000h     ;base of page alias's (=FFC00000).
        mov     ecx,4096/4
        xor     eax,eax
        cld
        rep     stosd [edi]
        call    CR3Flush
        mov     LinearEntry,1024        ;start address space (400h shl 12 = 400000h)
;
;Setup DET page alias.
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax

        mov     PageDETLinear,eax
        mov     ecx,4096/4
        xor     eax,eax
        cld
        rep     stosd [edi]             ;clear it.

        mov     eax,PDEMAPDET           ;PDE index
        mov     esi,PageDirLinear
        mov     edx,CurrPhysPage        ;get physical address again.
        or      dx,111b
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        mov     esi,PageAliasLinear
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        call    CR3Flush
;
;Setup DET page 1st.
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax

        mov     ecx,4096/4
        mov     eax,MEM_FILL
        cld
        rep     stosd [edi]             ;copy old to new.

        mov     edx,CurrPhysPage
        or      dx,111b
        mov     esi,PageDETLinear
        mov     eax,0
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        call    CR3Flush
;
;Allocate 2nd page DET
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax

        mov     ecx,4096/4
        mov     eax,MEM_FILL
        cld
        rep     stosd [edi]             ;copy old to new.

        mov     edx,CurrPhysPage        ;get physical address again.
        or      dx,111b
        mov     esi,PageDETLinear
        mov     eax,1
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        call    CR3Flush
;
;Move page alias into extended memory.
;
        call    getandmappage
        shl     eax,12                  ;get linear address.
        mov     edi,eax

        mov     esi,PageAliasLinear
        mov     PageAliasLinear,eax
        mov     ecx,4096/4
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd [edi],[esi]       ;copy old to new.
        pop     ds

        mov     edx,CurrPhysPage
        or      dx,111b
        mov     eax,1023
        mov     esi,PageDirLinear
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        mov     esi,PageAliasLinear
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        call    CR3Flush

if MOVEPAGE1STTOEXT
;
;Move page 1st into extended memory.
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax
        ;
        ;Copy table to new memory.
        ;
        mov     esi,Page1stLinear
        mov     Page1stLinear,eax       ;set new linear address.
        mov     ecx,4096/4
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd [edi],[esi]       ;copy old to new.
        pop     ds
        ;
        ;Set new address in page dir & page dir alias.
        ;
        mov     edx,CurrPhysPage
        or      dx,111b
        mov     eax,0
        mov     esi,PageDirLinear
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        mov     esi,PageAliasLinear
        mov     es:[esi+eax*4],edx      ;put new page into the map.
        call    CR3Flush
endif
;
;Move page dir into extended memory.
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax
        ;
        ;Copy table to new memory.
        ;
        mov     esi,PageDirLinear
        mov     PageDirLinear,eax       ;set new value.
        mov     ecx,4096/4
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd [edi],[esi]       ;copy old to new.
        pop     ds
        ;
        ;Make variables point to new memory.
        ;
        mov     eax,CurrPhysPage
        mov     vcpi._CR3,eax           ;set new physical address.
        call    CR3Flush
;
;Setup IDT.
;
        call    getandmappage
        shl     eax,12
        mov     edi,eax

        mov     d[IDTVal+2],eax
        mov     w[IDTVal+0],256*8-1
        ;
        mov     bl,0                    ;vectors#
        mov     ecx,offset InterruptHandler ;offset32
        mov     esi,DpmiEmuCS0          ;gate to use.
        mov     al,0
        mov     ah,DescPresent+DescPL3+Desc386Int ; create interrupt gates
cw5_3:
        call    MakeGate
        add     edi,sizeof GATE         ;address next IDT gate
        add     ecx,INTCALLSIZE         ;/
        inc     bl
        jnz     cw5_3

if MOVETSS
;--- move TSS just behind the IDT
        push    ds
        mov     ax, GDTData
        mov     ds, ax
        mov     si, KernalTS and 0fff8h
        mov     eax, edi
        mov     [si].Desc.Base_l, ax
        shr     eax,16
        mov     [si].Desc.Base_m, al
        mov     [si].Desc.Base_H, ah
        mov     si, KernalSS0
        mov     eax, edi
        add     eax,TSSFields.tPL1Stack
        mov     [si].Desc.Base_l, ax
        shr     eax,16
        mov     [si].Desc.Base_m, al
        mov     [si].Desc.Base_H, ah

        movzx   esi,cs:KernalTSSReal
        shl     esi, 4
        mov     ecx,sizeof TSSFields
        push    es
        pop     ds
        rep     movsb [edi],[esi]
        pop     ds
endif

ifdef KRNLDBG
 if EARLYKDINIT
;--- if there was an early kd init, copy the temp IDT to the current one 
        cmp     w[pminit+4],0
        jz      @F
        mov     edi,d[IDTVal+2]
        mov     esi, _cwStack
        shl     esi, 4
        mov     ecx, 128*8/4
        rep     movsd [edi],es:[esi]
@@:
 endif
endif
        ;
        ;Re-load IDT value.
        ;
        mov     ax, offset loadidt
        call    CallPL0Proc


ifdef KRNLDBG
 ife EARLYKDINIT
;--- kernel debugger init needs an IDT and must be in PL0.
;--- we are in PL3 now, so a call gate is used to switch to PL0;
;--- the offset of the code to call must be set in the call gate!

		cmp w[pminit+4], 0
		jz nokdinit
		push es
		mov ax, GDTData
		mov es, ax
		mov bx, MainPL3toPL0 and 0F8h
		mov eax, d[pminit+0]
		mov es:[bx].GATE.OfsLow, ax
		shr eax, 16
		mov es:[bx].GATE.OfsHigh, ax
		mov ax, w[pminit+4]
		xchg ax,es:[bx].GATE.sel
		push ax
		push es
		push bx
		mov ax, KernalZero
		mov es, ax
		mov edi,dword ptr IDTVal+2	;es:edi=IDT
		mov ax, PMINIT_INIT_IDT
		db 09ah
		dw 0
		dw MainPL3toPL0
		pop bx
		pop es
		pop es:[bx].GATE.sel
		mov es:[bx].GATE.OfsHigh, 0
		pop es
nokdinit:
 endif
endif
;
;Get extended memory for DPMI emulator.
;
        mov     IErrorNumber,5
        ;
        mov     ebp,cwDPMIEMUEnd - cwDPMIEMUStart
        add     ebp,4096-1
        shr     ebp,12                  ;Get number of pages needed.
        mov     edi,LinearEntry
cw5_2:
        call    getandmappage
        dec     ebp
        jnz     cw5_2
;
;Copy DPMI emulator code into extended memory we just allocated.
;
        shl     edi,12
        mov     eax,edi
        mov     esi,_cwDPMIEMU
        shl     esi,4                   ;Point to the source.
        mov     ecx,cwDPMIEMUEnd - cwDPMIEMUStart
        shr     ecx,2
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd [edi],[esi]       ;Copy it up their.
        pop     ds
;
;Setup DPMI emulator selectors.
;
        push    es
        mov     esi,eax
        mov     ax,GDTData
        mov     es,ax
        mov     ecx,cwDPMIEMUEnd - cwDPMIEMUStart - 1
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,DpmiEmuCS
        call    MakeDesc
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,DpmiEmuCS0
        call    MakeDesc
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,DpmiEmuDS
        call    MakeDesc
        pop     es
;
;Should be OK to enable interrupts at last.
;
        mov     ah,1
        int     16h
        sti
;
;Initialise hardware interrupt call-back's.
;
        call    InitHardwareInts
;
;Allocate memory for new GDT/LDT
;it's 64kB + 8 kB byte string behind GDT/LDT
;
        mov     IErrorNumber,5
        ;
        mov     ebp,(8192*8+8192)/4096  ;# of pages needed (=18)
        mov     edi,LinearEntry
cw5_6:
        call    getandmappage
        dec     ebp
        jnz     cw5_6
        ;
        shl     edi,12
        mov     MDTLinear,edi           ;store for allocation code.
        mov     ecx,(8192*8)/2
        cld
        xor     ax,ax
        rep     stosw [edi]
        mov     MDTLinear+4,edi         ;setup bit (actually: byte) string address.
        or      eax,-1
        stosd   [edi]                   ;Force VCPI values to not used.
        mov     cx,(8192-4)/2
        xor     ax,ax
        rep     stosw [edi]
        ;
        ;See which table we want to use.
        ;
        test    BYTE PTR RawSystemFlags,128 ;GDT or LDT?
        jnz     cw5_LDT

        ;
        ;Copy current GDT to new GDT.
        ;init the GDTData descriptor in the new GDT after the copy;
        ;this ensures a stable switch to the new GDT!
        ;
        mov     esi,MDTLinear

        push    ds
        mov     edi,esi
        push    edi
        mov     esi,d GDTVal+2
        push    es
        pop     ds
        mov     ecx,GDT_Entries*2
        cld
        rep     movsd [edi],[esi]
        pop     edi
        mov     eax,edi
        add     edi,GDTData and 0fff8h
        mov     [edi].Desc.Base_l,ax
        shr     eax,16
        mov     [edi].Desc.Base_m,al
        mov     [edi].Desc.Base_H,ah
        pop     ds
        
        ;
        ;Set new GDT values.
        ;
        pushf
        cli
        mov     eax,MDTLinear
        mov     d[GDTVal+2],eax
        popf
        mov     ah,1
        int     16h                     ;force GDT reload with mode switch.
        ;
        ;Now mark all used descriptors in allocation control string.
        ;
        mov     edi,d GDTVal+2
        mov     esi,MDTLinear+4
        mov     cx,GDT_Entries
cw5_4:
        test    BYTE PTR es:[edi].Desc.Access,DescPresent ;this descriptor in use?
        jz      cw5_5
        or      BYTE PTR es:[esi],-1
cw5_5:
        add     edi,8                   ;next descriptor.
        inc     esi                     ;update descriptor number.
        loop    cw5_4
        ;
        ;Now setup extra GDT descriptors.
        ;
        push    es
        mov     ax,GDTData
        mov     es,ax
        ;
        mov     di,KernalB000
        mov     esi,0b0000h
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        ;
        mov     di,KernalB800
        mov     esi,0b8000h
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        ;
        mov     di,KernalA000
        mov     esi,0a0000h
        mov     ecx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        pop     es
        ;
cw5_LDT:
        ;Setup new LDT.
        ;Map LDT into GDT.
        ;
        push    es
        mov     ax,GDTData
        mov     es,ax
        mov     esi,MDTLinear
        mov     di,KernalLDT
        mov     ecx,8192*8-1
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescLDT
        call    MakeDesc
        pop     es
        ;
        mov     ah,1
        int     16h                     ;force LDT reload with mode switch.

;--- The system tables just generated will remain in extended memory.
;--- At the final exit, VCPI pages are released while still in protected-mode.
;--- Some pages must be hold, and released only when in real-mode; those pages
;--- are stored in table PhysPageSave and the VCPI bit of the PTE are cleared here.

;--- reset VCPI flag for
;--- 1 first page of GDT
;--- 2 page directory
;--- 3 PDE for region 400000-7FFFFF (where the GDT is located)
;--- 4 optionally page for IDT (if KRNLDBG is defined)
;--- 5 optionally PDE for region 000000-3FFFFF (MOVEPAGE1STTOEXT=1)
;--- ES=zero-based flat

        mov     bx, offset PhysPageSave
        mov     esi,PTMAPADDR
        mov     eax, d GDTVal+2
        shr     eax, 12
        btr     w es:[esi+eax*4],10
        jnc     @F
        mov     eax,es:[esi+eax*4]
        mov     [bx], eax
        add     bx, 4
@@:
        mov     eax, PageDirLinear
        shr     eax, 12
        btr     w es:[esi+eax*4],10
        jnc     @F
        mov     eax,es:[esi+eax*4]
        mov     [bx], eax
        add     bx, 4
@@:
ifdef KRNLDBG ; release page for IDT in real-mode only, so KD can run till final switch to real-mode
        mov     eax, d[IDTVal+2]
        shr     eax, 12
        btr     w es:[esi+eax*4],10
        jnc     @F
        mov     eax,es:[esi+eax*4]
        mov     [bx], eax
        add     bx, 4
@@:
endif
        mov     esi, PageDirLinear
        btr     w es:[esi+4],10
        jnc     @F
        mov     eax,es:[esi+4]
        mov     [bx], eax
        add     bx, 4
@@:
if MOVEPAGE1STTOEXT
        btr     w es:[esi+0],10
        jnc     @F
        mov     eax,es:[esi+0]
        mov     [bx], eax
@@:
endif
;
;Initialise application memory pool.
;
        mov     eax,LinearEntry
        shl     eax,12
        mov     LinearBase,eax
        add     eax,4096
        mov     LinearLimit,eax
        ;
        call    getandmappage
        dec     LinearEntry             ;undo INC done inside getandmappage()
        mov     esi,1024*4096*PDEMAPDET ;=FF800000h
        mov     DWORD PTR es:[esi+eax*4],0      ;clear this page's details.
;
;Initialise virtual memory manager stuff.
;
        call    fPhysicalGetPages       ;find free pages.
        mov     eax,edx
        shl     eax,12
        add     eax,LinearLimit
        sub     eax,LinearBase
        mov     ebp,eax                 ;save this for comparisons.
        mov     di,offset VMMDrivPath1  ;point to start of paths.
        cmp     NoVMSwitch,0
        jnz     cw5_v9
        ;
        ;Work through list of possibles till we find a useful entry.
        ;
cw5_v0:                                 ;<<<<---- next entry
        cmp     BYTE PTR [di],0
        jz      cw5_v7
        cmp     BYTE PTR [di],-1
        jz      cw5_v9
        ;
        ;Check drive is valid and has enough free space.
        ;

COMMENT !
; MED 10/11/96
; if no drivespec, then use default drive
        cmp     BYTE PTR [di+1],':'
        je      isdrive

        movzx   edx,di                  ; scan to end of current pathspec
defloop:
        inc     dx                      ; edx known 16-bit value so 16-bit increment is valid
        cmp     BYTE PTR [edx],0
        jne     defloop

; at end of current pathspec, now shift forward two bytes to allow for drive
shiftloop:
        mov     al,[edx]
        mov     [edx+2],al
        dec     dx
        cmp     di,dx                   ; edx known 16-bit value so 16-bit compare is valid
        jae     shiftloop

        push    di
        mov     edi,offset PageInt      ; Pageint is in 16-bit segment, but RawSimulateInt expects addr in es:edi
        push    ds
        pop     es
        mov     w [di].RealRegsStruc.Real_EAX,1900h
        mov     RealRegsStruc.Real_SSSP[di],0
        mov     bl,21h
        call    RawSimulateInt
        mov     al,BYTE PTR [di].RealRegsStruc.Real_EAX
        pop     di
        add     al,'A'                  ; convert to drive
        mov     [di],al
        mov     BYTE PTR [di+1],':'     ; add colon to drivespec
END COMMENT !

isdrive:
        mov     dl,[di]                 ;get swap file drive.
        cmp     dl,'a'
        jb      cw5_v1
        cmp     dl,'z'
        ja      cw5_v1
        and     dl,5Fh                  ;convert to upper case.
cw5_v1:
        sub     dl,'A'                  ;make it real.
        inc     dl                      ;adjust for current type selection.

drivefree:
        mov     ah,36h                  ;get free space.
        push    ebp
        int     21h                     ;/
        pop     ebp
        cmp     ax,-1                   ;invalid drive?
        jz      cw5_v7
        mul     cx                      ;Get bytes per cluster.
        mul     bx                      ;Get bytes available.
        shl     edx,16
        mov     dx,ax
        cmp     edx,ebp                 ;Enough free space.
        jc      cw5_v7
        ;
        ;See if we can create a temp file.
        ;

; MED 02/25/96, use name specified in CAUSEWAY e-var
        test    DesiredVMMName,-1
        je      med5a                   ; no VMM name request
        push    di
        mov     si,di
        push    si                      ; save -> filespec start

; find end of pathspec
mednameloop:
        cmp     BYTE PTR [si],0
        je      medndone
        inc     si
        jmp     mednameloop

; append desired name on filespec
medndone:

; 05/15/98
; check for backslash already existing
        cmp     BYTE PTR [si-1],'\'
        je      medbs
        mov     BYTE PTR [si],'\'
        inc     si
medbs:
        mov     di,offset DesiredVMMName

medtransloop:
        mov     al,[di]
        mov     [si],al
        inc     di
        inc     si
        test    al,al
        jne     medtransloop
        pop     si                      ; restore si -> filespec start

        mov     edi,offset PageInt
        push    ds
        pop     es
        mov     [di].RealRegsStruc.Real_DS,GROUP16
        mov     w [di].RealRegsStruc.Real_EDX,si
        mov     w [di].RealRegsStruc.Real_EAX,3c00h
        mov     w [di].RealRegsStruc.Real_ECX,0
        mov     [di].RealRegsStruc.Real_SSSP,0
        mov     bl,21h
        call    RawSimulateInt
        test    BYTE PTR RealRegsStruc.Real_Flags[di],1
        mov     eax,RealRegsStruc.Real_EAX[di]
        pop     di
        jz      cw5_v8
        jmp     cw5_v7
        ;

med5a:
        push    di
        mov     si,di
        mov     edi,offset PageInt
        push    ds
        pop     es
        mov     [di].RealRegsStruc.Real_DS,GROUP16
        mov     w [di].RealRegsStruc.Real_EDX,si
        mov     w [di].RealRegsStruc.Real_EAX,5a00h
        mov     w [di].RealRegsStruc.Real_ECX,0
        mov     [di].RealRegsStruc.Real_SSSP,0
        mov     bl,21h
        call    RawSimulateInt
        test    BYTE PTR RealRegsStruc.Real_Flags[di],1
        mov     eax,RealRegsStruc.Real_EAX[di]
        pop     di
        jz      cw5_v8
        ;
cw5_v7:
        add     di,128
        jmp     cw5_v0
        ;
cw5_v8: ;Store the handle and copy the name accross.
        ;
        mov     VMMHandle,ax            ;store the handle.
        mov     di,offset VMMName
        mov     cx,128
        rep     movsb
        mov     al,VMMName
        cmp     al,'a'
        jb      cw5_v2
        cmp     al,'z'
        ja      cw5_v2
        and     al,5Fh                  ;convert to upper case.
cw5_v2:
        mov     VMMName,al

; MED 02/25/96, if pre-allocate, then force write to allocated size
        cmp     PreAllocSize,0
        je      medpre2
        mov     bx,VMMHandle
        mov     ecx,PreAllocSize
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h                ; seek from beginning of file
        int     21h
        xor     cx,cx                   ; write zero bytes (pre-allocating based on seek)
        mov     ah,40h                  ; write to file
        int     21h
        mov     ah,68h                  ; commit file
        int     21h
        xor     dx,dx                   ; get current size of swap file
        mov     cx,dx
        mov     ax,4202h
        int     21h
        mov     WORD PTR SwapFileLength+0,ax  ; update internal swapfile length variable
        mov     WORD PTR SwapFileLength+2,dx

medpre2:
        ;
        ;Now patch the exception vector.
        ;
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,ax
        assume ds:_cwDPMIEMU
        mov     ebx,offset ExceptionTable+14*6 ;Page fault vector.
        mov     edx,offset VirtualFault ;new EIP
        mov     cx,DpmiEmuCS            ;new CS
        xchg    edx,[ebx+0]             ;get old EIP
        xchg    cx,[ebx+4]              ;get old cs
        mov     d[OldExcep14+0],edx     ;store offset.
        mov     w[OldExcep14+4],cx
        or      DpmiEmuSystemFlags,1 shl 1 ;flag VMM's presence.
        pop     ds
        assume ds:GROUP16
        or      SystemFlags,1 shl 1     ;flag VMM's presence.
        or      RawSystemFlags,1 shl 1  ;flag VMM's presence.
cw5_v9:
if 1 ;resize memory to 8k/12k
        mov     edi,offset PageInt
        mov     ax,wUMB
        mov     [di].RealRegsStruc.Real_ES,ax
        mov     w [di].RealRegsStruc.Real_EBX,(2000h+1000h)/16
        mov     w [di].RealRegsStruc.Real_EAX,4A00h
        mov     [di].RealRegsStruc.Real_SSSP,0
        push    ds
        pop     es
        mov     bl,21h
        call    RawSimulateInt
endif
        jmp     cw5_InProtected

;--- end of RAW/VCPI specific init ---
;
;Do initialisations needed for DPMI
;
cw5_InitDPMI:
;
;Get 8k for the transfer buffer.
;currently the transfer buffer is used 
; a) to copy text about exceptions in interrup.inc
; b) its alias in EPSP_TransReal by int21h, int10h, int33h, ...
;
        mov     IErrorNumber,5
        mov     bx,8192/16
        mov     ah,48h
        int     21h                     ;get memory for transfer buffer.
        jc      InitError
        mov     TransferReal,ax
;
;Get some memory for the INT buffer.
;
        mov     bx,((RawStackTotal/2)/16)	;only half the size as in Raw/VCPI
        mov     ah,48h
        int     21h
        jc      InitError
        mov     DPMIStackSeg,ax
        mov     DPMIStackOff,RawStackTotal/2
;
;Do instalation check and get mode switch address.
;
        mov     IErrorNumber,9
        mov     ax,1687h                ;DPMI instalation check.
        int     2fh
        or      ax,ax                   ;None-zero means its not there.
        jnz     InitError
;
;Check for 32-bit support if needed.
;
        test    SystemFlags,1 shl 14    ;Dual mode?
        jnz     cw5_Use16Bit23
        ;
        test    BYTE PTR SystemFlags,1
        jz      cw5_Use32Bit23
        jmp     cw5_Use16Bit23
cw5_Use32Bit23:
        mov     IErrorNumber,9
        test    bx,1                    ;Must offer 32 bit support.
        jz      InitError
cw5_Use16Bit23:
        mov     bx,si                   ;Get DPMI save buffer size.
        mov     ax,si
        or      bx,bx
        jz      cw5_d0                  ;No guarante that it'll need it.
;
;Allocate memory for DPMI state save buffer.
;
        mov     IErrorNumber,5
        push    di
        push    es
        mov     ah,48h
        int     21h                     ;Try and claim memory for it.
        pop     es
        pop     di
        mov     bx,ax
        jc      InitError
cw5_d0:
        push    es                      ;Store the switch call address.
        push    di
        mov     bp,sp
        mov     es,bx
;
;Attempt to switch mode.
;
        mov     ax,1                    ;start as 32-bit client
        test    BYTE PTR SystemFlags,1
        jz      cw5_Use32Bit24
        xor     ax,ax                   ;start as 16-bit client
cw5_Use32Bit24:
        pusha
        call    d[bp]                   ;Make the switch.
        popa
        jnc     cw5_DpmiInProtected
        mov     IErrorNumber,9
        test    w[SystemFlags+2],1      ;Dual mode?
        jz      InitError
        xor     SystemFlags,1
        xor     ax,1                    ;toggle the mode.
        call    d[bp]                   ;Make the switch.
        jc      InitError               ;really isn't feeling well.
cw5_DpmiInProtected:
        mov     CodeSegment,cs
        mov     DataSegment,ds
        mov     StackSegment,ss
        mov     PSPSegment,es
        mov     ax,es:[2ch]
        mov     ENVSegment,ax
        mov     w[TerminationHandler+4],cs
        movzx   esp,sp
        add     sp,2+2
;
;Create a 0-4G selector.
;
        mov     IErrorNumber,8
        mov     ax,0000h
        mov     cx,1
        int     31h                     ;allocate a selector.
        jc      InitError
        mov     RealSegment,ax
        mov     bx,ax
        push    ds
        pop     es
        mov     di,offset dpmiSelBuffer
        xor     esi,esi
        or      ecx,-1
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        mov     ax,000ch
        push    ds
        pop     es
        mov     edi,offset dpmiSelBuffer
        int     31h
;
;Patch INT 21h exec function to preserve the stack.
;
if 0
        mov     bl,21h
        mov     ax,200h
        int     31h
        mov     w[OldInt21hExec+0],dx
        mov     w[OldInt21hExec+2],cx
        mov     dx,offset Int21hExecPatch
        mov     cx,GROUP16
        mov     bl,21h
        mov     ax,201h
        int     31h
endif
        or      IProtectedMode,-1

;--- END of DPMI specific init ---
;
;Now get on with installing the higher level stuff.
;code for RAW/VCPI/DPMI
;
cw5_InProtected:
;        mov     ax,DataSegment
;        mov     ds,ax
;        assume ds:GROUP16
        ;
        mov     ax,ProtectedType        ;Copy protected mode environment type into common
        shl     ax,1+1                  ;variable for application access. Might become useful
        or      w[SystemFlags],ax       ;at some point. Other flags can be added at will.
        mov     ax,ProtectedFlags       ;bits 0-2 are relevant
        shl     ax,1+1+2
        or      w[SystemFlags],ax
        or      w[SystemFlags],32768    ;Flags us in protected mode.
        ;
;        mov     ax,DataSegmenti
;        mov     ds,ax
;        assume ds:GROUP16
;
;Add CW API patch to int 31h and 2Fh.
;
        mov     IErrorNumber,5
        xor     bx,bx
        mov     cx, lowword offset endGroup32
        mov     ax,0501h
        int     31h                     ;Get memory.
        jc      InitError
        xor     si,si
        mov     di, lowword offset endGroup32
        mov     ax,0600h
        int     31h                     ;Lock memory.
        jc      InitError
        shl     ebx,16
        mov     bx,cx
        mov     dpmiSelBase,ebx
;
;Allocate code & data selector.
;
        mov     IErrorNumber,8
        mov     cx,2
        mov     ax,0000h
        int     31h                     ;allocate 2 selectors.
        jc      InitError
        mov     Group32CS,ax
        mov     bx,ax
        push    ds
        pop     es
        mov     edi,offset dpmiSelBuffer
        mov     esi,dpmiSelBase
        mov     ecx, offset endGroup32 - 1
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        call    MakeDesc
        mov     ax,000ch
        int     31h
;
;setup data selector.
;
        add     bx, 8
        mov     Group32DS,bx
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        mov     ax,000ch
        int     31h
;
;Copy API code into the new memory.
;
        push    es
        push    ds
        mov     es,Group32DS
        assume es:GROUP32
        mov     ds,RealSegment
        xor     edi,edi
        mov     esi,GROUP32
        shl     esi,4
        mov     ecx, offset endGroup32
        rep     movsb [edi],[esi]
        pop     ds
;
;Setup descriptors in new memory.
;
        mov     apiDSeg16,ds
        mov     apiDSeg32,es
        mov     eax,d[SystemFlags]
        mov     DWORD PTR [apiSystemFlags],eax
;
;Set INT vector to bring API code into play.
;
        mov     bl,31h
        mov     ax,204h
        int     31h
        test    BYTE PTR SystemFlags,1
        jz      cw5_Use32
        mov     [int31call], offset int31call16
        mov     [int31callcc], offset int31call16cc
        movzx   edx, dx
        jmp     cw5_Use0
cw5_Use32:
        mov     [int31call], offset int31call32
        mov     [int31callcc], offset int31call32cc
cw5_Use0:
        mov     DWORD PTR [OldInt31+0],edx
        mov     WORD PTR [OldInt31+4],cx
        mov     cx,Group32CS
        mov     edx,offset cwAPIpatch
        mov     bl,31h
        mov     ax,205h
        int     31h
;
;Copy version through to API.
;
        mov     al,b[VersionMajor]
        sub     al,'0'
        mov     cwMajorVersion,al
        mov     al,b[VersionMinor]
        sub     al,'0'
        shl     al,1    ;*2
        mov     ah,al
        shl     al,2    ;*8
        add     ah,al
        mov     al,b[VersionMinor+1]
        sub     al,'0'
        add     al,ah
        mov     cwMinorVersion,al
        pop     es
        assume es:nothing
;
;Set flag so we know the API is in place.
;
        mov     d[TerminationHandler+0],offset InitError
        mov     w[TerminationHandler+4],cs
;
;Sort out state save address & size.
;
        mov     ax,0305h
        int     31h
        jc      cw5_NoState
        mov     w[DPMIStateSize+0],ax
        mov     w[DPMIStateSize+2],0
        test    BYTE PTR SystemFlags,1
        jz      cw5_DS_Use32
        mov     w[DPMIStateAddr+0],di
        mov     w[DPMIStateAddr+2],si
        jmp     cw5_NoState
cw5_DS_Use32:
        mov     d[DPMIStateAddr+0],edi
        mov     w[DPMIStateAddr+4],si
        ;
cw5_NoState:
;
;Patch exception vectors to put API handlers in place.
;
        mov     grp32Ofs, offset PatchExc
        call    [grp32Proc]
;
;Get memory for new PSP.
;
        mov     IErrorNumber,5
        mov     ecx,size EPSP_Struc
        Sys     GetMem32
        jc      InitError
        xchg    bx,PSPSegment
        push    ds
        push    es
        mov     es,PSPSegment
        mov     ds,bx
        xor     si,si
        xor     di,di
        mov     cx,256/4
        rep     movsd
        pop     es
        pop     ds
;
;Initialise PSP fields.
;
        mov     IErrorNumber,8
        push    ds
        push    es
        xor     edx,edx
        mov     es,PSPSegment
        mov     ax,ENVSegment
        mov     es:[PSP_Struc.PSP_Environment],ax           ;Setup ENV in PSP.
        mov     ax,RealENVSegment
        mov     es:[EPSP_Struc.EPSP_RealENV],ax
        mov     ax,offset Int21hExecCount
        mov     WORD PTR es:[EPSP_Struc.EPSP_ExecCount+0],ax
        mov     WORD PTR es:[EPSP_Struc.EPSP_ExecCount+2],ds
        mov     es:[EPSP_Struc.EPSP_Resource],edx           ;Clear memory fields.
        mov     es:[EPSP_Struc.EPSP_INTMem],edx
        mov     DWORD PTR es:[EPSP_Struc.EPSP_DPMIMem],edx  ;is a word only!
        mov     es:[EPSP_Struc.EPSP_Parent],es              ;set parent PSP.
        mov     es:[EPSP_Struc.EPSP_Next],dx
        mov     DWORD PTR es:[EPSP_Struc.EPSP_DTA+0],80h    ;Use default PSP DTA.
        mov     WORD PTR es:[EPSP_Struc.EPSP_DTA+4],es
        mov     eax,16384-(MCBCHUNKLEN+MCBLEN)
        mov     es:[EPSP_Struc.EPSP_mcbMaxAlloc],eax
        mov     es:[EPSP_Struc.EPSP_mcbHead],edx
        mov     es:[EPSP_Struc.EPSP_SegBase],dx
        mov     es:[EPSP_Struc.EPSP_SegSize],dx
        mov     es:[EPSP_Struc.EPSP_LastPSP],edx
        mov     es:[EPSP_Struc.EPSP_NextPSP],edx
        mov     es:[EPSP_Struc.EPSP_Exports],edx
        mov     es:[EPSP_Struc.EPSP_Imports],edx
        mov     es:[EPSP_Struc.EPSP_Links],80000000h
        mov     WORD PTR es:[EPSP_Struc.EPSP_EntryCSEIP+4],dx
        mov     es:[EPSP_Struc.EPSP_PSPSel],es
        mov     es:[EPSP_Struc.EPSP_FileName],dl
        mov     BasePSP,es
        mov     bx,es
        Sys     GetSelDet32
        mov     BasePSPAddress,edx
        ;
        Sys     GetSel
        jc      InitError
        movzx   edx,WORD PTR es:[PSP_Struc.PSP_HandlePtr+2]
        shl     edx,4
        movzx   ecx,WORD PTR es:[PSP_Struc.PSP_Handles]
        movzx   eax,WORD PTR es:[PSP_Struc.PSP_HandlePtr]
        add     edx,eax
        Sys     SetSelDet32
        mov     dx,bx
cw5_normal:
        mov     WORD PTR es:[PSP_Struc.PSP_HandlePtr+2],dx
        mov     WORD PTR es:[PSP_Struc.PSP_HandlePtr],0
        pop     es
        pop     ds
;
;Setup transfer buffer and selector.
;
        mov     IErrorNumber,8
        Sys     GetSel
        jc      InitError
        movzx   edx,TransferReal
        shl     edx,4
        mov     ecx,TransferSize-1	;default 8k
        Sys     SetSelDet32
        jc      InitError
        push    es
        mov     es,PSPSegment
        mov     es:[EPSP_Struc.EPSP_TransProt],bx
        mov     ax,TransferReal
        mov     es:[EPSP_Struc.EPSP_TransReal],ax
        mov     eax,TransferSize
        mov     es:[EPSP_Struc.EPSP_TransSize],eax
        pop     es
;
;Setup internaly EXPORT'ed symbols.
;
        mov     bx,Group32DS
        Sys     GetSelDet32
        mov     edi,edx
        add     edi,offset apiExports
        push    es
        mov     es,PSPSegment
        mov     es:[EPSP_Struc.EPSP_Exports],edi
        mov     es,RealSegment
        mov     ebp,es:[edi]
        add     edi,4
        add     DWORD PTR es:[edi],edx
        add     edi,4
cw5_exp0:
        add     DWORD PTR es:[edi],edx
        mov     esi,es:[edi]
        mov     bx,CodeSegment
        cmp     WORD PTR es:[esi+4],0
        jz      cw5_exp1
        mov     bx,DataSegment
        cmp     WORD PTR es:[esi+4],1
        jz      cw5_exp1
        mov     bx,Group32CS
        cmp     WORD PTR es:[esi+4],2
        jz      cw5_exp1
        mov     bx,Group32DS
        cmp     WORD PTR es:[esi+4],3
        jz      cw5_exp1
        or      bx,-1
cw5_exp1:
        mov     es:[esi+4],bx
        add     edi,4
        dec     ebp
        jnz     cw5_exp0
        pop     es
;
;Initialise extensions.
;
        or      mcbAllocations,-1       ;Enable MCB code.
        ;
        mov     di,offset ExtensionList
cw5_e0:
        cmp     di,offset ExtensionListEnd ;end of the list?
        jz      cw5_e9
        push    di
        mov     di,[di]
        mov     ax,Group32CS
        mov     word ptr [di].EXTENSION.pInit+4,ax ;store it for now.
        mov     word ptr [di].EXTENSION.pExit+4,ax
        mov     [di].EXTENSION.wFlgs,-1  ;flag installed.
        push    di
        push    ds
        mov     es,DataSegment
        mov     ds,Group32DS
        call    es:[di].EXTENSION.pInit
        pop     ds
        pop     di
        jc      InitError
        pop     di
        add     di,2
        jmp     cw5_e0
cw5_e9:
;
;Restore DOS memory allocation strategy.
;
        mov     bx,[cw5_OldStrat+2]
        xor     bh,bh
        mov     ax,5803h
        int     21h
        mov     bx,[cw5_OldStrat+0]
        xor     bh,bh
        mov     ax,5801h
        int     21h
;
;We're all done here so switch to main code segment for final re-size and run.
;
        jmp cwOpen

;--- out: eax=LinearEntry
;---      edx=PTE
;---      LinearEntry incremented
;---      CurrPhysPage updated
;---      edi preserved

getandmappage:        
        call    fPhysicalGetPage        ;try to allocate a page.
        jc      InitError
        mov     CurrPhysPage,edx        ;store physical address.
        and     cx,1                    ;put user bits in useful place.
        shl     cx,10
        and     dx,0f000h
        or      dx,111b                 ;present+user+write.
        or      dx,cx                   ;set use flags.
        mov     eax,LinearEntry
        mov     esi,PTMAPADDR           ;base of page alias's.
        mov     es:[esi+eax*4],edx      ;set physical address.
        call    CR3Flush
        inc     LinearEntry             ;update pointer.
        retn

Startup endp


;-------------------------------------------------------------------------------
;
;Something is wrong with this system so print an error message and get out of
;here.
;
InitError       proc    near
;
;Find out if we're in protected mode or not.
;
; allow 286 non-crash at this point

        .286
        cmp     cs:IProtectedMode,0     ; are we in protected mode?
        jz      cw6_RealMode

        .386
        cmp     cs:Group32DS,0          ;API installed?
        jz      cw6_noAPI
        ;
        mov     ds,cs:DataSegment
        assume ds:GROUP16
;
;Remove extension patches.
;
        mov     di,offset ExtensionListEnd-2 ;list of interupt patches.
cw6_p0:
        push    di
        mov     di,[di]
        cmp     [di].EXTENSION.wFlgs,-1  ;installed?
        jnz     cw6_p2
        call    [di].EXTENSION.pExit
cw6_p2:
        pop     di
        sub     di,2
        cmp     di,offset ExtensionList-2
        jnz     cw6_p0
;
;Remove api exception patches.
;
        cmp     apiExcepPatched,0
        jz      cw6_pe0
        mov     grp32Ofs, offset UnPatchExc
        call    [grp32Proc]
cw6_pe0:
;
;Remove the API patch.
;
        mov     ds,cs:DataSegment
        assume ds:GROUP16
        mov     es,Group32DS
        assume es:GROUP32
        mov     edx,DWORD PTR es:[OldInt31+0]
        mov     cx,WORD PTR es:[OldInt31+4]
        mov     bl,31h
        mov     ax,205h
        int     31h
        mov     DWORD PTR es:[cwIdentity+0],0
        mov     DWORD PTR es:[cwIdentity+4],0
        assume es:nothing
        ;
cw6_noAPI:
        cmp     cs:ProtectedType,PT_DPMI
        jz      cw6_DPMI
;
;Make RAW stuff addressable.
;
        cli                             ;Don't want interrupts interfering.
        mov     ax,KernalDS             ;Get supervisor data descriptor,
        mov     ds,ax                   ;DS,ES,FS,GS,SS must be data with 64k limit
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,ax
;
;Switch to RAW exit code. This returns with CPU in real-mode
;
        push    GROUP16          ; push a real-mode segment as return address!
        push    offset cw6_RealMode
        push    KernalCS
        push    offset RawVCPIRealMode
        retf
;
;Remove DPMI stuff.
;
        .386
cw6_DPMI:

if 0
        mov     ds,cs:iDataSegment
        assume ds:GROUP16
        mov     ds,DataSegment
        cmp     d[OldInt21hExec],0
        jz      cw6_d0
        mov     bl,21h
        mov     dx,w[OldInt21hExec+0]
        mov     cx,w[OldInt21hExec+2]
        mov     ax,201h
        int     31h
cw6_d0:
endif

;        mov     ds,cs:iDataSegment
        mov     ds,cs:DataSegment
        assume ds:GROUP16

        cmp     IErrorNumber,0
        jz      cw6_NoError
        mov     ax,IErrorNumber
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[IErrorM00n+0],al
        add     dl,'0'
        mov     b[IErrorM00n+1],dl

        mov     ax,w[InitErrorList]     ;get the "CauseWay error nn : " string
        mov     di,offset Int21Buffer
        push    ds
        pop     es
        mov     w RealRegsStruc.Real_EDX[di],ax
        mov     w RealRegsStruc.Real_EAX[di],900h
        mov     RealRegsStruc.Real_DS[di],GROUP16
        xor     cx,cx                   ;No stack parameters.
        mov     bx,21h                  ;no flags.
        mov     ax,300h
        int     31h                     ;Use real dpmi service.
;
;Get a pointer to the appropriate error message and print it.
;
        mov     bx,IErrorNumber
        add     bx,bx
        mov     ax,[InitErrorList+bx]
        mov     w RealRegsStruc.Real_EDX[di],ax
        mov     w RealRegsStruc.Real_EAX[di],900h
        mov     RealRegsStruc.Real_DS[di],GROUP16
        xor     cx,cx                   ;No stack parameters.
        mov     bx,21h                  ;no flags.
        mov     ax,300h
        int     31h                     ;Use real dpmi service.
        jmp     cw6_NoError

        .286
;
;Make sure our data is addressable.
;
cw6_RealMode:
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16
        cmp     IErrorNumber,0
        jz      cw6_NoError
        mov     ax,IErrorNumber
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[IErrorM00n+0],al
        add     dl,'0'
        mov     b[IErrorM00n+1],dl
        mov     dx,w[InitErrorList]     ;display "CauseWay error nn : " string
        mov     ah,9
        int     21h
;
;Get a pointer to the appropriate error message and print it.
;
        mov     bx,IErrorNumber
        add     bx,bx
        mov     dx,[InitErrorList+bx]
        mov     ah,9
        int     21h
;
;Now exit with the error number as the DOS "errorlevel".
;
cw6_NoError:
        mov     ax,IErrorNumber
        mov     ah,4ch
        int     21h
        assume ds:GROUP16
InitError       endp


;-------------------------------------------------------------------------------
;
;Initialise real mode hardware interupt vectors so that control is always passed to protected mode
;even if the interupt occurs in real mode. This simulates the DPMI environment and is essential for
;any program that re-programs IRQ-0 frequency.
;
InitHardwareInts proc near
        .386
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,ax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,ax
;       mov     ch,16
;       mov     cl,1ch
;       call    @@0
        mov     ch,17
        mov     cl,23h                  ;patch ctrl-break.
        call    cw7_0
        mov     ch,18
        mov     cl,24h                  ;patch critical error.
        call    cw7_0
        pop     es
        pop     ds
        ret
        ;
cw7_0:
        mov     ax,size CallBackStruc
        movzx   bx,ch
        mul     bx
        mov     bx,ax
        add     bx,offset CallBackTable
        pushf
        cli
        mov     CallBackStruc.CallBackNum[bx],cl    ;set interupt number.
        mov     CallBackStruc.CallBackFlags[bx],1+2 ;mark call back as used interupt.
        mov     ax,CallBackSize
        movzx   dx,ch
        mul     dx
        mov     si,offset ICallBackList
        add     si,ax                   ;index list of calls.
        push    bx
        movzx   bx,cl
        shl     bx,2
        mov     dx,es:[bx+0]
        mov     cx,es:[bx+2]
        mov     es:[bx+0],si
        mov     WORD PTR es:[bx+2],GROUP16
        pop     bx
        mov     w[bx].CallBackStruc.CallBackReal+0,dx
        mov     w[bx].CallBackStruc.CallBackReal+2,cx   ;store original real mode vector.
        popf
        ret
        assume ds:GROUP16
        .286
InitHardwareInts endp


;-------------------------------------------------------------------------------
CheckProcessor  proc    near
        xor     ax,ax                   ;Clear the flags.
        push    ax                      ;/
        popf                            ;/
        pushf                           ;Get the flags back.
        pop     ax                      ;/
        and     ax,0F000h               ;Get Bits 11-8
        cmp     ax,0F000h               ;Exist on this processor?
        je      cw8_9                   ;Must be an 8086
        mov     ax,0F000h               ;Setup the flags again.
        push    ax                      ;/
        popf                            ;/
        pushf                           ;Get them back.
        pop     ax                      ;/
        and     ax,0F000h               ;Get Bits 11-8
        jz      cw8_9                   ;Valid so must be 80286
        clc
        ret
cw8_9:  stc
        ret
CheckProcessor  endp


;-------------------------------------------------------------------------------
CheckDOSVersion proc near
        mov     ah,30h
        int     21h                     ;Get DOS version.
        mov     DOSVersion,ax
        cmp     al,3                    ;3.? or above?
        jc      cw9_9
        jnz     cw9_0                   ;less means trouble.
        cmp     ah,1
        jc      cw9_9
cw9_0:  clc
        ret
cw9_9:  stc
        ret
CheckDOSVersion endp

;--- get decimal number from es:[si]
;--- out: EDX=number

        .386

getnum proc
        xor     edx,edx
nextchar:
        mov     al,es:[si]
        or      al,al
        jz      done
        cmp     al," "
        jz      done
        cmp     al,"0"
        jc      done
        cmp     al,"9"+1
        jnc     done
        sub     al,"0"
        movzx   eax,al
        add     edx,edx
        mov     ebx,edx
        add     edx,edx
        add     edx,edx
        add     edx,ebx
        add     edx,eax
        inc     si
        jmp     nextchar
done:
        ret
getnum endp        


;-------------------------------------------------------------------------------
;
;Check for "CAUSEWAY","TEMP" and "TMP" environment variables and fetch any relevant settings.
;
GetENVStuff     proc    near
        mov     es,RealENVSegment
        xor     si,si
cw10_0:                                 ;<--- scan next line in environment
        mov     eax,es:[si]
        cmp     eax,"SUAC"
        jz      cw10_causeway
        cmp     eax,"PMET"
        jz      cw10_temp
        cmp     eax,"=PMT"
        jz      cw10_tmp
cw10_1:
        inc     si
cw10_skipline:
        cmp     BYTE PTR es:[si-1],0
        jnz     cw10_1
        cmp     BYTE PTR es:[si],0      ;end of all strings?
        jnz     cw10_0
        ret
        ;
cw10_causeway:
        mov     eax,es:[si+4]
        cmp     eax,"YAWE"
        jnz     cw10_skipline
        cmp     byte ptr es:[si+8],'='
        jnz     cw10_skipline

        ;Found "CAUSEWAY" so have a look at the settings.
        ;
        add     si,9                    ;skip past "="
        push    si
cw10_2_0:
        mov     al,es:[si]
        cmp     al,'a'
        jb      cw10_2_1
        cmp     al,'z'
        ja      cw10_2_1
        and     al,5Fh                  ;convert to upper case.
cw10_2_1:
        mov     es:[si],al
        inc     si
        or      al,al
        jnz     cw10_2_0
        pop     si
        ;
cw10_3:                                 ;<---- continue scan "CAUSEWAY"
        cmp     BYTE PTR es:[si]," "
        jnz     cw10_4
        inc     si
        jmp     cw10_3
        ;
cw10_4: cmp     BYTE PTR es:[si],0      ;end of line?
        jz      cw10_9
        ;
        mov     eax,es:[si]
        cmp     eax,"MVON"              ;NOVM?
        jz      cw10_novm
        cmp     eax,"MXAM"              ;MAXMEM?
        jz      cw10_maxmem
        cmp     eax,"ATXE"              ;EXTALL?
        jz      cw10_extall
        cmp     eax,"IMPD"              ;DPMI?
        jz      cw10_dpmi
        cmp     eax,"PAWS"              ;swap?
        jz      cw10_swap
        cmp     eax,"MWOL"              ;lowmem?
        jz      cw10_lowmem
        cmp     eax,"EMIH"              ; himem?
        jz      cw10_himem
        cmp     eax,"APON"              ; nopass?
        jz      cw10_nopass
        cmp     eax,":ERP"              ; pre?
        jz      cw10_pre
        cmp     eax,"EMAN"              ; NAME?
        jz      cw10_name
        cmp     eax,"1DAP"              ; PAD1?
        jz      cw10_pad1
        cmp     eax,"XEON"              ; NOEX?
        jz      cw10_noex
        cmp     eax,"1GIB"              ; BIG1?
        jz      cw10_big1
;--- just continue with next char?
        inc     si
        jmp     cw10_3
        ;
cw10_nopass:
        ; shut off passing of real mode interrupts to protected mode
        add     si,4
        mov     ax,es:[si]
        cmp     ax,"SS"
        jnz     cw10_3
        add     si,2
        or      NoPassFlag,-1
        jmp     cw10_3

cw10_big1:
        ; specify alternate extended memory size computation
        add     si,4
        or      Big1Flag,-1
        jmp     cw10_3

cw10_himem:
        ;HIMEM:xxx - Set amount of physical memory to use.
        ;
        add     si,4
        mov     al,es:[si]
        cmp     al,"M"
        jnz     cw10_3
        inc     si
        cmp     BYTE PTR es:[si],":"
        jnz     cw10_3
        inc     si
        call    getnum
        cmp     edx,4096*1024
        jnc     cw10_3
        shl     edx,10                  ;turn K into byte's
        shr     edx,12                  ;get number of pages.
        mov     [MaxMemPhys],edx
        jmp     cw10_3
        ;
cw10_extall:
        ;Set flag to use all extended memory.
        ;
        add     si,4
        mov     ax,es:[si]
        cmp     ax,"LL"
        jnz     cw10_3
        add     si,2
        or      ExtALLSwitch,-1
        jmp     cw10_3
        ;
cw10_novm:
        ;They want to disable VM.
        ;
        add     si,4
        or      NoVMSwitch,-1
        jmp     cw10_3
        ;
cw10_maxmem:
        ;MAXMEM:xxx - Set maximum linear address space size.
        ;
        add     si,4
        mov     ax,es:[si]
        cmp     ax,"ME"
        jnz     cw10_3
        add     si,2
        cmp     BYTE PTR es:[si],":"
        jnz     cw10_3
        inc     si
        call    getnum
        cmp     edx,4096                ;4096MB or more?
        jnc     cw10_3
        shl     edx,20                  ;turn Meg into byte's
        mov     [MaxMemLin],edx
        jmp     cw10_3

cw10_pre:
        ;PRE:xxx - Want to set preallocate amount
        ;
        add     si,4
        call    getnum
        cmp     edx,4096
        jnc     cw10_3

        shl     edx,20                  ;turn Meg into byte's
        mov     d[PreAllocSize],edx
        jmp     cw10_3

cw10_pad1:
        mov     Pad1Flag,1 ; accessible thru FS
        add     si,4
        jmp     cw10_3

cw10_noex:
        mov     NoEXECPatchFlag,1 ; accessible thru FS
        add     si,4
        jmp     cw10_3

cw10_dpmi:
        ;They want to force DPMI use if possible.
        ;
        mov     ProtectedForce,1 ; accessible thru FS
        add     si,4
        jmp     cw10_3
        ;
cw10_swap:
        ;They want to specify the swap drive.
        ;
        add     si,4
        cmp     BYTE PTR es:[si],":"
        jnz     cw10_3
        inc     si
        mov     di,offset VMMDrivPath1
cw10_s0:
        mov     al,es:[si]
        mov     [di],al
        inc     si
        inc     di
        or      al,al
        jz      cw10_s1

        cmp     al,";"
        je      cw10_s1

        cmp     al," "
        jnz     cw10_s0
cw10_s1:
        mov     b[di-1],0
        dec     si
        jmp     cw10_3
        ;
cw10_name:
        ; Specify the swap name.
        ;
        add     si,4
        cmp     BYTE PTR es:[si],":"
        jnz     cw10_3
        inc     si
        mov     di,offset DesiredVMMName
        xor     dx,dx
cw10_n0:
        mov     al,es:[si]
        mov     [di],al
        inc     si
        inc     di
        or      al,al
        jz      cw10_n1
        inc     dx
        cmp     dx,12
        ja      cw10_n1                 ; don't allow more than 12 chars in file name

        cmp     al,";"
        je      cw10_n1

        cmp     al," "
        jnz     cw10_n0
cw10_n1:
        mov     b[di-1],0
        dec     si
        jmp     cw10_3
        ;
cw10_lowmem:
        ;They want to specify low memory retention.
        ;
        add     si,4
        mov     ax,es:[si]
        cmp     ax,"ME"
        jnz     cw10_3
        add     si,2
        cmp     BYTE PTR es:[si],":"
        jnz     cw10_3
        inc     si
        call    getnum
        shl     edx,10-4                ;turn K into para's
        movzx   ebx,w[CONVSaveSize]
        add     edx,ebx
        cmp     edx,65535
        jc      cw10_lm2
        mov     edx,65535
cw10_lm2:
        mov     w[CONVSaveSize],dx      ;set new size.
        jmp     cw10_3
        ;
cw10_9:                                 ;done scanning CAUSEWAY var
        jmp     cw10_skipline
        ;
cw10_temp:
        add     si,4
        cmp     BYTE PTR es:[si],"="
        jnz     cw10_skipline
        inc     si
        mov     di,offset VMMDrivPath2
cw10_temp3:
        mov     al,es:[si]
        mov     [di],al
        inc     si
        inc     di
        or      al,al
        jz      cw10_temp4
        cmp     al," "
        jnz     cw10_temp3
cw10_temp4:
        mov     b[di-1],0
        jmp     cw10_skipline
        ;
cw10_tmp:
        add     si,4
        mov     di,offset VMMDrivPath3
cw10_tmp3:
        mov     al,es:[si]
        mov     [di],al
        inc     si
        inc     di
        or      al,al
        jz      cw10_tmp4
        cmp     al," "
        jnz     cw10_tmp3
cw10_tmp4:
        mov     b[di-1],0
        jmp     cw10_skipline
GetENVStuff     endp

;-------------------------------------------------------------------------------
;--- get name of binary from environment

GetEXECName     proc    near

        mov     es,RealENVSegment
        xor     si,si
cw11_1:
        mov     al,es:[si]              ;Get a byte.
        inc     si                      ;/
        or      al,al                   ;End of a string?
        jnz     cw11_1                  ;keep looking.
        mov     al,es:[si]              ;Double zero?
        or      al,al                   ;/
        jnz     cw11_1                  ;keep looking.
        add     si,3                    ;Skip last 0 and word count.
        mov     di,offset MainExec
        mov     bx,offset VMMDrivPath4
        mov     cx,sizeof VMMDrivPath4
        mov     dx,bx
cw11_2:
        lodsb   es:[si]
        mov     [di],al
        mov     [bx],al
        inc     di
        inc     bx
        cmp     al,"\"
        jnz     cw11_2_0
        mov     dx,bx
        dec     dx
cw11_2_0:
        cmp     al,0                    ;got to the end yet?
        loopnz  cw11_2
        mov     bx,dx
        mov     BYTE PTR [bx],0         ;terminate VMM path.

COMMENT !
; MED 10/10/96
        cmp     bx,offset VMMDrivPath4
        jne     genexit                 ; non-null path
        mov     BYTE PTR [bx+0],'.'     ; null path, give a valid one of '.\'
        mov     BYTE PTR [bx+1],'\'
        mov     BYTE PTR [bx+2],0
genexit:
END COMMENT !

        ret

GetEXECName     endp


;-------------------------------------------------------------------------------
;--- real-mode proc
;--- DS:GROUP16

        assume ds:GROUP16

GetSystemFlags  proc    near
        push    ds
        mov     dx,offset MainExec
        mov     ax,3d40h                ;open, read only, deny none
        int     21h
        jc      cw12_5
        mov     bx,ax
        mov     dx,offset IExeHdr       ;somewhere to put the info.
        mov     cx,sizeof MZHdr         ;size of it.
        mov     ah,3fh
        int     21h
        jc      cw12_4
        cmp     ax,sizeof MZHdr         ;did we read right amount?
        jnz     cw12_4
        cmp     IExeHdr.Signature,'ZM'  ;Normal EXE?
        jnz     cw12_4
        mov     ax,IExeHdr._Length+2    ;get length in 512 byte blocks

; MED 01/17/96
        cmp     IExeHdr._Length,0
        je      medexe2                 ; not rounded if no modulo

        dec     ax                      ;lose 1 cos its rounded up

medexe2:
        add     ax,ax                   ;mult by 2
        mov     dh,0
        mov     dl,ah
        mov     ah,al
        mov     al,dh                   ;mult by 256=*512
        add     ax,IExeHdr._Length      ;add length mod 512
        adc     dx,0                    ;add any carry to dx
        mov     cx,ax
        xchg    cx,dx                   ;swap round for DOS.
        mov     ax,4200h                ;set absolute position.
        int     21h
        mov     dx,offset INewHeader    ;somewhere to put the info.
        mov     cx,size NewHeaderStruc  ;size of it.
        mov     ah,3fh
        int     21h
        jc      cw12_4
        or      ax,ax                   ;end of the file?
        jz      cw12_SetRUN
        cmp     ax,size NewHeaderStruc  ;did we read right amount?
        jnz     cw12_4
        cmp     w[INewHeader],'P3'      ;ID ok?
        jnz     cw12_4
        mov     si,offset INewHeader
        mov     ax,w[si].NewHeaderStruc.NewFlags+0  ;Copy main flags.
        mov     cx,w[si].NewHeaderStruc.NewFlags+2
        mov     w[SystemFlags+0],ax
        mov     w[SystemFlags+2],cx
        mov     w[RawSystemFlags+0],ax
        mov     w[RawSystemFlags+2],cx
        .386
        mov     dx,_cwDPMIEMU
        mov     ds,dx
        assume ds:_cwDPMIEMU
        mov     w[DpmiEmuSystemFlags+0],ax
        mov     w[DpmiEmuSystemFlags+2],cx
        mov     dx,GROUP32
        mov     ds,dx
        assume ds:GROUP32
        mov     w[apiSystemFlags+0],ax
        mov     w[apiSystemFlags+2],cx
        .286
cw12_4:
        mov     ah,3eh
        int     21h
        jmp     cw12_5
;
;Nothing on the end of the extender so replace the exec name with first
;command line argument and shuffle everything else down. Allows CW32 to be used
;to run 32-bit programs not attached to it from the command line.
;
        assume ds:GROUP16

cw12_SetRUN:
        mov     ah,3eh                  ;close file, we don't need it.
        int     21h
        push    es
        mov     es,RealPSPSegment
        mov     si,80h
        xor     ch,ch
        mov     cl,BYTE PTR es:[si]
        jcxz    cw12_sr5
        inc     si
        mov     di,offset MainExec      ;default to storeing program name.
        ;
        ;Skip white space.
        ;
cw12_sr0:
        mov     al,es:[si]
        cmp     al," "
        jnz     cw12_sr1
        inc     si
        loop    cw12_sr0
        jmp     cw12_sr3
        ;
        ;Get program name.
        ;
cw12_sr1:
        mov     al,es:[si]
        cmp     al," "
        jz      cw12_sr2
        mov     BYTE PTR es:[si],' '
        mov     [di],al
        inc     si
        inc     di
        loop    cw12_sr1
cw12_sr2:
        mov     b[di],0
cw12_sr3:
        pop     es
;
;Clean up the command line, ie, remove any spaces created by removeing name.
;
        push    es
        mov     es,RealPSPSegment
        assume es:_cwEnd
        mov     si,80h
        xor     ch,ch
        mov     cl,BYTE PTR es:[si]
        jcxz    cw12_cl3
        inc     si
        mov     di,si
cw12_cl0:
        cmp     BYTE PTR es:[si],' '
        jnz     cw12_cl1
        inc     si
        loop     cw12_cl0
cw12_cl1:
        jcxz    cw12_cl2
        push    cx
        push    ds

        push    es
        pop     ds
        rep     movsb                   ;Copy it down.

        pop     ds
        pop     cx
cw12_cl2:
        mov     BYTE PTR es:[80h],cl    ;Store new length.
cw12_cl3:
        xor     ch,ch
        add     cx,81h
        mov     si,cx
        mov     BYTE PTR es:[si],13     ;Terminate it correctly.
cw12_sr5:
        pop     es
        ;
cw12_5:
        pop     ds
        ret
GetSystemFlags  endp


;-------------------------------------------------------------------------------
GetProtectedType proc near
;
;Find out what protected mode environments are available.
;
        call    ChkDPMI                 ;32 bit DPMI server present?
        jc      cw13_0
        or      ProtectedFlags,1
cw13_0:
        call    ChkVCPI                 ;VCPI >= v1.0 present?
        jc      cw13_1
        or      ProtectedFlags,2
cw13_1:
        call    ChkRAW                  ;Running in real mode?
        jc      cw13_2
        or      ProtectedFlags,4
cw13_2:
        ret
GetProtectedType endp


;-------------------------------------------------------------------------------
SetProtectedType proc near
        cmp     ProtectedForce,0
        jz      cw14_NoDPMIForce
        test    BYTE PTR ProtectedFlags,1  ; DPMI available?
        jnz     cw14_2
        ;
cw14_NoDPMIForce:
        test    BYTE PTR ProtectedFlags,4  ; XMS/RAW available?
        jz      cw14_1
        mov     ax,PT_RAWXMS               ; Use raw mode.
        jmp     cw14_3
cw14_1:
        test    BYTE PTR ProtectedFlags,2  ; VCPI available?
        jz      cw14_2
        mov     ax,PT_VCPI
        jmp     cw14_3
cw14_2:
        mov     ax,PT_DPMI
cw14_3:
        mov     ProtectedType,ax
        ret
SetProtectedType endp


;-------------------------------------------------------------------------------
ChkDPMI proc    near
;
;See if DPMI server present.
;
        mov     ax,1687h                ;DPMI instalation check.
        int     2fh
        or      ax,ax                   ;None-zero means its not there.
        jnz     cw15_9
        test    w[SystemFlags],1
        jz      cw15_Use32Bit21
        jmp     cw15_Use16Bit21
cw15_Use32Bit21:
        test    bx,1                    ;Must offer 32 bit support.
        jz      cw15_9
cw15_Use16Bit21:
        clc
        ret
        ;
cw15_9: stc
        ret
ChkDPMI endp


;-------------------------------------------------------------------------------
;
; The following routine checks to see if a VCPI master program is installed.
; If one is not, the carry flag is set on return
; If one is, the version info is stored and the carry flag is cleared on return
;
ChkVCPI proc    near
        push    ax
        push    bx
        push    es
        xor     ax,ax
        mov     es,ax
        mov     si,es:[67h*4+0]           ;Check a handler exists.
        mov     di,es:[67h*4+2]
        mov     ax,si
        or      ax,di
        jnz     cw16_IsHandler
        cli
        mov     WORD PTR es:[67h*4+0],offset cw16_DummyIRET
        mov     WORD PTR es:[67h*4+2],cs
cw16_IsHandler:
        push    si
        push    di
        mov     ax,0DE00h               ;Get VCPI installed state
        int     67h
        pop     di
        pop     si
        mov     es:[67h*4+0],si         ;restore int 67h
        mov     es:[67h*4+2],di
        assume es:nothing
        sti
        ;
        cmp     al,0
        jne     cw16_NotThere
        or      bx,3030h                ;Turn to ASCII
        cmp     bh,'1'
        jc      cw16_NotThere
        mov     ax,si
        or      ax,di                   ;Only pretending to be there?
        jz      cw16_HopeThere
        ;
        call    cw16_ChkEMS             ;Make sure EMS is in first
        jc      cw16_HopeThere          ;EMS not in.
        call    cw16_GrabPage           ;Make sure EMS initiated
cw16_HopeThere:
        clc                             ;Set for no error
        jmp     cw16_Done
cw16_NotThere:
        stc
cw16_Done:
        pop     es
        pop     bx
        pop     ax
        ret
cw16_DummyIRET:
        iret
;
; The following routine checks to see if an EMM is installed.
; If one is not, the carry flag is set on return
; If one is, the carry flag is cleared on return
;
cw16_ChkEMS:
        push    ax
        push    bx
        push    dx
        push    es
        push    ds
        push    cs
        pop     ds
        mov     dx,offset cw16_EMSName  ;Device driver name
        mov     ah,3Dh                  ;Open file
        mov     al,0                    ;Access/file sharing mode
        int     21h
        pop     ds
        jc      cw16_NotThere2
        mov     bx,ax                   ;Put handle in proper place
        mov     ah,44h                  ;IOCTL
        mov     al,07h                  ;Get output status
        int     21h
        jc      cw16_NotThere1
        cmp     al,0FFh
        jne     cw16_NotThere1
        mov     ah,3Eh                  ;Close file
        int     21h
        clc                             ;Set for no error
        jmp     cw16_Done1
cw16_NotThere1:
        mov     ah,3Eh                  ;Close file
        int     21h
cw16_NotThere2:
        stc
cw16_Done1:
        pop     es
        pop     dx
        pop     bx
        pop     ax
        ret
;
; This function allocates an EMS page, and then releases it.  This is
; done to make sure the EMS driver has switched the CPU to V86 mode.
; On return, the carry is set if there was any problem using the EMS
; functions.  Carry is clear otherwise.
;
cw16_GrabPage:
        mov     ah,43h                  ;Allocate pages
        mov     bx,1                    ;Get 1 page (16K)
        int     67h
        cmp     ah,0                    ;Was there an error?
        jne     cw16_GPErr              ;Yes, so exit
        mov     ah,45h                  ;Release EMS handle
        int     67h
        cmp     ah,0                    ;Was there an error?
        jne     cw16_GPErr              ;Yes, so exit
        clc                             ;Mark for no error
        jmp     cw16_GPEnd
cw16_GPErr:
        stc
cw16_GPEnd:
        ret
;
cw16_EMSName    DB 'EMMXXXX0',0
ChkVCPI endp


;-------------------------------------------------------------------------------
ChkRAW  proc    near
;
;Can we run on this machine.
;
        .286P
        smsw    ax
        and     ax,1                    ; are we in protected mode?
        jnz     cw17_9
        clc
        ret
cw17_9: stc
        ret
ChkRAW  endp


;-------------------------------------------------------------------------------
MakeDesc        proc    near
;
;Build a segment descriptor.
;
;On Entry:-
;
;ES:DI  - Descriptor entry to use.
;ESI    - Linear base to set
;ECX    - limit in bytes
;AL     - Code size bit.
;AH     - Present/PL/memory|system/type bits.
;
        .386
        pushad
        and     di,not 7                ;lose RPL & TI
        cmp     ecx,0100000h            ; see if we need to set g bit
        jc      cw18_0
        shr     ecx,12                  ; div by 4096
        or      al,80h                  ; set g bit
cw18_0: mov     es:[di],cx              ;store low word of limit.
        shr     ecx,16
        or      cl,al
        mov     es:[di+6],cl            ;store high bits of limit and gran/code size bits.
        mov     es:[di+2],si            ;store low word of linear base.
        shr     esi,16
        mov     bx,si
        mov     es:[di+4],bl            ;store mid byte of linear base.
        mov     es:[di+7],bh            ;store high byte of linear base.
        mov     es:[di+5],ah            ;store pp/dpl/dt/type bits.
        popad
        ret
MakeDesc        endp


;-------------------------------------------------------------------------------
;
;Build a gate descriptor.
;
;On Entry:-
;
;ES:EDI - Descriptor entry to use.
;SI     - selector 
;ECX    - offset
;AL     - 00
;AH     - Present/PL/memory|system/type bits.
;
MakeGate       proc    near
        .386
        push    ecx
        mov     es:[edi].GATE.OfsLow, cx
        mov     es:[edi].GATE.sel,si    ;store low word of linear base.
        shr     ecx, 16
        mov     es:[edi].GATE.attr,ax   ;store pp/dpl/dt/type bits.
        mov     es:[edi].GATE.OfsHigh,cx
        pop     ecx
        ret
MakeGate       endp

;--- PL0 proc to (re)load IDT

loadidt proc far
        .386p
        assume ds:GROUP16
        lidt [IDTVal]
        assume ds:nothing
        .386
        retd   ; called thru a 32-bit call gate!
loadidt endp

setcr0cr4 proc
; MED 12/04
; check if CPUID is available, if so, check if need to enable SSE instructions
        pushfd
        pop     eax
        mov     ecx,eax
        xor     eax,200000h             ; toggle cpu id bit
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax,ecx                 ; see if cpu id bit was changed
        je      nosse                   ; no, cpuid instruction not supported

        .586p
        mov     eax,1
        cpuid
        and     edx,3000000h            ; only want SSE and FXSR bit status
        cmp     edx,3000000h
        jne     nosse                   ; both bits required
        mov     eax,cr0
        and     al,NOT 6                ; clear EM and MP bits
        mov     cr0,eax
        mov     eax,cr4
        or      ax,200h                 ; set OSFXSR bit to allow SSE instructions
        mov     cr4,eax
nosse:
        cld
        clts
        retd
setcr0cr4 endp

_cwInit ends

        .386p
        include rawvcpi2.inc

_TEXT32 segment para use32 public 'CODE'
_TEXT32 ends
_DATA32 segment para use32 public 'DATA'
_DATA32 ends

_TEXT32 segment
        include exceptn.inc
        include api.inc
        include int10h.inc
        include int21h.inc
        include int33h.inc
_TEXT32 ends

_DATA32 segment
endGroup32 equ $
_DATA32 ends

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;End marker so we know what to resize program memory size to initialy.
;
_cwEnd  segment para public 'end marker' use16
_cwEnd  ends

        end     Startup