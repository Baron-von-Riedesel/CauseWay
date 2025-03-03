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

b       equ <byte ptr>
w       equ <word ptr>
d       equ <dword ptr>

;--- values for variable ProtectedType
PT_RAWXMS equ 0
PT_VCPI   equ 1
PT_DPMI   equ 2

;--- bits in variable ProtectedFlags
PF_DPMI   equ 1     ;DPMI host detected
PF_VCPI   equ 2
PF_RAW    equ 4

;--- values for GDT/LDT bytestring (MDTLinear+4)
DT_FREE    equ 0	;free descriptor 
DT_LDTDESC equ 1	;LDT descriptor 
DT_GDTDESC equ 2	;GDT descriptor

;--- values for SystemFlags
SF_16BIT   equ 0001h  ;running a 16-bit app
SF_VMM     equ 0002h  ;VMM & swapfile present
;--- bits 000Ch are copied from ProtectedType
;--- bits 0070h are copied from ProtectedFlags
SF_GDT     equ 0080h  ;move GDT
SF_SINGLE  equ 4000h  ;single (=not dual) mode; or is it bit 16???
SF_PM      equ 8000h  ;running in protected-mode

PTMAPADDR   equ 0FFC00000h ;=1024*4096*1023, last page directory entry
PDEMAPDET   equ 1022       ;entry in page dir, address range FF800000-FFBFFFFF 
MAINSTKSIZE equ 2048       ;stack size of PL3 stack during init/exit (segment _cwStack)

VIDEOGDTSELS     equ 1	;1=define selectors A000h,B000h and B800h (an odd CauseWay peculiarity)
SMARTRMALLOC     equ 1	;1=if conv. memory couldn't be alloc'd in an UMB, it will use space behind transient area
MOVEPAGE1STTOEXT equ 1	;1=move page table for region 0-3fffff to extended memory
MOVETSS          equ 1	;1=move TSS to extended memoy (behind IDT)
RELXMSINRM       equ 1	;1=release xms memory handles after final switch to real-mode
VCPIPMCALL       equ 1	;1=alloc/release vcpi pages via protected-mode VCPI call
ifndef EARLYKDINIT
EARLYKDINIT      equ 1	;1=init KD very early after switch to protected-mode
endif
;DPMIDBG          equ 1	;1=emulate DPMI initial switch to pm to allow DEBUG to intrude in raw/vcpi mode

IRET16 struc
_ip		dw ?
_cs		dw ?
_fl		dw ?
IRET16 ends
IRET32 struc
_ip		dd ?
_cs		dd ?
_fl		dd ?
IRET32 ends

SDA struct
		db ?
bInDos	db ?
		db 0Ch - $ dup (?)
dwDTA	dd ?
wPSP	dw ?
		dw ?,?
bDrive	db ?
bBreak	db ?	;+17h
SDA ends

GROUP16 group _cwMain, _cwRaw, _cwRaw$1, _cwInit
GROUP32 group _TEXT32, _DATA32

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Main code segment. This takes care of calling the right initialisation routines
;and generaly getting everything rolling.
;
_cwMain segment para public 'Main code' use16

;-------------------------------------------------------------------------------
;
;Some global data.
;
RealPSPSegment  dw ?            ;Real mode PSP segment.
RealENVSegment  dw ?            ;Real mode environment segment.
ProtectedFlags  dw 0            ;Bit significant, 0-DPMI,1-VCPI,2-RAW.
ProtectedType   dw 0            ;0-RAW/XMS,1-VCPI,2-DPMI.
SystemFlags     dw 0,0
grp32Proc label fword
grp32Ofs        dd 0
Group32CS       dw 0            ;code selector GROUP32
Group32DS       dw 0            ;data selector GROUP32
;
CodeSegment     dw MainCS       ;CS selector for GROUP16.
DataSegment     dw MainDS       ;DS selector for GROUP16
StackSegment    dw MainSS       ;_cwStack, used for init/exit
RealSegment     dw KernalZero   ;4G zero-based data selector
PSPSegment      dw MainPSP
ENVSegment      dw MainEnv
BasePSP         dw 0            ;selector of base PSP
BasePSPAddress  dd 0            ;linear address of base PSP
;
TSREnd          dw _cwInit      ;TSR end para for Raw/VCPI mode
;
ErrorNumber     dw 0
ErrorLevel      dw 0
	ALIGN 2
;MainExec        db 128 dup (0)
;
DtaBuffer       db 128 dup (0)
;
;TransferSize    dd 8192
TransferSize    equ 8192
TransferReal    dw ?       ; 8k transfer buffer real-mode segment address
;
	align 4
ResourceTracking dd 0      ; flag 0/-1, dword size cause it's pushed/poped
ForcedFind       dd 0,0    ; used by FindResource/ReleaseResource
mcbAllocations   db 0      ; mcb allocations on/off
LinearAddressCheck db 0    ; 1=check for valid linear address active (see int 31h, ax=fffch)
DebugDump        db 0      ; flag to display a state dump on exit
Pad1Flag         db 0      ; CAUSEWAY environment setting "PAD1"

;
	align 4
TerminationHandler label fword
	dd offset InitError
	dw MainCS, 0

;--- segment _Excep is now 32-bit; this requires a proper definition of a 16-bit far pointer
;--- called from within a 32-bit code segment.
PF16 typedef far16 ptr

;
ifdef SRDPMISTATE
DPMIStateAddr   df 0       ; DPMI address save/restore state
DPMIStateSize   dd 0
endif
;
IFDEF PERMNOEX
NoEXECPatchFlag DB 1       ; same as CAUSEWAY=NOEX - don't hook int 21h in real-mode to trap ax=4B00h
ELSE
NoEXECPatchFlag DB 0
ENDIF

; MED, 11/11/99
; used to flag checking XMS total memory because EMM386 lies and acts as VCPI
;  host when NOVCPI set, but provides no memory
VCPIHasNoMem    DB 0
apiExcepPatched db 0
;
	align 4

EXTENSION struct
pInit   df ?            ;+0  init code.
pExit   df ?            ;+6  remove code.
wFlgs   dw ?            ;+12 installed flag.
EXTENSION ends
;
ExtensionStart label EXTENSION
ExceptionExtension EXTENSION < offset ExcepOpen,  offset ExcepClose,  0>
Int21hExtension    EXTENSION < offset Int21hOpen, offset Int21hClose, 0>
Int10hExtension    EXTENSION < offset Int10hOpen, offset Int10hClose, 0>
Int33hExtension    EXTENSION < offset Int33hOpen, offset Int33hClose, 0>
ExtensionEnd label byte
;
;--- user-defined dump
DebugUserOff    DD      ?
DebugUserSel    DW      ?
DebugUserCount  DW      0       ; must be initialized, nonzero value flags operation
DebugAsciiFlag  DB      ?

ifndef NOI21RMHOOK
Int21hExecCount db 0
	align 4
OldInt21hExec   dd 0        ; real-mode address
endif

        .386

ifdef _DEBUG
        include dprint16.inc
else
@dprintf equ <;>
endif

;-------------------------------------------------------------------------------
;--- int 21h real-mode handler, installed by int21h.inc extension

ifndef NOI21RMHOOK
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

Int21hExecPatch endp
endif

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
DOSVersion      dw ?

LowMemory label dword         ; Set equal to 0000:0080
                dw 00080h
                dw 00000h
HighMemory label dword
                dw 00090h     ; Set equal to FFFF:0090
                dw 0FFFFh
;
;--- there's also a MZHeader struct in api.inc, size 64!
MZHdr struct
Signature	dw ?	;00 Identifier text 'MZ', '3P'.
_Length		dw ?	;02 Length of file MOD 512
NumPages	dw ?	;04 Length of file in 512 byte blocks.
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

INewHeader  NewHeaderStruc <>
	org INewHeader
IExeHdr     MZHdr <>
	org INewHeader + NewHeaderStruc

	align 2
;
IErrorNumber    dw 0
InitErrorList   label word
        dw IErrorM00,IErrorM01,IErrorM02,IErrorM03,IErrorM04,IErrorM05,IErrorM06,IErrorM07
        dw IErrorM08,IErrorM09
SELECTTEXT = 2
        include texts.inc
;
IFDEF PERMNOVM
NoVMSwitch      db 1
ELSE
NoVMSwitch      db 0
ENDIF

MainExec        db 128 dup (0)  ;.exe file name
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
;cw5_NumXMSHandles db ?
IProtectedMode    db 0
IProtectedForce   db 0          ;CAUSEWAY environment setting "DPMI"
;
;
;-------------------------------------------------------------------------------
Startup proc    near
;
;Make global data addresable.
;
        .286
;        mov     ax,GROUP16
        mov     ax,cs
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
        assume es:_cwEnd
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
        jc      toiniterr
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
        call    GetProtectedFlags       ;set variable ProtectedFlags
        mov     IErrorNumber,3
        cmp     ProtectedFlags,0        ;Any types available?
        jz      toiniterr
;
;Get CAUSEWAY environment variable settings.
;
        call    GetENVStuff
;
;Decide which environment to use.
;
        call    GetProtectedType
        mov     ProtectedType,ax
;
;Move the DTA to where we can get at it in the future.
;
        mov     dx,offset DtaBuffer
        mov     ah,1ah
        int     21h
;
;Change DOS allocation strategy to highest so we'll get UMB's if available.
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
;Get SDA address so VMM can change BREAK state; v5.0: to allow VMM to query indos flag.
;
        push    ds
        mov     ax,5d06h                ;get SDA in DS:SI
        int     21h
        mov     ax,ds
        pop     ds
        add     si,17h                  ;offset 17h=extended BREAK flag
        movzx   eax,ax
        shl     eax,4
        movzx   esi,si
        add     eax,esi
        mov     SDAaddress,eax
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
        mov     ah,89h                  ;claim biggest block to force XMS
        call    [XMSControl]            ;to stake a claim on int 15h.
        cmp     ax,1
        jnz     cw5_YesXMS
        mov     di,1
;        mov     [cw5_NumXMSHandles],1
        push    dx
        mov     ah,8eh                  ; get handle information
        call    [XMSControl]            ; CX=free handles
        cmp     ax,1
        jnz     cw5_NoHandles3
        cmp     cx,4
        jc      cw5_NoHandles3
        sub     cx,2
;        mov     [cw5_NumXMSHandles],32
        mov     di,XMSITEMS
        cmp     cx,bx
        jnc     cw5_NoHandles3
;        mov     [cw5_NumXMSHandles],cl  ; cx known 8-bit value
        mov     di,cx

cw5_NoHandles3:
;        mov     [cw5_NumXMSHandles],di
        pop     dx
        mov     ah,0ah
        call    [XMSControl]            ;now free it.

;        movzx   eax,[cw5_NumXMSHandles]
        movzx   eax,di
        mov     edx,eax
        shl     eax,16
        sub     eax,edx                 ; eax == handles (up to 32) * 65535
        cmp     eax,[cw5_XMSSize]
        jae     cw5_ComputeSize
        mov     [cw5_XMSSize],eax    ; throttle maximum size

cw5_ComputeSize:
        push    eax
        xor     edx,edx
;        movzx   ebx,[cw5_NumXMSHandles]
        movzx   ebx,di
        div     ebx
        pop     ebx
        cmp     ax,4                    ; eax known 16-bit value
        jnc     cw5_SizeOK3
        mov     ax,bx                   ; ebx == maximum size, known 16-bit value here

cw5_SizeOK3:
        mov     XMSBlockSize,ax
        jmp     cw5_YesXMS

;--- handle XMS v2 host
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
;        mov     [cw5_NumXMSHandles],1
        mov     di,1
        push    dx
        mov     ah,0eh                  ;get handle info
        call    [XMSControl]            ;BL=# of free handles
        cmp     ax,1
        jnz     cw5_NoHandles
        cmp     bl,4
        jc      cw5_NoHandles
        sub     bl,2
;        mov     [cw5_NumXMSHandles],32
        mov     di,XMSITEMS
        mov     bh,0
        cmp     bx,di
        jnc     cw5_NoHandles
;        mov     [cw5_NumXMSHandles],bl
        mov     di,bx
cw5_NoHandles:
        pop     dx
        mov     ah,0ah
        call    [XMSControl]            ;now free it.
        mov     ax,WORD PTR [cw5_XMSSize]
        push    ax
        xor     dx,dx
;        xor     bh,bh
;        mov     bl,[cw5_NumXMSHandles]
        mov     bx,di
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
        call    InstallA20              ; set A20HandlerCall (called by A20Handler)
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
        @dprintf DOPT_DOSMEM,<"DOS mem for paging tables: %X",10>,ax
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
        @dprintf DOPT_DOSMEM,<"DOS mem for paging tables: %X",10>,ax
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
        @dprintf DOPT_DOSMEM,<"DOS mem for kernal TSS: %X",10>,ax
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
        @dprintf DOPT_DOSMEM,<"DOS mem for GDT: %X",10>,ax
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
        @dprintf DOPT_DOSMEM,<"DOS mem for kernal stack: %X",10>,ax
        mov     es,ax
if SMARTRMALLOC
;--- if allocation is in low memory, skip it
;--- and (re)use part of _cwInit as stack.
        cmp ax, 0A000h
        jae @F
        mov ah, 49h
        int 21h
        mov ax, TSREnd
        mov RawStackReal, ax
        add TSREnd, RawStackTotal/16
        mov ax, offset _cwInit:cw_safesp
        and al, 0fch
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
;in VCPI mode, this will be overwritten by int 67h, ax=DE01h
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
        mov     al,b[SystemFlags]
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
;KernalDS  - Kernel (RAW) data seg (must be 64k!).
;KernalCS  - Kernel (RAW) code seg.
;
        mov     esi,GROUP16
        shl     esi,4
        mov     cx,65535
        xor     al,al
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,KernalCS0
        call    MakeDesc

        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,KernalDS
        call    MakeDesc

if 0
        mov     cx,_cwDPMIEMU
        sub     cx,GROUP16
        shl     cx,4
        dec     cx
endif
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,KernalCS
        call    MakeDesc

;
;KernalSS - (RAW) stack seg PL3.
;
        movzx   esi,RawStackReal
        shl     esi,4
;        mov     ecx,[]65535
        mov     ecx,RawStackTotal-1
        mov     al,b[SystemFlags]
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
        mov     esi,DPMIGRP
        shl     esi,4
        mov     ecx,offset cwDPMIEMUEnd - 1
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,DpmiEmuCS
        call    MakeDesc
;
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        mov     di,DpmiEmuDS
        call    MakeDesc
;
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,DpmiEmuCS0
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
        mov     al,b[SystemFlags]
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
;Make sure A20 is enabled.
;
        mov     IErrorNumber,7
        mov     ax,1
        call    A20Handler
        jnz     InitError
;
;Raw/VCPI: ready to switch to protected mode at last
;
        cli
        mov     cx,MainSS
        mov     edx,offset _cwStackEnd
        push    MainCS
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
        mov     ax,offset setupkd
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
        or      [SystemFlags],8000h     ;Flags us in protected mode.
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
cw_safesp:                  ;space up to this point may be used for rm stack while _cwInit code is still running
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
        mov     ebp, offset cwDPMIEMUEnd
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
        mov     esi,DPMIGRP
        shl     esi,4                   ;Point to the source.
        mov     ecx,offset cwDPMIEMUEnd
        shr     ecx,2
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd [edi],[esi]       ;Copy it up their.
        pop     ds
;
;Reinit DPMI emulator selectors after code has been moved.
;
        push    es
        mov     esi,eax
        mov     ax,GDTData
        mov     es,ax
        mov     ecx,offset cwDPMIEMUEnd - 1
        mov     al,1 shl 6
        mov     ah,DescPresent+DescPL3+DescMemory+DescERCode
        mov     di,DpmiEmuCS
        call    MakeDesc
        mov     ah,DescPresent+DescPL0+DescMemory+DescERCode
        mov     di,DpmiEmuCS0
        call    MakeDesc
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
;--- mark all GDT entries as "used" (including selector 0)
        mov     al,DT_GDTDESC
        mov     cl,GDT_Entries
        rep     stosb [edi]
;--- the rest is free
        mov     cx,(8192-GDT_Entries)
        mov     al,DT_FREE
        rep     stosb [edi]
        ;
        ;See which table we want to use.
        ;
        test    BYTE PTR SystemFlags,80h;GDT or LDT?
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

if 0 ; obsolete
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
endif

if VIDEOGDTSELS
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
        call    MakeDesc
        ;
        mov     di,KernalA000
        mov     esi,0a0000h
        call    MakeDesc
        pop     es

;--- mark the extra descriptors as used
        mov     esi,MDTLinear+4
        mov     al,DT_GDTDESC
        mov     es:[esi+(KernalA000 shr 3)],al
        mov     es:[esi+(KernalB000 shr 3)],al
        mov     es:[esi+(KernalB800 shr 3)],al
        ;
endif

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
        mov     edi,offset PageInt
        push    ds
        pop     es
        mov     [di].RealRegsStruc.Real_AX,1900h
        mov     RealRegsStruc.Real_SSSP[di],0
        mov     bx,21h
        xor     cx,cx
        mov     ax,300h
        int     31h
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
        mov     ax,3c00h                ; create file
        jmp     med5a1
med5a:
        push    di
        mov     si,di
        mov     ax,5a00h                ; create temporary file
med5a1:
        mov     edi,offset PageInt
        mov     [di].RealRegsStruc.Real_AX,ax
        push    ds
        pop     es
        mov     [di].RealRegsStruc.Real_DS,GROUP16
        mov     [di].RealRegsStruc.Real_DX,si
        mov     [di].RealRegsStruc.Real_CX,0
        mov     [di].RealRegsStruc.Real_SSSP,0
        mov     bx,21h
        xor     cx,cx
        mov     ax,300h
        int     31h
        test    BYTE PTR RealRegsStruc.Real_Flags[di],1
        mov     ax,RealRegsStruc.Real_AX[di]
        pop     di
        jz      cw5_v8
        ;
cw5_v7:
        add     di,128
        jmp     cw5_v0   ; try next entry
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
        assume ds:DPMIGRP
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
cw5_v9:
if 1 ;resize memory to 8k/12k
        mov     edi,offset PageInt
        mov     ax,wUMB
        mov     [di].RealRegsStruc.Real_ES,ax
        mov     [di].RealRegsStruc.Real_BX,(2000h+1000h)/16
        mov     [di].RealRegsStruc.Real_AX,4A00h
        mov     [di].RealRegsStruc.Real_SSSP,0
        push    ds
        pop     es
        mov     bx,21h
        xor     cx,cx
        mov     ax,300h
        int     31h
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
        mov     TSREnd, _cwRaw          ;cwRaw not needed for dpmi mode

        mov     IErrorNumber,5
        mov     bx,8192/16
        mov     ah,48h
        int     21h                     ;get memory for transfer buffer.
        jc      InitError
if SMARTRMALLOC
        cmp ax, 0A000h
        jae @F
        mov es,ax
        mov ah,49h
        int 21h
        mov ax, TSREnd
        add TSREnd, 8192/16
@@:
endif
        @dprintf DOPT_DOSMEM,<"DOS mem for transfer buffer: %X",10>,ax
        mov     TransferReal,ax
;
;Do installation check and get mode switch address.
;
        mov     IErrorNumber,9
        mov     ax,1687h                ;DPMI installation check.
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
        test    bx,1                    ;Must offer 32 bit support.
        jz      InitError
cw5_Use16Bit23:
        push    es                      ;Store the switch call address.
        push    di
        mov     bp,sp
        mov     bx,si                   ;DPMI buffer needed?
        or      bx,bx
        jz      cw5_d0
;
;Allocate memory for DPMI state save buffer.
;
        mov     IErrorNumber,5
        mov     ah,48h
        int     21h                     ;Try and claim memory for it.
        jc      InitError
        mov     es,ax
if SMARTRMALLOC
        cmp ax, 0A000h
        jae @F
        mov ah,49h
        int 21h
        mov es, TSREnd
        add TSREnd, bx
@@:
endif
        @dprintf DOPT_DOSMEM,<"DOS mem for DPMI buffer: %X",10>,es
cw5_d0:
;
;Attempt to switch mode.
;
        mov     IErrorNumber,9
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
        test    [SystemFlags+2],1       ;Dual mode?
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
ifdef _DEBUG
 ifdef LLOUT
        mov     bx,_B000H
        mov     ax,000Dh     ;alloc specific descriptor for low-level video out
        int     31h
        jc      @F
        mov     cx,000Bh
        xor     dx,dx
        mov     ax,7
        int     31h
        or      dx,-1
        xor     cx,cx
        mov     ax,8
        int     31h
@@:
 endif
endif
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
        mov     edi,offset dpmiSelBuffer
        xor     esi,esi
        or      ecx,-1
        xor     al,al
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    MakeDesc
        mov     ax,000ch
        int     31h
        jc      InitError
;
;Get some memory for the INT buffer (functions ff01/ff02).
;
        mov     IErrorNumber,5
        mov     bx,(RawStackTotal/2)/16	;only half the size as in Raw/VCPI
        mov     ax,100h
        int     31h
        jc      InitError
if SMARTRMALLOC
;--- if block is in low memory, skip it and (re)use part of _cwRaw as stack.
        cmp     ax, 0A000h
        jae     @F
        mov     ax,101h
        int     31h
        mov     ax, TSREnd
        add     TSREnd,bx
@@:
endif
        @dprintf DOPT_DOSMEM,<"DOS mem for DPMI rm stack (ff01/02): %X",10>,ax
        mov     ecx,_DATA32
        shl     ecx,4
        mov     es,RealSegment
        mov     es:[ecx][DPMIStackSeg],ax
        mov     es:[ecx][DPMIStackOfs],RawStackTotal/2
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
        or      [SystemFlags],ax        ;at some point. Other flags can be added at will.
        mov     ax,ProtectedFlags       ;bits 0-2 are relevant
        shl     ax,1+1+2
        or      ax,8000h                ;Flags us in protected mode.
        or      [SystemFlags],ax
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
;        shl     ebx,16
;        mov     bx,cx
;        mov     dpmiSelBase,ebx
        mov      w dpmiSelBase+0, cx
        mov      w dpmiSelBase+2, bx
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
;D-bit isn't set - however, the segment has a 1 kB region (MouseEventStack)
;used as stack! Segment size is well below 64 kB though, so not really a problem...
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
        mov     d [OldInt31+0],edx
        mov     w [OldInt31+4],cx
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
;        mov     d[TerminationHandler+0],offset InitError
;        mov     w[TerminationHandler+4],cs
;
;Sort out state save address & size.
;
ifdef SRDPMISTATE
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
endif
;
;Patch exception vectors to put API handlers in place.
;
        mov     grp32Ofs, offset PatchExc
        call    [grp32Proc]

ifndef NOEXPORTS
;
;Setup internaly EXPORT'ed symbols.
;
        push    es
        mov     bx,Group32DS
        mov     es,bx
        assume es:GROUP32
        Sys     GetSelDet32
        mov     edi,offset apiExports  ;size of GROUP32 is < 64k
        mov     cx,es:[di]             ;load number of exports
        add     di,4
        add     DWORD PTR es:[di],edx  ;export module name "CAUSEWAY_KERNAL"
        add     di,4
cw5_exp0:
        mov     esi,es:[di]
        add     DWORD PTR es:[di],edx  ;convert to linear
        mov     ax,es:[si].EXPORTSTRUC.wSeg
        mov     bx,CodeSegment         ;GROUP16 code selector
        cmp     ax,0
        jz      cw5_exp1
        mov     bx,DataSegment         ;GROUP16 data selector
        cmp     ax,1
        jz      cw5_exp1
        mov     bx,Group32CS           ;GROUP32 code selector
        cmp     ax,2
        jz      cw5_exp1
        mov     bx,Group32DS           ;GROUP32 data selector
        cmp     ax,3
        jz      cw5_exp1
        or      bx,-1
cw5_exp1:
        mov     es:[si].EXPORTSTRUC.wSeg,bx
        add     di,4
        loop    cw5_exp0
        pop     es
        assume es:nothing
endif
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
ifndef NOI21RMHOOK
        mov     ax,offset Int21hExecCount
        mov     WORD PTR es:[EPSP_Struc.EPSP_ExecCount+0],ax
        mov     WORD PTR es:[EPSP_Struc.EPSP_ExecCount+2],ds
endif
        mov     es:[EPSP_Struc.EPSP_Resource],edx           ;Clear memory fields.
        mov     es:[EPSP_Struc.EPSP_INTMem],edx
ifdef SRDPMISTATE
;        mov     DWORD PTR es:[EPSP_Struc.EPSP_DPMIMem],edx  ;is a word only!
        mov     es:[EPSP_Struc.EPSP_DPMIMem],dx
endif
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
;Initialise extensions.
;
        or      mcbAllocations,-1       ;Enable MCB code.
        ;
        mov     di,offset ExtensionStart
cw5_e0:
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
        add     di,sizeof EXTENSION
        cmp     di,offset ExtensionEnd
        jb      cw5_e0
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
        push Group32CS
        push lowword offset cwOpen
        retf

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

        mov     ds,cs:DataSegment
        assume ds:GROUP16

        .386
        cmp     Group32DS,0             ;API installed?
        jz      cw6_noAPI
        ;
;
;Remove extension patches.
;
        mov     di,offset ExtensionEnd - sizeof EXTENSION ;list of interupt patches.
cw6_p0:
        cmp     [di].EXTENSION.wFlgs,-1  ;installed?
        jnz     cw6_p2
        push    di
        call    [di].EXTENSION.pExit
        pop     di
cw6_p2:
        sub     di,sizeof EXTENSION
        cmp     di,offset ExtensionStart
        jae     cw6_p0
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
        mov     es,Group32DS
        assume es:GROUP32
        mov     edx,d es:[OldInt31+0]
        mov     cx,w es:[OldInt31+4]
        mov     bl,31h
        mov     ax,205h
        int     31h
        mov     d es:[cwIdentity+0],0
        mov     d es:[cwIdentity+4],0
        assume es:nothing
        ;
cw6_noAPI:
        mov     ax,IErrorNumber
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[IErrorM00n+0],al
        add     dl,'0'
        mov     b[IErrorM00n+1],dl

        cmp     ProtectedType,PT_DPMI
        jz      cw6_DPMI

        mov     ax,KernalZero
        mov     es,ax
        cli                             ;Don't want interrupts interfering.
        push    offset cw6_RealMode     ;returns in real-mode
        jmp     RawVCPIRealMode

;
;Remove DPMI stuff.
;
cw6_DPMI:

if 0
        cmp     d[OldInt21hExec],0
        jz      cw6_d0
        mov     bl,21h
        mov     dx,w[OldInt21hExec+0]
        mov     cx,w[OldInt21hExec+2]
        mov     ax,201h
        int     31h
cw6_d0:
endif
        cmp     IErrorNumber,0
        jz      cw6_NoError
        mov     ax,[InitErrorList]      ;get the "CauseWay error nn : " string
        mov     edi,offset PageInt      ;not used yet in dpmi mode
        push    ds
        pop     es
        mov     RealRegsStruc.Real_DX[di],ax
        mov     RealRegsStruc.Real_AX[di],900h
        mov     RealRegsStruc.Real_DS[di],GROUP16
        xor     cx,cx                   ;No stack parameters.
        mov     bx,21h                  ;no flags.
        mov     ax,300h
        int     31h
        mov     bx,IErrorNumber
        add     bx,bx
        mov     ax,[InitErrorList+bx]
        mov     RealRegsStruc.Real_DX[di],ax
        xor     cx,cx                   ;No stack parameters.
        mov     bx,21h                  ;no flags.
        mov     ax,300h
        int     31h
        jmp     cw6_NoError

        .286
;
;Make sure our data is addressable.
;
cw6_RealMode:
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16
        mov     bx,IErrorNumber
        cmp     bx,0
        jz      cw6_NoError
        mov     dx,[InitErrorList]     ;display "CauseWay error nn : " string
        mov     ah,9
        int     21h
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
;        push    ds
        push    es
;        mov     ax,KernalDS
;        mov     ds,ax
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
;        pop     ds
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
        shl     edx,1
        lea     edx,[edx*4+edx]   ;*10
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
        @dprintf DOPT_DOSMEM,<"CAUSEWAY environment variable found",10>

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
        jz      cw10_1
        ;
        push    offset cw10_3
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
skipkw:
        @dprintf DOPT_DOSMEM,<"unknown CAUSEWAY setting at %ls",10>,si,es
        inc     si
        ret
        ;
cw10_nopass:
        ; shut off passing of real mode interrupts to protected mode
        mov     ax,es:[si+4]
        cmp     ax,"SS"
        jnz     skipkw
        add     si,4+1
        or      NoPassFlag,-1
        ret

cw10_big1:
        ; specify alternate extended memory size computation
        add     si,4
        or      Big1Flag,-1
        ret

cw10_himem:
        ;HIMEM:xxx - Set amount of physical memory to use.
        ;
        mov     ax,es:[si+4]
        cmp     ax,":M"
        jnz     skipkw
        add     si,6
        call    getnum
        cmp     edx,4096*1024
        jnc     @F
        shl     edx,10                  ;turn K into byte's
        shr     edx,12                  ;get number of pages.
        mov     [MaxMemPhys],edx
@@:
        ret
        ;
cw10_extall:
        ;Set flag to use all extended memory.
        ;
        mov     ax,es:[si+4]
        cmp     ax,"LL"
        jnz     skipkw
        add     si,4+2
        or      ExtALLSwitch,-1
        ret
        ;
cw10_novm:
        ;They want to disable VM.
        ;
        add     si,4
        or      NoVMSwitch,-1
        ret
        ;
cw10_maxmem:
        ;MAXMEM:xxx - Set maximum linear address space size.
        ;
        mov     ax,es:[si+4]
        cmp     ax,"ME"
        jnz     skipkw
        cmp     BYTE PTR es:[si+6],":"
        jnz     skipkw
        add     si,7
        call    getnum
        cmp     edx,4096                ;4096MB or more?
        jnc     @F
        shl     edx,20                  ;turn Meg into byte's
        mov     [MaxMemLin],edx
@@:
        ret

cw10_pre:
        ;PRE:xxx - Want to set preallocate amount
        ;
        add     si,4
        call    getnum
        cmp     edx,4096
        jnc     @F
        shl     edx,20                  ;turn Meg into byte's
        mov     d[PreAllocSize],edx
@@:
        ret

cw10_pad1:
        mov     Pad1Flag,1 ; accessible thru FS
        add     si,4
        ret

cw10_noex:
        mov     NoEXECPatchFlag,1 ; accessible thru FS
        add     si,4
        ret

cw10_dpmi:
        ;They want to force DPMI use if possible.
        ;
        @dprintf DOPT_DOSMEM,<"CAUSEWAY=DPMI found",10>
        mov     IProtectedForce,1 ; accessible thru FS
        add     si,4
        ret
        ;
cw10_swap:
        ;They want to specify the swap drive.
        ;
        cmp     BYTE PTR es:[si+4],":"
        jnz     skipkw
        add     si,4+1
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
        ret
        ;
cw10_name:
        ; Specify the swap name.
        ;
        cmp     BYTE PTR es:[si+4],":"
        jnz     skipkw
        add     si,4+1
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
        ret
        ;
cw10_lowmem:
        ;They want to specify conventional memory retention.
        ;
        mov     ax,es:[si+4]
        cmp     ax,"ME"
        jnz     skipkw
        cmp     BYTE PTR es:[si+6],":"
        jnz     skipkw
        add     si,4+2+1
        call    getnum
ifndef NOLOWMEM
        shl     edx,10-4                ;turn K into para's
;--- the amount is no longer ADDED to the default value
;        movzx   ebx,[ConvSaveSize]
;        add     edx,ebx
        cmp     edx,65535
        jc      cw10_lm2
        mov     edx,65535
cw10_lm2:
        mov     [ConvSaveSize],dx       ;set new size.
endif
        ret
        ;
;--- found TEMP
cw10_temp:
        add     si,4
        cmp     BYTE PTR es:[si],"="
        jnz     cw10_skipline
        @dprintf DOPT_DOSMEM,<"TEMP environment variable found, setting VMMDrivPath2",10>
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
;--- found TMP
cw10_tmp:
        add     si,4
        @dprintf DOPT_DOSMEM,<"TMP environment variable found, setting VMMDrivPath3",10>
        mov     di,offset VMMDrivPath3
        jmp cw10_temp3

GetENVStuff     endp

;-------------------------------------------------------------------------------
;--- get name of binary from environment - runs in real-mode

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
retry:
        mov     dx,offset MainExec
        mov     ax,3d40h                ;open, read only, deny none
        int     21h
        jc      cw12_exit
        mov     bx,ax
        mov     dx,offset IExeHdr       ;somewhere to put the info.
;        mov     cx,sizeof MZHdr         ;size of it.
        mov     cx,sizeof NewHeaderStruc;read 40h bytes
        mov     ah,3fh
        int     21h
        jc      cw12_readerr
        cmp     ax,cx                   ;did we read right amount?
        jnz     cw12_readerr
        cmp     IExeHdr.Signature,'ZM'  ;Normal EXE?
        jnz     cw12_checkP3
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
        mov     al,dh                   ;now DX:AX = ([_Length+2])*512
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
        jc      cw12_readerr
        or      ax,ax                   ;end of the file?
        jz      cw12_SetRUN
        cmp     ax,cx                   ;did we read right amount?
        jnz     cw12_readerr
cw12_checkP3:
        cmp     w INewHeader.NewID,'P3'   ;ID ok?
        jnz     cw12_readerr
        mov     si,dx
        .386
        mov     eax,[si].NewHeaderStruc.NewFlags  ;Copy main flags.
        mov     d[SystemFlags],eax
        push    ds
        mov     dx,DPMIGRP
        mov     ds,dx
        assume ds:DPMIGRP
        mov     d[DpmiEmuSystemFlags],eax
        mov     dx,GROUP32
        mov     ds,dx
        assume ds:GROUP32
        mov     d[apiSystemFlags],eax
        .286
        pop     ds
        assume ds:GROUP16
cw12_readerr:                 ;<--- no MZ or P3 header found
        mov     ah,3eh
        int     21h
cw12_exit:
        ret
;
;Nothing on the end of the extender so replace the exec name with first
;command line argument and shuffle everything else down. Allows CauseWay to be used
;to run 16- and 32-bit programs not attached to it from the command line.
;
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
        ;
        ;Skip white space.
        ;
cw12_sr0:
        mov     al,es:[si]
        cmp     al," "
        jnz     cw12_sr1
        inc     si
        loop    cw12_sr0
cw12_sr5:
        pop     es
        ret
cw12_sr1:
        ;
        ;Get program name.
        ;
        mov     di,offset MainExec      ;default to storeing program name.
cw12_nextchar:
        mov     [di],al
        inc     di
        inc     si
        mov     al,es:[si]
        cmp     al," "
        loopnz  cw12_nextchar
        mov     b[di],0
        mov     di,80h+1
        rep     movsb es:[di],es:[si]   ;Copy it down.
        mov     b es:[di],13
        mov     ax,di
        sub     ax,80h+1
        mov     es:[80h],al
        pop     es
        jmp     retry                   ;now retry with the new name in MainExec
GetSystemFlags  endp


;-------------------------------------------------------------------------------
GetProtectedFlags proc near
;
;Find out what protected mode environments are available.
;
        call    ChkDPMI                 ;DPMI host present?
        jc      cw13_0
        or      ProtectedFlags,PF_DPMI
cw13_0:
        call    ChkVCPI                 ;VCPI >= v1.0 present?
        jc      cw13_1
        or      ProtectedFlags,PF_VCPI
cw13_1:
        call    ChkRAW                  ;Running in real mode?
        jc      cw13_2
        or      ProtectedFlags,PF_RAW   ;RAW (=XMS/I15) mode possible
cw13_2:
        ret
GetProtectedFlags endp


;-------------------------------------------------------------------------------
GetProtectedType proc near
        cmp     IProtectedForce,0
        jz      cw14_NoDPMIForce
        test    BYTE PTR ProtectedFlags,1  ; DPMI available?
        jnz     cw14_2
        ;
cw14_NoDPMIForce:
        test    BYTE PTR ProtectedFlags,4  ; XMS/RAW available?
        jz      cw14_1
        mov     ax,PT_RAWXMS               ; Use raw mode.
        ret
cw14_1:
        test    BYTE PTR ProtectedFlags,2  ; VCPI available?
        jz      cw14_2
        mov     ax,PT_VCPI
        ret
cw14_2:
        mov     ax,PT_DPMI
        ret
GetProtectedType endp


;-------------------------------------------------------------------------------
ChkDPMI proc    near
;
;See if DPMI server present.
;
        mov     ax,1687h                ;DPMI instalation check.
        int     2fh
        or      ax,ax                   ;None-zero means its not there.
        jnz     cw15_9
        test    [SystemFlags],1
        jz      cw15_Use32Bit21
        jmp     cw15_Use16Bit21
cw15_Use32Bit21:
        test    bx,1                    ;Must offer 32 bit support.
        jz      cw15_9
cw15_Use16Bit21:
        clc
        ret
        ;
cw15_9:
        stc
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
        cmp     ah,0
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
        .286
        smsw    ax
        and     ax,1                    ; are we in protected (=v86) mode?
        jnz     cw17_9
        clc
        ret
cw17_9:
        stc
        ret
ChkRAW  endp

;-------------------------------------------------------------------------------
;
;Install relevant A20 handler for this machine.
;
InstallA20      proc near
        ;
rv32_IAChkPS2:
        ; Are we on a PS/2?
        ;
        call    IsPS2Machine
        cmp     ax,1
        jne     rv32_IAOnAT
        mov     A20HandlerCall,offset A20_PS2
        jmp     rv32_0
        ;
rv32_IAOnAT:
        ;Assume we're on an AT.
        ;
        mov     A20HandlerCall,offset A20_AT
        ;
rv32_0:
        ret
InstallA20      endp


;-------------------------------------------------------------------------------
IsPS2Machine    proc   near
        mov     ah,0C0h         ; Get System Description Vector
        stc
        int     15h
        jc      rv33_IPMNoPS2           ; Error?  Not a PS/2.
        ;
        ; Do we have a "Micro Channel" computer?
        ;
        mov     al,byte ptr es:[bx+5]   ; Get "Feature Information Byte 1"
        test    al,00000010b            ; Test the "Micro Channel Implemented" bit
        jz      rv33_IPMNoPS2
        ;
rv33_IPMFoundIt:
        xor     ax,ax           ; Disable A20. Fixes PS2 Ctl-Alt-Del bug
        call    A20_PS2
        mov     ax,1
        ret
        ;
rv33_IPMNoPS2:
        xor     ax,ax
        ret
IsPS2Machine    endp


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
cw18_0:
        mov     es:[di+0],cx            ;store low word of limit.
        mov     es:[di+2],si            ;store low word of linear base.
        shr     ecx,16
        shr     esi,16
        or      al,cl
        mov     bx,si
        mov     es:[di+4],bl            ;store mid byte of linear base.
        mov     es:[di+5],ah            ;store pp/dpl/dt/type bits.
        mov     es:[di+6],al            ;store high bits of limit and gran/code size bits.
        mov     es:[di+7],bh            ;store high byte of linear base.
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

_cwDPMIEMU segment para public 'DPMI emulator code' use32

_cwDPMIEMU$1 segment dword public 'DPMI emulator data' use32
_cwDPMIEMU$1 ends

DPMIGRP group _cwDPMIEMU, _cwDPMIEMU$1

        .386p

ifdef _DEBUG
dpsuffix equ <1>
        include dprint32.inc
endif
        include rawvcpi2.inc
        include interrup.inc
        include ldt.inc
        include memory.inc

_cwDPMIEMU ends

_cwDPMIEMU$1 segment
	align 4
cwDPMIEMUEnd    label byte
_cwDPMIEMU$1 ends


_TEXT32 segment para use32 public 'CODE'
_TEXT32 ends
_DATA32 segment para use32 public 'DATA'
_DATA32 ends

_TEXT32 segment
ifdef _DEBUG
dpsuffix equ <2>
        include dprint32.inc
endif
        include exceptn.inc
        include api.inc
        include load3p.inc
        include loadle.inc
        include decode_c.inc
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
