
TRAPINT00 equ 1		;1=DPMI host requires to trap int 00 instead of exc 00
EXC16INCS16 equ 1	;1=DPMI host requires 16-bit exceptions be handled by 16-bit code (Win9x!)

;-------------------------------------------------------------------------
ExcepSystemFlags dd ?
;

EXCFRY  struc
_ebp    dd ?
_edi    dd ?
_esi    dd ?
_edx    dd ?
_ecx    dd ?
_ebx    dd ?
_eax    dd ?
_gs     dd ?    ; segment _Excep is now 32bit!
_fs     dd ?
_es     dd ?
_ds     dd ?
EXCFRY  ends

DebugBuffer EXCFRY <>
;--- followed by an IRET32 frame
DebugEIP        dd ?
DebugCS         dw ?,?
DebugEFL        dd ?
DebugESP        dd ?
DebugSS         dw ?,?

DebugTR dw ?
DebugCR0        dd ?
;DebugCR1        dd ?
DebugCR2        dd ?
DebugCR3        dd ?
;
;--- must be in this order
DebugCSApp      dd ?
DebugDSApp      dd ?
DebugESApp      dd ?
DebugFSApp      dd ?
DebugGSApp      dd ?
DebugSSApp      dd ?
;
DebugExceptionCode dd 0
DebugExceptionIndex dw 0

NUMEXC equ 16

_DATA32 segment

	align 4
OldExceptions df NUMEXC dup (0)
if TRAPINT00
OldInt00      df 0
endif
if EXC16INCS16
cs16sel         dw 0    ;16-bit CS alias
endif

;
DebugName       db 'cw.err',0
;
ResHeader       db 13,10
        db '   Application resource tracking details   ',13,10
        db '===========================================',13,10,'$'
ResHeaderLen    equ     ($-1)-ResHeader
;
SelHeader       db 13,10,13,10
        db 'Selectors',13,10
        db '=========',13,10,13,10
        db ' sel   base     limit  type D mem count',13,10
        db '----------------------------------------',13,10,'$'
SelHeaderLen    equ ($-1)-SelHeader
;
TotalSelsText   db 13,10,"Total selectors: "
TotalSelsNum    db "0000",13,10
TotalSelsTLen   equ     $-TotalSelsText
;
MemHeader       db 13,10,13,10
        db 'Linear memory blocks',13,10
        db '====================',13,10,13,10
        db ' handle    base    length ',13,10
        db '--------------------------',13,10,'$'
MemHeaderLen    equ ($-1)-MemHeader
;
TotalMemText    db 13,10,"Total Linear memory: "
TotalMemNum1    db "00000000 ("
TotalMemNum2    db "00000000) in "
TotalMemNum3    db "00000000 blocks",13,10
TotalMemTLen    equ     $-TotalMemText
;
MouseEHeader    db 13,10,13,10
        db "Mouse event target: ","$"
MouseEHeaderLen equ ($-1)-MouseEHeader
DosMemHeader    db 13,10,13,10
        db "DOS memory blocks",13,10
        db "=================",13,10,13,10
        db " sel   base    length ",13,10
        db "----------------------",13,10,"$"
DosMemHeaderLen equ ($-1)-DosMemHeader
CallBackHeader  db 13,10,13,10
        db "Call-Backs",13,10
        db "==========",13,10,13,10
        db "  real        target",13,10
        db "-----------------------",13,10,"$"
CallBackHeaderLen equ ($-1)-CallBackHeader
LockHeader      db 13,10,13,10
        db 'Linear memory locked',13,10
        db '====================',13,10,13,10
        db '  base    length ',13,10
        db '-----------------',13,10,'$'
LockHeaderLen   equ ($-1)-LockHeader
;
PIntHeader      db 13,10,13,10
        db 'Protected mode interrupt vectors',13,10
        db '================================',13,10,13,10
        db 'No sel   offset ',13,10
        db '----------------',13,10,'$'
PIntHeaderLen   equ ($-1)-PIntHeader
;
EIntHeader      db 13,10,13,10
        db 'Protected mode exception vectors',13,10
        db '================================',13,10,13,10
        db 'No sel   offset ',13,10
        db '----------------',13,10,'$'
EIntHeaderLen   equ ($-1)-EIntHeader
;
RIntHeader      db 13,10,13,10
        db 'Real mode interrupt vectors',13,10
        db '===========================',13,10,13,10
        db 'No seg offset',13,10
        db '-------------',13,10,'$'
RIntHeaderLen   equ ($-1)-RIntHeader
;
TotalLinearMem  dd 0,0,0
TotalSelectors  dd 0
;
DebugHeader     db 13,10,'Exception: '
DebugINum       db '00, Error code: '
DebugENum       db '0000',13,10,13,10
        db 'EAX='
DebugEAXt       db '00000000 '
        db 'EBX='
DebugEBXt       db '00000000 '
        db 'ECX='
DebugECXt       db '00000000 '
        db 'EDX='
DebugEDXt       db '00000000 '
        db 'ESI='
DebugESIt       db '00000000 '
        db 13,10
        db 'EDI='
DebugEDIt       db '00000000 '
        db 'EBP='
DebugEBPt       db '00000000 '
        db 'ESP='
DebugESPt       db '00000000 '
        db 'EIP='
DebugEIPt       db '00000000 '
        db 'EFL='
DebugEFLt       db '00000000 '
        db 13,10,13,10
        db 'CS='
DebugCSt        db '0000-'
DebugCSAppt     db '00000000 '
        db 'DS='
DebugDSt        db '0000-'
DebugDSAppt     db '00000000 '
        db 'ES='
DebugESt        db '0000-'
DebugESAppt     db '00000000 '
        db 13,10
        db 'FS='
DebugFSt        db '0000-'
DebugFSAppt     db '00000000 '
        db 'GS='
DebugGSt        db '0000-'
DebugGSAppt     db '00000000 '
        db 'SS='
DebugSSt        db '0000-'
DebugSSAppt     db '00000000 '
        db 13,10,13,10
        db 'CR0='
DebugCR0t       db '00000000 '
        db 'CR2='
DebugCR2t       db '00000000 '
        db 'CR3='
DebugCR3t       db '00000000 '
        db 'TR='
DebugTRt        db '0000'
        db 13,10,13,10
        db 'Info flags='
DebugSysFlags   db '00000000 '
        db 13,10,13,10,'$'
        db      768 DUP (0)
DebugHeaderEnd  label byte
;
HexTable        db '0123456789ABCDEF'
;
InExcep db 0
InExcepJMP      df 0
;
WritingCWERR    db "Writing CW.ERR file....",13,10,13,10,36
;

CSEIPDebugText  DB      "CS:EIP > ",0
SSESPDebugText  DB      "SS:ESP > ",0
SSEBPDebugText  DB      "SS:EBP > ",0
UserDebugText   DB      "User   > ",0
LoadAddrDebugText       DB      "Program Linear Load Address: ",0
DumpAsciiFlag   DB      ?

;-------------1----1----0----0
;-------------5----0----5----0
ExcBitStr dw 01110001111110001b

_DATA32 ends


;-------------------------------------------------------------------------

        assume es:GROUP16
        assume ds:GROUP32

ExcepOpen       proc    far
;        mov     ExcepDSeg,es            ;Store GROUP16 data selector.
;        mov     ExcepDDSeg,ds
        mov     eax,SystemFlags
        mov     ExcepSystemFlags,eax
        ;
;--- actually, installs handler for Exc 00, 04-09 and 0D-0F only.
;--- also, handler for Int 00.

        mov     ecx,DPMIExcPatchSize
        mov     esi,offset DPMIExc00Patch
if EXC16INCS16
        push    ebp
        mov     ebp, cs
        cmp     ProtectedType,PT_DPMI
        jnz     @F
        test    byte ptr ExcepSystemFlags,1
        jz      @F
        call    dpmi16bitspec
@@:
endif
        xor     ebx,ebx
        mov     edi,offset OldExceptions
exc1_2:
        bt      ExcBitStr, bx
        jnc     skipexc
        push    ecx
        call    exc1_1
        pop     ecx
skipexc:
        add     esi,ecx
        add     edi,6
        inc     ebx
        cmp     ebx,NUMEXC
        jb      exc1_2
if EXC16INCS16
        pop     ebp
endif
if TRAPINT00
        ;
        mov     bl,00
        Sys     GetVect
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc1_i00Use32
        movzx   edx, dx
exc1_i00Use32:
        mov     d[OldInt00+0],edx
        mov     w[OldInt00+4],cx
        mov     edx,offset Int00Handler
        mov     ecx,cs
        Sys     SetVect
endif
        ;
        clc
        ret
        ;
exc1_1:
        mov     ax,202h
        int     31h
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc1_Use32
        movzx   edx,dx
exc1_Use32:
        mov     d[edi+0],edx
        mov     w[edi+4],cx
        mov     edx,esi
if EXC16INCS16
        mov     ecx,ebp
else
        mov     ecx,cs
endif
        mov     ax,203h
        int     31h
        retn

if EXC16INCS16
;--- this is for 16-bit apps running as DPMI clients only.
;--- the Win9x (also probably Win3x) host needs a 16-bit CS for
;--- exception handlers.

dpmi16bitspec:
        mov     cx,1
        mov     ax,0
        int     31h
        mov     ebp, eax
        mov     edi, offset dpmiSelBuffer
        mov     ebx, cs
        mov     ax,000bh
        int     31h
        mov     ebx,ebp
        mov     cs16sel,bp
        xor     byte ptr es:[edi+6],40h
        mov     ax,000ch
        int     31h
        mov     ecx,5
        mov     esi,offset DPMIExc00Patch16
        xor     ebx,ebx
        push    esi
nextvec:
        mov     w[esi+3],cs
        add     esi,ecx
        inc     ebx
        cmp     ebx,NUMEXC
        jnz     nextvec
        mov     thunk_reenter_from_exc.wCS,cs
        pop     esi
        retn
endif

ExcepOpen       endp

        assume es:nothing

;-------------------------------------------------------------------------
ExcepClose      proc    far
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        xor     bx,bx
        mov     edi,offset OldExceptions
exc2_2:
        mov     edx,d[edi+0]
        mov     cx,w[edi+4]
        jcxz    @F
        mov     ax,203h
        int     31h
@@:
        add     edi,6
        inc     bx
        cmp     bx,NUMEXC
        jb      exc2_2
        ;
if TRAPINT00
        cmp     w[OldInt00+4],0
        jz      exc2_i2
        mov     edx,d[OldInt00+0]
        mov     cx,w[OldInt00+4]
        mov     bl,00
        Sys     SetVect
        ;
exc2_i2:
endif
        pop     ds
        clc
        ret
        ;

ExcepClose      endp

if TRAPINT00
;-------------------------------------------------------------------------
;
;Handle a divide by zero.
;this code is needed only if a host won't recognize an divide error as an exception.
;
Int00Handler    proc    near
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     DebugBuffer._eax,eax
        mov     DebugBuffer._ebx,ebx
        mov     DebugBuffer._ecx,ecx
        mov     DebugBuffer._edx,edx
        mov     DebugBuffer._esi,esi
        mov     DebugBuffer._edi,edi
        mov     DebugBuffer._ebp,ebp
        pop     eax
        mov     DebugESP,esp
        mov     DebugBuffer._ds,eax
        mov     DebugBuffer._es,es
        mov     DebugBuffer._fs,fs
        mov     DebugBuffer._gs,gs
        ;
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc3_Use32_0
        add     DebugESP,2+2+2
        movzx   ebp,sp
        mov     ax,[ebp+2+2]
        mov     w[DebugEFL],ax
        mov     ax,[ebp+2]
        mov     DebugCS,ax
        movzx   eax,WORD PTR [ebp]
        mov     DebugEIP,eax
        jmp     exc3_Use0_0
        ;
exc3_Use32_0:
        add     DebugESP,4+4+4
        mov     eax,[esp+4+4]
        mov     DebugEFL,eax
        mov     eax,[esp+4]
        mov     DebugCS,ax
        mov     eax,[esp+0]
        mov     DebugEIP,eax
        ;
exc3_Use0_0:
        mov     DebugExceptionIndex,0
        mov     DebugExceptionCode,0
        ;
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     DebugDump,1
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        sti
        jmp     [TerminationHandler]
Int00Handler    endp
endif

;-------------------------------------------------------------------------

if EXC16INCS16
;--- thunks to switch CS from 16 to 32 bits

thunk_16to32 struc
		db 0EAh	;jmp SSSS:OOOO in 16-bit
wIP		dw ?	;offset
wCS		dw ?	;selector
thunk_16to32 ends

EXCEPT = 0
DPMIExc00Patch16:
    rept 16
    thunk_16to32 <,offset DPMIExc00Patch+EXCEPT*4>
EXCEPT = EXCEPT + 1
    endm
thunk_reenter_from_exc thunk_16to32 <,offset offset exc20_Use0_2>
endif

EXCEPT = 0
DPMIExc00Patch:
	rept 16
	push EXCEPT
    jmp short DPMIExcPatch
EXCEPT = EXCEPT + 1
    endm
DPMIExcPatchSize equ ($ - offset DPMIExc00Patch) / 16

;-------------------------------------------------------------------------
;--- stack: DPMI exception frame 16/32 bit

DPMIEXC16 struc
		dw ?,?	;return to DPMI host
_Err	dw ?
_IP		dw ?
_CS		dw ?
_Flags	dw ?
_SP		dw ?
_SS		dw ?
DPMIEXC16 ends

DPMIEXC32 struc
		dd ?,?	;return to DPMI host
_Err	dd ?
_IP		dd ?
_CS		dd ?
_Flags	dd ?
_SP		dd ?
_SS		dd ?
DPMIEXC32 ends

STKFRAME struc
		EXCFRY <>
wExc	dd ?      ;_excep is now 32bit
STKFRAME ends

DPMIExcPatch    proc    far
        push    ds
        push    es
        push    fs
        push    gs
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     ebp, esp
        test    BYTE PTR cs:ExcepSystemFlags,1
        jz      @F
        movzx   ebp, bp
@@:
        ;
        cmp     [ebp].STKFRAME.wExc,14
        jnz     exc20_Not14Special
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        cmp     LinearAddressCheck,0
        pop     ds
        assume ds:GROUP32
        jz      exc20_Not14Special
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     LinearAddressCheck,0
        test    BYTE PTR cs:ExcepSystemFlags,1
        pop     ds
        assume ds:GROUP32
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        pop     gs
        pop     fs
        pop     es
        pop     ds
        mov     esi,0
        lea     esp,[esp+4]      ; skip exception#
        jz      exc20_S14_32
        retw
exc20_S14_32:
        retd
        ;
exc20_Not14Special:
        ;Retrieve register values and get outa here.
        ;
        cmp     InExcep,0
        jz      exc20_ok
        mov     esi,[ebp].STKFRAME.wExc
        add     esi,esi        ; esi = esi*6
        mov     eax,esi
        add     esi,esi
        add     esi,eax
        add     esi,offset OldExceptions
        mov     eax,[esi+0]
        mov     d[InExcepJMP+0],eax
        mov     ax,[esi+4]
        mov     w[InExcepJMP+4],ax
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        pop     gs
        pop     fs
        pop     es
        pop     ds
        lea     esp,[esp+4]         ;skip exception#
        jmp     FWORD PTR cs:[InExcepJMP]
        assume ds:GROUP32
        ;
exc20_ok:
        or      InExcep,-1
        push    ds
        pop     es
        assume es:GROUP32
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        mov     esi,ebp
        mov     edi,offset DebugBuffer
        mov     ecx,sizeof DebugBuffer/4
        cld
        rep     movsd [edi],[esi]   ;copy registers off the stack.
        ;
        lodsd   [esi]               ;exception#
        mov     es:DebugExceptionIndex, ax
        test    BYTE PTR es:ExcepSystemFlags,1
        jz      exc20_Use32Bit17
        movzx   eax,w[ebp+sizeof STKFRAME+(2+2)]
        mov     es:DebugExceptionCode,eax
        add     esi,2+2+2           ;skip DPMI return address + error code
        xor     eax, eax
        lodsw   [esi]               ;copy IP,CS,FL,SP,SS
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        jmp     exc20_Use16Bit678
        ;
exc20_Use32Bit17:
        mov     eax,[ebp+sizeof STKFRAME+(4+4)]
        mov     es:DebugExceptionCode,eax
        add     esi,4+4+4            ;skip DPMI return address + error code.
        movsd   [edi],[esi]         ;copy EIP,CS,EFL,ESP,SS
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
exc20_Use16Bit678:
        push    es
        pop     ds
        assume es:nothing
        assume ds:GROUP32
        ;
        mov     DebugTR,0
        mov     DebugCR0,0
        mov     DebugCR2,0
        mov     DebugCR3,0
        ;
        ;Process segment registers to produce application relative base
        ;addresses.
        ;
        mov     ax,DebugCS
        mov     edi,offset DebugCSApp
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._ds
        mov     edi,offset DebugDSApp
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._es
        mov     edi,offset DebugESApp
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._fs
        mov     edi,offset DebugFSApp
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._gs
        mov     edi,offset DebugGSApp
        call    DebugSegmentDPMI
        mov     ax,DebugSS
        mov     edi,offset DebugSSApp
        call    DebugSegmentDPMI
        ;
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        pop     gs
        pop     fs
        pop     es
        pop     ds
        lea     esp,[esp+4]    ; skip exception#
        ;
        push    eax
        push    ebp
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     DebugDump,1
        mov     ErrorNumber,9
        ;
        mov     ebp,esp         ;make stack addressable.
        ;
        ;Now modify original CS:EIP,SS:ESP values and return control
        ;to this code via interupt structure to restore stacks.
        ;
        test    BYTE PTR SystemFlags,1
        jz      exc20_Use32_2
if EXC16INCS16
        mov     ax, cs:cs16sel
        and     ax, ax
        jz      @F
        mov     [bp+3*4].DPMIEXC16._IP,lowword offset thunk_reenter_from_exc
        mov     [bp+3*4].DPMIEXC16._CS,ax
        jmp     exc20_Use16_1
@@:
endif
        mov     [bp+3*4].DPMIEXC16._IP,lowword offset exc20_Use0_2
        mov     [bp+3*4].DPMIEXC16._CS,cs
exc20_Use16_1:
        pop     ds
        pop     ebp
        pop     eax
        retw
        ;
exc20_Use32_2:
        mov     [ebp+3*4].DPMIEXC32._IP,offset exc20_Use0_2
        mov     [ebp+3*4].DPMIEXC32._CS,cs
        pop     ds
        pop     ebp
        pop     eax
        retd
        ;
exc20_Use0_2::
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ss,StackSegment
        mov     esp,offset _cwStackEnd-256
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        jmp     [TerminationHandler]
DPMIExcPatch    endp

;-------------------------------------------------------------------------
;
;Convert segment value into real & application relative.
;
;On Entry:-
;
;AX     - Selector.
;DS:EDI - Buffer address.
;
DebugSegmentDPMI proc near
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        push    fs
        mov     fs,PSPSegment
        mov     ebx,-1
        ;
        push    eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,size EPSP_Struc
        pop     eax
        jc      exc21_9
        ;
        mov     edx,eax
        cmp     ax,fs:[EPSP_Struc.EPSP_SegBase]    ;inside application selector space?
        jc      exc21_9
        mov     cx,fs:[EPSP_Struc.EPSP_SegSize]    ;8 bytes per selector.
        add     cx,fs:[EPSP_Struc.EPSP_SegBase]    ;add in base selector.
        cmp     ax,cx
        jnc     exc21_9                                     ;outside application startup selectors.
        mov     ebx,edx
        push    cx
        push    dx
        mov     ax,0006h
        int     31h
        mov     ax,cx
        shl     eax,16
        mov     ax,dx
        pop     dx
        pop     cx
        sub     eax,fs:[EPSP_Struc.EPSP_MemBase]   ;get offset within application.
        mov     ebx,eax
exc21_9:
        pop     fs
        pop     ds
        mov     [edi],ebx                                   ;store generated value.
        ret
DebugSegmentDPMI endp

        assume ds:GROUP32

stdregdump label dword
    dd offset DebugCR0,         offset DebugCR0t
    dd offset DebugCR2,         offset DebugCR2t
    dd offset DebugCR3,         offset DebugCR3t
    dd offset DebugBuffer._eax, offset DebugEAXt
    dd offset DebugBuffer._ebx, offset DebugEBXt
    dd offset DebugBuffer._ecx, offset DebugECXt
    dd offset DebugBuffer._edx, offset DebugEDXt
    dd offset DebugBuffer._esi, offset DebugESIt
    dd offset DebugBuffer._edi, offset DebugEDIt
    dd offset DebugBuffer._ebp, offset DebugEBPt
    dd offset DebugESP,         offset DebugESPt
    dd offset DebugEIP,         offset DebugEIPt
    dd offset DebugEFL,         offset DebugEFLt
segregdump label dword
    dd offset DebugCS,          offset DebugCSt, offset DebugCSAppt
    dd offset DebugBuffer._ds,  offset DebugDSt, offset DebugDSAppt
    dd offset DebugBuffer._es,  offset DebugESt, offset DebugESAppt
    dd offset DebugBuffer._fs,  offset DebugFSt, offset DebugFSAppt
    dd offset DebugBuffer._gs,  offset DebugGSt, offset DebugGSAppt
    dd offset DebugSS,          offset DebugSSt, offset DebugSSAppt
;-------------------------------------------------------------------------
;
;Do a debug dump. Very rough but it works for now....
;called by cwClose as FAR32
;checks UserTermFlag and DebugDump variables
;
DebugDisplay    proc    far

        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        cmp     UserTermFlag,0  ; see if user termination routine
        je      dd2             ; no

INFOLISTSIZE    EQU     104

        xor     ecx,ecx
        mov     esi,OFFSET DebugBuffer
        mov     edi,DWORD PTR UserTermDump
        cmp     UserTermFlag,1  ; see if 16-bit termination routine
        mov     UserTermFlag,0  ; reset flag, KEEP CPU FLAG STATUS
        jne     ut32            ; not
        mov     ax,WORD PTR UserTermDump+2
        mov     ecx,eax
        lsl     ecx,ecx
        jnz     utsi            ; invalid selector
        mov     es,eax
        mov     ecx,INFOLISTSIZE
        rep     movsb   es:[edi], cs:[esi]
utsi:
        mov     si,sp
;--- since _Excep is now 32-bit, "DWORD PTR" isn't the correct attribute to call a FAR16 proc
;        call    DWORD PTR [UserTermRoutine]
        call    [UserTermRoutine16]
        jmp     dd2
ut32:
        mov     ax,WORD PTR UserTermDump+4
        mov     ecx,eax
        lsl     ecx,ecx
        jnz     utesi           ; invalid selector
        mov     es,eax
        mov     ecx,INFOLISTSIZE
        rep     movsb   es:[edi], cs:[esi]
utesi:
        mov     esi,esp
        call    [UserTermRoutine]
dd2:
        mov     ds,cs:apiDSeg16         ; restore ds if used
        cmp     DebugDump,0             ;register dump needed?
        jz      exc22_9
        mov     DebugDump,0

        cmp     EnableDebugDump,0       ; see if debug dumping enabled
        je      exc22_9

        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        mov     ah,0fh
        int     10h
        cmp     al,3
        jz      exc22_ModeOk
        cmp     al,2
        jz      exc22_ModeOk
        cmp     al,7
        jz      exc22_ModeOk
        mov     ax,3
        int     10h
exc22_ModeOk:
        mov     [exc22_Handle],0
        xor     cx,cx
        mov     ah,3ch

; MED 06/18/96
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     edx,OFFSET NewCWErrName ; new error file name set with (undoc) cw func 0xff32?
;        cmp     ds:[edx],al            ; fixed - AL most likely isn't 00 here!
        cmp     ds:[edx],cl             ; file name set?
        je      oldcwerr                ; no new error file name
        int     21h
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        jnc     debhand                 ; successfully created new error file
oldcwerr:
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     edx,offset DebugName    ; the default "cw.err" file name
        xor     cx,cx
        mov     ah,3ch
        int     21h
        jc      exc22_NoFile
debhand:
        mov     [exc22_Handle],eax

exc22_NoFile:
        ;Display debug info.
        ;
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     edx,offset Copyright
        mov     ecx,SizeCopyright
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        pop     ds
        assume ds:GROUP32
        ;
        mov     ax,DebugExceptionIndex
        mov     cx,2
        mov     edi,offset DebugINum
        call    Bin2Hex
        mov     ax,w[DebugExceptionCode]        ;high word undefined upto 486.
        mov     cx,4
        mov     edi,offset DebugENum
        call    Bin2Hex
        ;
        mov     ax,DebugTR
        mov     cx,4
        mov     edi,offset DebugTRt
        call    Bin2Hex

        mov     esi, offset stdregdump
nextreg:
        mov     eax, [esi+0]
        mov     eax, [eax]
        mov     edi, [esi+4]
        mov     cx, 8
        call    Bin2Hex
        add     esi, 8
        cmp     esi, offset segregdump
        jnz     nextreg
        mov     esi, offset segregdump
        mov     ebx, offset DebugCSApp
nextseg:
        mov     eax, [esi+0]
        mov     ax, [eax]
        mov     cx, 4
        mov     edi, [esi+4]
        call    Bin2Hex
        mov     eax, [ebx]
        mov     cx, 8
        mov     edi, [esi+8]
        cmp     eax, -1
        jnz     exc22_0
        mov     d[edi+0],'xxxx'
        mov     d[edi+4],'xxxx'
        add     edi,8
        jmp     exc22_1
exc22_0:
        call    Bin2Hex
exc22_1:
        add     esi,3*4
        add     ebx,4
        cmp     ebx,offset DebugCSApp+6*4
        jnz     nextseg

        mov     edi,offset DebugSysFlags
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        movzx   eax,w[SystemFlags]
        assume ds:GROUP32
        pop     ds
        mov     cx,8
        call    Bin2Hex


        ;
        ;Now print the results.
        ;

        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     esi,offset Copyright
exc22_cp0:
        cmp     b[esi],0
        jz      exc22_cp1
        mov     dl,[esi]
        mov     ah,2
        int     21h
        inc     esi
        jmp     exc22_cp0
exc22_cp1:
        assume ds:GROUP32
        pop     ds


        mov     edx,offset DebugHeader
        mov     ah,9
        int     21h
        mov     edx,offset DebugHeader
        mov     ecx,offset DebugHeaderEnd-(DebugHeader+1+768)
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h

        ;
        ;Print message about writing cw.err
        ;
        mov     edx,offset WritingCWERR
        mov     ah,9
        int     21h

CSEIPDEBUGDUMPCOUNT     EQU     256     ; should be multiple of 16
SSESPDEBUGDUMPCOUNT     EQU     256     ; should be multiple of 16
SSEBPDEBUGDUMPCOUNT     EQU     256     ; should be multiple of 16

        push    fs

        mov     fs,ds:apiDSeg16
        assume fs:GROUP16
        mov     fs,fs:PSPSegment
        assume fs:nothing
        mov     edi,offset DebugHeader
        mov     edx,OFFSET LoadAddrDebugText
        call    DebugTextCopy
        xor     edx,edx
        mov     eax,fs:[EPSP_Struc.EPSP_MemBase]
        mov     cx,8
        call    Bin2Hex
        mov     DWORD PTR ds:[edi],0a0d0a0dh
        add     edi,4

        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h

        ;
        ;Do a CS:EIP dump.
        ;
        mov     ecx,CSEIPDEBUGDUMPCOUNT
        mov     fs,DebugCS
        mov     esi,DebugEIP
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc22_cseip32
        movzx   esi,si

exc22_cseip32:
        xor     eax,eax
        mov     ax,fs
        lsl     eax,eax
        cmp     eax,-1
        jz      exc22_flat0
        inc     eax

exc22_flat0:
        mov     ebx,esi
        add     ebx,ecx
        cmp     ebx,eax
        jc      exc22_cseip2
        sub     ebx,eax
        sub     ecx,ebx
        jz      exc22_cseip3
        js      exc22_cseip3

exc22_cseip2:
        ;
        mov     edi,offset DebugHeader
        mov     edx,OFFSET CSEIPDebugText
        call    DebugTextCopy
        xor     edx,edx
exc22_cseip0:
        push    ecx
        mov     ecx,2
        mov     al,fs:[esi]
        call    Bin2Hex
        pop     ecx
        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        and     dl,0fh
        jnz     exc22_cseip1
        mov     WORD PTR ds:[edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi
        ;
exc22_cseip1:
        dec     ecx
        jnz     exc22_cseip0
        mov     DWORD PTR ds:[edi],0a0d0a0dh
        add     edi,4
;       mov     b[edi],"$"
        ;
        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h

exc22_cseip3:

; Do user-defined dump, if appropriate
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16       ; allow access to user input variables

        movzx   ecx,WORD PTR DebugUserCount
        test    ecx,ecx         ; no user dump defined
        je      userdone
        mov     ax,DebugUserSel
        verr    ax
        jnz     userdone
        mov     fs,ax
        mov     esi,DebugUserOff
        mov     al,DebugAsciiFlag

        mov     ds,cs:apiDSeg32        ; restore ds for normal variable access
        assume ds:GROUP32
        mov     DumpAsciiFlag,al

        test    BYTE PTR ExcepSystemFlags,1
        jz      user32
        movzx   esi,si

user32:
        xor     eax,eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,-1
        jz      userflat0
        inc     eax

userflat0:
        mov     ebx,esi
        add     ebx,ecx
        cmp     ebx,eax
        jc      user2
        sub     ebx,eax
        sub     ecx,ebx
        jz      userdone
        js      userdone

user2:
        mov     edi,offset DebugHeader
        mov     edx,OFFSET UserDebugText
        call    DebugTextCopy
        xor     edx,edx
user0:
        push    ecx
        mov     ecx,2
        mov     al,fs:[esi]
        cmp     DumpAsciiFlag,0 ; show as ASCII if so flagged
        je      userbin                 ; binary
        cmp     al,' '                  ; ensure that space or greater
        jae     userascii
        mov     al,'.'                  ; show a period for control characters

userascii:
        mov     BYTE PTR ds:[edi],al
        inc     edi
        jmp     usershow

userbin:
        call    Bin2Hex

usershow:
        mov     b[edi]," "
        pop     ecx
        inc     edi
        inc     esi
        inc     edx
        and     dl,0fh
        jnz     user1
        mov     WORD PTR ds:[edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi

user1:
        dec     ecx
        jnz     user0
        mov     DWORD PTR ds:[edi],0a0d0a0dh
        add     edi,4

        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h

userdone:
        mov     ds,cs:apiDSeg32        ; restore ds for normal variable access
        assume ds:GROUP32

ssesp:
        ;Do a SS:ESP dump.
        ;
        cmp     DebugExceptionIndex,0ch ;stack problem?
        jz      exc22_ssesp3
        mov     ecx,SSESPDEBUGDUMPCOUNT
        mov     fs,DebugSS
        mov     esi,DebugESP
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc22_ssesp32
        movzx   esi,si

exc22_ssesp32:
        xor     eax,eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,-1
        jz      exc22_flat1
        inc     eax

exc22_flat1:
        mov     ebx,esi
        add     ebx,ecx
        cmp     ebx,eax
        jc      exc22_ssesp2
        sub     ebx,eax
        sub     ecx,ebx
        jz      exc22_ssesp3
        js      exc22_ssesp3

exc22_ssesp2:
        ;
        mov     edi,offset DebugHeader
        mov     edx,OFFSET SSESPDebugText
        call    DebugTextCopy
        xor     edx,edx

exc22_ssesp0:
        push    ecx
        mov     ecx,2
        mov     al,fs:[esi]
        call    Bin2Hex
        pop     ecx
        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        test    dl,0fh
        jnz     exc22_ssesp1
        mov     WORD PTR ds:[edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi
        ;
exc22_ssesp1:
        dec     ecx
        jnz     exc22_ssesp0

        mov     DWORD PTR ds:[edi],0a0d0a0dh
        add     edi,4
;       mov     b[edi],"$"
        ;
        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h

exc22_ssesp3:
        ;
        ;Do a SS:EBP dump.
        ;
        cmp     DebugExceptionIndex,0ch ;stack problem?
        jz      exc22_ssebp3

; do a pre-EBP dump for stack frames
        mov     ecx,SSEBPDEBUGDUMPCOUNT
        mov     fs,DebugSS
        mov     esi,DebugBuffer._ebp
        test    esi,esi         ; see if any previous bytes
        je      ebpdump                 ; no, nothing to display
        cmp     esi,ecx                 ; see if stack frame as large as display byte dump
        jae     med2d                   ; yes
        mov     ecx,esi

med2d:
        sub     esi,ecx

        test    BYTE PTR ExcepSystemFlags,1
        jz      medssebp32
        movzx   esi,si

medssebp32:
        xor     eax,eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,-1
        jz      medflat2
        inc     eax

medflat2:
        mov     ebx,esi
        add     ebx,ecx
        cmp     ebx,eax
        jc      medssebp2
        sub     ebx,eax
        sub     ecx,ebx
        jz      ebpdump
        js      ebpdump

medssebp2:
        ;
        xor     edx,edx

        mov     edi,offset DebugHeader
        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi

        push    ecx
        and     ecx,0fh
        je      med3d
        mov     dl,16                   ; high bytes known zero
        sub     dl,cl                   ; save hex bytes left to display on row
        mov     cl,dl

medloop:
        mov     ds:[edi],ax             ; 3-byte pads until at proper display position
        mov     ds:[edi+2],al
        add     edi,3
        dec     ecx
        jne     medloop

med3d:
        pop     ecx

medssebp0:
        push    ecx
        mov     al,fs:[esi]
        mov     ecx,2
        call    Bin2Hex
        pop     ecx

        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        test    dl,0fh
        jnz     medssebp1

        mov     WORD PTR ds:[edi],0a0dh
        add     edi,2
        cmp     ecx,1
        je      medssebp1
        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi

        ;
medssebp1:
        dec     ecx
        jne     medssebp0

;       mov     WORD PTR ds:[edi],0a0dh
;       add     edi,2
;       mov     b[edi],"$"
        ;
        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;

ebpdump:
        mov     ecx,SSEBPDEBUGDUMPCOUNT
        mov     fs,DebugSS
        mov     esi,DebugBuffer._ebp
        test    BYTE PTR ExcepSystemFlags,1
        jz      exc22_ssebp32
        movzx   esi,si

exc22_ssebp32:
        xor     eax,eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,-1
        jz      exc22_flat2
        inc     eax

exc22_flat2:
        mov     ebx,esi
        add     ebx,ecx
        cmp     ebx,eax
        jc      exc22_ssebp2
        sub     ebx,eax
        sub     ecx,ebx
        jz      exc22_ssebp3
        js      exc22_ssebp3

exc22_ssebp2:
        ;
        mov     edi,offset DebugHeader
        mov     edx,OFFSET SSEBPDebugText
        call    DebugTextCopy
        xor     edx,edx

exc22_ssebp0:
        push    ecx
        mov     ecx,2
        mov     al,fs:[esi]
        call    Bin2Hex
        pop     ecx
        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        test    dl,0fh
        jnz     exc22_ssebp1
        mov     WORD PTR ds:[edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     DWORD PTR ds:[edi],eax
        mov     DWORD PTR ds:[edi+4],eax
        add     edi,8
        mov     BYTE PTR ds:[edi],al
        inc     edi
        ;
exc22_ssebp1:
        dec     ecx
        jnz     exc22_ssebp0
        mov     DWORD PTR ds:[edi],0a0d0a0dh
        add     edi,4
;       mov     b[edi],"$"
        ;
        mov     edx,offset DebugHeader
        sub     edi,edx
        mov     ecx,edi
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;

exc22_ssebp3:

        pop     fs

        ;
        ;Do resource output stuff.
        ;
        push    fs
        mov     fs,apiDSeg16
        assume fs:GROUP16
        mov     fs,fs:PSPSegment
        assume fs:_cwEnd   ; make assembler assume a 16-bit segment
        push    es
        pushad
        cmp     fs:[EPSP_Struc.EPSP_Resource],0
        jz      exc22_r8

        mov     edx,offset ResHeader
        mov     ah,40h
        mov     ecx,ResHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h

        mov     edx,offset SelHeader
        mov     ah,40h
        mov     ecx,SelHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h

        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_LookLoop:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_r0:
        cmp     BYTE PTR es:[edi],Res_PSP
        jz      exc22_r1_0
        cmp     BYTE PTR es:[edi],Res_SEL
        jnz     exc22_r1
exc22_r1_0:
        mov     ax,fs:[EPSP_Struc.EPSP_DPMIMem]
        cmp     WORD PTR es:[esi],ax
        jz      exc22_r1

        ;
exc22_SEL:
        pushad
        mov     edi,offset DebugHeader
        mov     eax,es:[esi]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ebx,es:[esi]
        Sys     GetSelDet32
        push    ecx
        mov     eax,edx
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        pop     eax
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi]
        lar     eax,eax
        test    eax,0000100000000000b
        mov     ebx,'EDOC'
        jnz     exc22_dc0
        mov     ebx,'ATAD'
exc22_dc0:
        mov     [edi],ebx
        add     edi,4
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi]
        lar     eax,eax
        mov     bx,"61"
        test    eax,00400000h
        jz      exc22_Use32It
        mov     bx,"23"
exc22_Use32It:
        mov     [edi],bx
        add     edi,2
        mov     b[edi],' '
        inc     edi
        inc     TotalSelectors


        ;
        ;See if there is a memory block that matches this selector.
        ;
        mov     ebx,es:[esi]
        Sys     GetSelDet32
        mov     ebx,edx
        pushad

        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_s2:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_s0:
        cmp     BYTE PTR es:[edi],Res_MEM       ;Anything here?
        jnz     exc22_s1

        mov     eax,fs:[EPSP_Struc.EPSP_INTMem]
        cmp     es:[esi],eax
        jz      exc22_s1
        cmp     fs:[EPSP_Struc.EPSP_DPMIMem],0
        jz      exc22_nodpmi
        pushad
        mov     bx,fs:[EPSP_Struc.EPSP_DPMIMem]
        Sys     GetSelDet32
        cmp     es:[esi],edx
        popad
        jz      exc22_s1

exc22_nodpmi:
        cmp     ebx,es:[esi]            ;Fits this block?
        jnz     exc22_s1
        pop     esi
        clc
        jmp     exc22_s3
        ;
exc22_s1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_s0
        pop     esi
        mov     esi,es:[esi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_s2
        ;
        ;Have a look in the MCB list.
        ;
        cmp     fs:[EPSP_Struc.EPSP_mcbHead],0
        jz      exc22_nomcbsel
        mov     esi,fs:[EPSP_Struc.EPSP_mcbHead]
exc22_mcbs0:
        mov     edi,esi
        add     esi,MCBCHUNKLEN
exc22_mcbs1:
        mov     eax,esi
        add     eax,MCBLEN
        cmp     ebx,eax
        jnz     exc22_mcbs2
        clc
        jmp     exc22_s3
exc22_mcbs2:
        cmp     es:[esi].McbStruc.mcbNext,"M"
        jz      exc22_mcbs3
        mov     esi,es:[edi].McbChunkStruc.mcbChunkNext
        or      esi,esi
        jz      exc22_nomcbsel
        jmp     exc22_mcbs0
exc22_mcbs3:
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     esi,eax
        jmp     exc22_mcbs1

exc22_nomcbsel:
        stc

exc22_s3:
        popad

        jc      exc22_r4
        ;
        mov     b[edi],'Y'
        jmp     exc22_r5
        ;
exc22_r4:
        mov     d[edi],'N'
        ;
exc22_r5:
        inc     edi
        mov     b[edi],' '
        inc     edi
        ;
        mov     eax,es:[esi]
        mov     bx,fs:[EPSP_Struc.EPSP_SegBase]
        cmp     ax,bx
        jc      exc22_r2
        mov     bx,fs:[EPSP_Struc.EPSP_SegSize]    ;8 bytes per selector.
        add     bx,fs:[EPSP_Struc.EPSP_SegBase]
        cmp     ax,bx
        jnc     exc22_r2
        mov     bx,fs:[EPSP_Struc.EPSP_SegBase]
        shr     bx,3
        shr     ax,3
        sub     ax,bx
        inc     ax
        mov     ecx,4
        call    Bin2Hex
        jmp     exc22_r20
        ;
exc22_r2:
        mov     d[edi],'xxxx'
        add     edi,4
        ;
exc22_r20:
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        popad
        ;
exc22_r1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_r0
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]  ;link to next list.
        or      esi,esi
        jnz     exc22_LookLoop
        ;
        mov     edi,offset TotalSelsNum
        mov     eax,TotalSelectors
        mov     ecx,4
        call    Bin2Hex
        mov     ecx,TotalSelsTLen
        mov     edx,offset TotalSelsText
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        ;Now do memory blocks.
        ;
        mov     edx,offset MemHeader
        mov     ah,40h
        mov     ecx,MemHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
exc22_mLookLoop:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_m0:
        cmp     BYTE PTR es:[edi],Res_MEM
        jnz     exc22_m1

        mov     eax,fs:[EPSP_Struc.EPSP_INTMem]
        cmp     es:[esi],eax
        jz      exc22_m1
        cmp     fs:[EPSP_Struc.EPSP_DPMIMem],0
        jz      exc22_nodpmimem
        pushad
        mov     bx,fs:[EPSP_Struc.EPSP_DPMIMem]
        Sys     GetSelDet32
        cmp     es:[esi],edx
        popad
        jz      exc22_m1

        ;
exc22_nodpmimem:
        ;Check if this is an MCB block.
        ;
        push    edi
        cmp     fs:[EPSP_Struc.EPSP_mcbHead],0
        jz      exc22_MEM
        mov     ebx,es:[esi]
        mov     edi,fs:[EPSP_Struc.EPSP_mcbHead]
exc22_mcbmc0:
        cmp     ebx,edi
        jz      exc22_mcbmc1
        mov     edi,es:[edi].McbChunkStruc.mcbChunkNext
        or      edi,edi
        jz      exc22_MEM
        jmp     exc22_mcbmc0

        ;
exc22_mcbmc1:
        ;Update Total memory value and skip this block.
        ;
        mov     eax,es:[esi+8]
        add     eax,4095
        and     eax,0fffff000h
        add     TotalLinearMem+4,eax
        pop     edi
        jmp     exc22_m1

exc22_MEM:
        pop     edi
        pushad
        mov     edi,offset DebugHeader
        mov     eax,es:[esi+4]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+8]
        add     TotalLinearMem,eax
        push    eax
        add     eax,4095
        and     eax,0fffff000h
        add     TotalLinearMem+4,eax
        pop     eax
        inc     TotalLinearMem+8
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        push    es
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        pop     es
        popad
        ;
exc22_m1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_m0
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_mLookLoop
        ;
        ;Now print MCB controlled blocks.
        ;
        cmp     fs:[EPSP_Struc.EPSP_mcbHead],0
        jz      exc22_nomcbdis
        mov     esi,fs:[EPSP_Struc.EPSP_mcbHead]
exc22_mcb0:
        mov     edi,esi
        add     esi,MCBCHUNKLEN
exc22_mcb1:
        cmp     es:[esi].McbStruc.mcbFreeUsed,"J"       ;Free block?
        jz      exc22_mcb2

        mov     eax,fs:[EPSP_Struc.EPSP_INTMem]
        sub     eax,MCBLEN
        cmp     esi,eax
        jz      exc22_mcb2
        cmp     fs:[EPSP_Struc.EPSP_DPMIMem],0
        jz      exc22_nodpmimemhere
        pushad
        mov     bx,fs:[EPSP_Struc.EPSP_DPMIMem]
        Sys     GetSelDet32
        sub     edx,MCBLEN
        cmp     esi,edx
        popad
        jz      exc22_mcb2

exc22_nodpmimemhere:
        pushad
        mov     edi,offset DebugHeader
        mov     eax,esi
        add     eax,MCBLEN
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,esi
        add     eax,MCBLEN
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        inc     TotalLinearMem+8
        mov     cx,8
        push    eax
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        pop     eax
        add     TotalLinearMem,eax
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        push    es
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        pop     es

        popad

exc22_mcb2:
        cmp     es:[esi].McbStruc.mcbNext,"M"   ;end of the chain?
        jz      exc22_mcb3
        mov     esi,es:[edi].McbChunkStruc.mcbChunkNext
        or      esi,esi
        jnz     exc22_mcb0
        jmp     exc22_nomcbdis
        ;
exc22_mcb3:
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     esi,eax
        jmp     exc22_mcb1



        ;
exc22_nomcbdis:
        ;Display totals.
        ;
        mov     edi,offset TotalMemNum1
        mov     eax,TotalLinearMem
        mov     ecx,8
        call    Bin2Hex
        mov     edi,offset TotalMemNum2
        mov     eax,TotalLinearMem+4
        mov     ecx,8
        call    Bin2Hex
        mov     edi,offset TotalMemNum3
        mov     eax,TotalLinearMem+8
        mov     ecx,8
        call    Bin2Hex
        mov     ecx,TotalMemTLen
        mov     edx,offset TotalMemText
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        ;Now do lock details.
        ;
        mov     edx,offset LockHeader
        mov     ah,40h
        mov     ecx,LockHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_lLookLoop:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_l0:
        cmp     BYTE PTR es:[edi],Res_LOCK
        jnz     exc22_l1
        ;
exc22_LOCK:
        pushad
        mov     edi,offset DebugHeader
        mov     eax,es:[esi]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+4]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        popad
        ;
exc22_l1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_l0
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_lLookLoop
        ;
        ;Now do DOS memory details.
        ;
        mov     edx,offset DosMemHeader
        mov     ah,40h
        mov     ecx,DosMemHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_dmLookLoop:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_dm0:
        cmp     BYTE PTR es:[edi],Res_DOSMEM
        jnz     exc22_dm1
        ;
        pushad
        mov     edi,offset DebugHeader
        mov     eax,es:[esi]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        push    ebx
        push    ecx
        push    edx
        mov     bx,es:[esi]
        Sys     GetSelDet32
        mov     eax,edx
        pop     edx
        pop     ecx
        pop     ebx
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+4]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        popad
        ;
exc22_dm1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_dm0
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_dmLookLoop
        ;
        cmp     fs:[EPSP_Struc.EPSP_INTMem],0
        jz      exc22_r8
        ;
        ;Now do protected mode int details.
        ;
        mov     edx,offset PIntHeader
        mov     ah,40h
        mov     ecx,PIntHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     ecx,256
        mov     esi,fs:[EPSP_Struc.EPSP_INTMem]
        mov     ebx,0
exc22_pv0:
        push    ebx
        push    ecx
        push    esi
        cmp     WORD PTR es:[esi+4],-1
        jz      exc22_pv1
        ;
        mov     edi,offset DebugHeader
        mov     eax,ebx
        mov     cx,2
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ax,WORD PTR es:[esi+4]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,DWORD PTR es:[esi]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;
exc22_pv1:
        pop     esi
        pop     ecx
        pop     ebx
        add     esi,6
        inc     ebx
        loop    exc22_pv0



        ;
        ;Now do protected mode exception details.
        ;
        mov     edx,offset EIntHeader
        mov     ah,40h
        mov     ecx,EIntHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     ecx,32
        mov     esi,fs:[EPSP_Struc.EPSP_INTMem]
        add     esi,256*6
        mov     ebx,0
exc22_pe0:
        push    ebx
        push    ecx
        push    esi
        cmp     WORD PTR es:[esi+4],-1
        jz      exc22_pe1
        ;
        mov     edi,offset DebugHeader
        mov     eax,ebx
        mov     cx,2
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ax,WORD PTR es:[esi+4]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,DWORD PTR es:[esi]
        mov     cx,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;
exc22_pe1:
        pop     esi
        pop     ecx
        pop     ebx
        add     esi,6
        inc     ebx
        loop    exc22_pe0



        ;
        ;Now do real mode int details.
        ;
        mov     edx,offset RIntHeader
        mov     ah,40h
        mov     ecx,RIntHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     ecx,256
        mov     esi,fs:[EPSP_Struc.EPSP_INTMem]
        add     esi,(256*6)+(32*6)
        mov     ebx,0
exc22_ri0:
        push    ebx
        push    ecx
        push    esi
        cmp     WORD PTR es:[esi+2],-1
        jz      exc22_ri1
        ;
        mov     edi,offset DebugHeader
        mov     eax,ebx
        mov     cx,2
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ax,WORD PTR es:[esi+2]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ax,WORD PTR es:[esi]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        ;
exc22_ri1:
        pop     esi
        pop     ecx
        pop     ebx
        add     esi,4
        inc     ebx
        loop    exc22_ri0
        ;
        ;Now do call-back details.
        ;
        mov     edx,offset CallBackHeader
        mov     ah,40h
        mov     ecx,CallBackHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_cbLookLoop:
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_cb0:
        cmp     BYTE PTR es:[edi],Res_CALLBACK
        jnz     exc22_cb1
        ;
        pushad
        mov     edi,offset DebugHeader
        mov     eax,es:[esi]
        shr     eax,16
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,es:[esi]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+8]
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,es:[esi+4]
        mov     cx,8
        call    Bin2Hex
        ;
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h
        popad
        ;
exc22_cb1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_cb0
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_cbLookLoop
        ;
        ;Print mouse event target details.
        ;
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     eax,MouseETarget+0
        mov     ebx,MouseETarget+4
        assume ds:GROUP32
        pop     ds
        mov     ecx,eax
        or      ecx,ebx
        jz      exc22_r8
        push    eax
        push    ebx
        mov     edx,offset MouseEHeader
        mov     ah,40h
        mov     ecx,MouseEHeaderLen
        mov     ebx,[exc22_Handle]
        int     21h
        pop     eax
        pop     ebx
        mov     edi,offset DebugHeader
        mov     cx,4
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,ebx
        mov     ecx,8
        call    Bin2Hex
        mov     b[edi],13
        inc     edi
        mov     b[edi],10
        inc     edi
        mov     b[edi],13
        mov     b[edi+1],10
        mov     b[edi+2],'$'
        add     edi,2
        push    edi
        pop     ecx
        sub     ecx,offset DebugHeader
        mov     edx,offset DebugHeader
        mov     ah,40h
        mov     ebx,[exc22_Handle]
        int     21h

exc22_r8:
        popad
        pop     es
        pop     fs

        ;
        mov     ebx,[exc22_Handle]
        mov     ah,3eh
        int     21h
        ;

exc22_9:
        pop     ds
        retf
        ;
align 4
exc22_Handle  dd ?

DebugDisplay    endp


;-------------------------------------------------------------------------
;
;Convert number into ASCII Hex version.
;
;On Entry:-
;
;EAX    - Number to convert.
;CX     - Digits to do.
;DS:EDI - Buffer to put string in.
;
Bin2Hex proc    near
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    edi
        movzx   ecx,cx
        mov     ebx,offset HexTable
        add     edi,ecx
        dec     edi
        mov     edx,eax
exc23_0:
        mov     al,dl
        shr     edx,4
        and     al,15
        xlat    cs:[ebx]
        mov     [edi],al
        dec     edi
        dec     ecx
        jnz     exc23_0
        pop     edi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        push    ecx
        movzx   ecx,cx
        add     edi,ecx
        pop     ecx
        ret
Bin2Hex endp

;-------------------------------------------------------------------------
; copy debug text into output buffer
; upon entry ds:edi -> out buffer, ds:edx -> in buffer
;
DebugTextCopy   PROC    NEAR

dbdloop:
        mov     al,[edx]
        test    al,al
        je      dbdret          ; null terminator flags end of text, don't transfer
        mov     [edi],al
        inc     edx
        inc     edi
        jmp     dbdloop
dbdret:
        ret
DebugTextCopy   ENDP

