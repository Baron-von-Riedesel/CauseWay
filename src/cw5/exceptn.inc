
TRAPINT00 equ 1		;1=DPMI host requires to trap int 00 instead of exc 00
EXC16INCS16 equ 1	;1=DPMI host requires 16-bit exceptions be handled by 16-bit code (Win9x!)

EXCFRY  struc
_ebp    dd ?
_edi    dd ?
_esi    dd ?
_edx    dd ?
_ecx    dd ?
_ebx    dd ?
_eax    dd ?
_gs     dd ?    ; Exception handler is now in a 32bit code segment
_fs     dd ?
_es     dd ?
_ds     dd ?
EXCFRY  ends

_DATA32 segment

DebugBuffer EXCFRY <>  ; 44 bytes
;--- followed by an IRET32 frame - 20 bytes
DebugEIP        dd ?
DebugCS         dd ?
DebugEFL        dd ?
DebugESP        dd ?
DebugSS         dd ?
;--- followed by TR,CR0,CR2,CR3 - 14 bytes
DebugTR         dw 0
DebugCR0        dd 0
DebugCR2        dd 0
DebugCR3        dd 0
;--- followed by segment addresses - 24 bytes
DebugCSApp      dd ?	;must be in this order
DebugDSApp      dd ?
DebugESApp      dd ?
DebugFSApp      dd ?
DebugGSApp      dd ?
DebugSSApp      dd ?
;--- followed by exc# and code - 6 bytes
DebugExceptionIndex dw 0
DebugExceptionCode  dd 0
INFOLISTSIZE    EQU $ - offset DebugBuffer  ; should be 108 (44+20+14+24+6)
;DebugCR4        dd 0

NUMEXC equ 16

	align 4
OldExceptions   df NUMEXC dup (0)
if TRAPINT00
OldInt00        df 0
endif
if EXC16INCS16
cs16sel         dw 0    ;16-bit CS alias
endif
exc22_Handle    dd ?    ;file handle for DebugDisplay
TotalLinearMem  dd 0,0,0
TotalSelectors  dd 0
InExcep         db 0
DumpAsciiFlag   DB ?
EnableDebugDump DB 1       ; may be set/reset by int 31h, ax=0ff30h

;DbgOut          db 1024 DUP (0); space for debug dump rendering
NewCWErrName    DB 81 DUP (0) ; error file name set with cw function 0xff32
_DATA32 ends

DebugName       db 'cw.err',0
;
ResHeader       db 13,10
        db '   Application resource tracking details   ',13,10
        db '===========================================',13,10
ResHeaderLen    equ     $-ResHeader
;
SelHeader       db 13,10,13,10
        db 'Selectors',13,10
        db '=========',13,10,13,10
        db ' sel   base     limit  type D mem count',13,10
        db '----------------------------------------',13,10
SelHeaderLen    equ $-SelHeader
;
TotalSelsText   db 13,10,"Total selectors: %4",13,10,0
;
MemHeader       db 13,10,13,10
        db 'Linear memory blocks',13,10
        db '====================',13,10,13,10
        db ' handle    base    length ',13,10
        db '--------------------------',13,10
MemHeaderLen    equ $-MemHeader
;
TotalMemText    db 13,10,"Total Linear memory: %8 (%8) in %8 blocks",13,10,0
;
MouseEHeader    db 13,10,13,10
        db "Mouse event target: "
MouseEHeaderLen equ $-MouseEHeader

DosMemHeader    db 13,10,13,10
        db "DOS memory blocks",13,10
        db "=================",13,10,13,10
        db " sel   base    length ",13,10
        db "----------------------",13,10
DosMemHeaderLen equ $-DosMemHeader

CallBackHeader  db 13,10,13,10
        db "Call-Backs",13,10
        db "==========",13,10,13,10
        db "  real        target",13,10
        db "-----------------------",13,10
CallBackHeaderLen equ $-CallBackHeader

LockHeader      db 13,10,13,10
        db 'Linear memory locked',13,10
        db '====================',13,10,13,10
        db '  base    length ',13,10
        db '-----------------',13,10
LockHeaderLen   equ $-LockHeader
;
PIntHeader      db 13,10,13,10
        db 'Protected mode interrupt vectors',13,10
        db '================================',13,10,13,10
        db 'No sel   offset ',13,10
        db '----------------',13,10
PIntHeaderLen   equ $-PIntHeader
;
EIntHeader      db 13,10,13,10
        db 'Protected mode exception vectors',13,10
        db '================================',13,10,13,10
        db 'No sel   offset ',13,10
        db '----------------',13,10
EIntHeaderLen   equ $-EIntHeader
;
RIntHeader      db 13,10,13,10
        db 'Real mode interrupt vectors',13,10
        db '===========================',13,10,13,10
        db 'No seg offset',13,10
        db '-------------',13,10
RIntHeaderLen   equ $-RIntHeader
;
DebugHeader label byte
        db 13,10
        db 'Exception: %2, '
        db 'Error code: %4',13,10,13,10
        db 'EAX=%8 '
        db 'EBX=%8 '
        db 'ECX=%8 '
        db 'EDX=%8 '
        db 'ESI=%8',13,10
        db 'EDI=%8 '
        db 'EBP=%8 '
        db 'ESP=%8 '
        db 'EIP=%8 '
        db 'EFL=%8',13,10,13,10
        db 'CS=%4-%9 '
        db 'DS=%4-%9 '
        db 'ES=%4-%9',13,10
        db 'FS=%4-%9 '
        db 'GS=%4-%9 '
        db 'SS=%4-%9',13,10,13,10
        db 'CR0=%8 '
        db 'CR2=%8 '
        db 'CR3=%8 '
        db 'TR=%4',13,10,13,10
        db 'Info flags=%8',13,10,13,10
        db 0
;
WritingCWERR    db "Writing CW.ERR file....",13,10,13,10,'$'
;

CSEIPDebugText  DB      "CS:EIP > ",0
SSESPDebugText  DB      "SS:ESP > ",0
SSEBPDebugText  DB      "SS:EBP > ",0
UserDebugText   DB      "User   > ",0
LoadAddrDebugText       DB      "Program Linear Load Address: ",0

Copyright       label byte
        db 'CauseWay DOS Extender v'
VersionMajor    db '5.'
VersionMinor    db '00'
        db " MIT License.",13,10,'$'
SizeCopyright equ $ - offset Copyright - 1

SELECTTEXT = 1
        include texts.inc

	align 4

stdregdump label dword
    dd offset DebugExceptionIndex
    dd offset DebugExceptionCode
    dd offset DebugBuffer._eax
    dd offset DebugBuffer._ebx
    dd offset DebugBuffer._ecx
    dd offset DebugBuffer._edx
    dd offset DebugBuffer._esi
    dd offset DebugBuffer._edi
    dd offset DebugBuffer._ebp
    dd offset DebugESP
    dd offset DebugEIP
    dd offset DebugEFL
    dd offset DebugCS,        offset DebugCSApp
    dd offset DebugBuffer._ds,offset DebugDSApp
    dd offset DebugBuffer._es,offset DebugESApp
    dd offset DebugBuffer._fs,offset DebugFSApp
    dd offset DebugBuffer._gs,offset DebugGSApp
    dd offset DebugSS,        offset DebugSSApp
    dd offset DebugCR0
    dd offset DebugCR2
    dd offset DebugCR3
    dd offset DebugTR
    dd offset apiSystemFlags

TotalMemParms label dword
    dd offset TotalLinearMem+0
    dd offset TotalLinearMem+4
    dd offset TotalLinearMem+8
TotalSelsParms label dword
    dd offset TotalSelectors

	align 4

ErrorList       label dword
        dd ErrorM00,ErrorM01,ErrorM02,ErrorM03,ErrorM04,ErrorM05,ErrorM06,ErrorM07
        dd ErrorM08,ErrorM09,ErrorM10,ErrorM11,ErrorM12,ErrorM13,ErrorM14,ErrorM15
        dd ErrorM16

intstuff label dword
        dd offset PIntHeader, PIntHeaderLen, 256, 4
        dd offset EIntHeader, EIntHeaderLen,  32, 4
        dd offset RIntHeader, RIntHeaderLen, 256, 2
;
;-------------1----1----0----0
;-------------5----0----5----0
ExcBitStr dw 01110001111110001b
;
;-------------------------------------------------------------------------

        assume es:GROUP16
        assume ds:GROUP32

ExcepOpen       proc    far
;        mov     ExcepDSeg,es            ;Store GROUP16 data selector.
;        mov     ExcepDDSeg,ds
;        mov     eax,d SystemFlags
;        mov     ExcepSystemFlags,eax
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
        test    byte ptr apiSystemFlags,1
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
        test    BYTE PTR apiSystemFlags,1
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
        test    BYTE PTR apiSystemFlags,1
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
        mov     ebx,[exc22_Handle]
        cmp     ebx,0
        jz      @F
        mov     ah,3eh
        int     21h
@@:
        xor     ebx,ebx
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
;this code is needed only if a host won't recognize a divide error as an exception.
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
        test    BYTE PTR apiSystemFlags,1
        jz      exc3_Use32_0
        add     DebugESP,2+2+2
        movzx   ebp,sp
        movzx   eax,w[ebp+2+2]
        mov     [DebugEFL],eax
        mov     ax,[ebp+2]
        mov     DebugCS,eax
        mov     ax,[ebp+0]
        mov     DebugEIP,eax
        jmp     exc3_Use0_0
        ;
exc3_Use32_0:
        add     DebugESP,4+4+4
        mov     eax,[esp+4+4]
        mov     DebugEFL,eax
        mov     eax,[esp+4]
        mov     DebugCS,eax
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
    thunk_16to32 <,lowword offset DPMIExc00Patch+EXCEPT*4>
EXCEPT = EXCEPT + 1
    endm
thunk_reenter_from_exc thunk_16to32 <,lowword offset exc20_Use0_2>
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
        push    ds      ;pushs to create an EXCFRY frame
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
        mov     ebp, esp
        test    BYTE PTR apiSystemFlags,1
        jz      @F
        movzx   ebp, bp
@@:
        ;
        cmp     [ebp].STKFRAME.wExc,14
        jnz     exc20_Not14Special
        mov     ds,apiDSeg16
        assume ds:GROUP16
        cmp     LinearAddressCheck,0   ; linear address check active?
        jz      exc20_Not14Special
        mov     LinearAddressCheck,0
        pop     ebp
if 1
        lea     esp,[esp+6*4+3*4]
else
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        pop     gs
        pop     fs
        pop     es
endif
        pop     ds
        mov     esi,0            ; reset linear address set by cwAPI_LinearCheck
        lea     esp,[esp+4]      ; skip exception#
        test    BYTE PTR apiSystemFlags,1
        jz      exc20_S14_32
        retw
exc20_S14_32:
        retd
        ;
exc20_Not14Special:
        cld
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;Retrieve register values and get outa here.
        ;
        cmp     InExcep,0
        jz      exc20_ok
        mov     esi,[ebp].STKFRAME.wExc
        add     esi,esi
;--- table OldExceptions[] holds old vectors for exceptions 0-15 only!
        lea     esi,[esi*2+esi+offset OldExceptions] ; esi*6
        lodsd
        mov     edx,eax
        lodsw
;--- store old exception vector in the last two members of STKFRAME (_ds and wExc)
        mov     ds,[ebp].STKFRAME._ds
        mov     [ebp].STKFRAME._ds, edx
        mov     w[ebp].STKFRAME.wExc, ax
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
        retd
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
        rep     movsd [edi],[esi]   ;copy registers off the stack.
        ;
        lodsd   [esi]               ;exception#
        mov     es:DebugExceptionIndex, ax
        test    BYTE PTR es:apiSystemFlags,1
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
        add     esi,4+4+4           ;skip DPMI return address + error code.
        movsd   [edi],[esi]         ;copy EIP,CS,EFL,ESP,SS
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
exc20_Use16Bit678:
        push    es
        pop     ds
        assume ds:GROUP32
        assume es:nothing           ;it's actually still GROUP32
        ;
        ;Process segment registers to produce application relative base
        ;addresses.
        ;
        mov     eax,DebugCS
        mov     edi,offset DebugCSApp
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._ds
        add     edi,4
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._es
        add     edi,4
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._fs
        add     edi,4
        call    DebugSegmentDPMI
        mov     eax,DebugBuffer._gs
        add     edi,4
        call    DebugSegmentDPMI
        mov     eax,DebugSS
        add     edi,4
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

;--- _cwStackEnd-512 is NOT really a safe to use address!
        mov     ss,StackSegment
        mov     esp,offset _cwStackEnd-512
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        jmp     [TerminationHandler]
DPMIExcPatch    endp

        assume ds:nothing
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
        push    fs
        mov     fs,cs:apiDSeg16
        assume fs:GROUP16
        mov     fs,PSPSegment
        assume fs:_cwEnd
        or      ebx,-1
        ;
        mov     edx,eax
        mov     eax,fs
        lsl     eax,eax
        cmp     eax,size EPSP_Struc - 1
        jc      exc21_9
        mov     cx,fs:[EPSP_Struc.EPSP_SegBase]
        cmp     dx,cx                              ;inside application selector space?
        jc      exc21_9
        add     cx,fs:[EPSP_Struc.EPSP_SegSize]
        cmp     dx,cx
        jnc     exc21_9                            ;outside application startup selectors.
        mov     ebx,edx
        mov     ax,0006h
        int     31h
        push    cx
        push    dx
        pop     eax
        sub     eax,fs:[EPSP_Struc.EPSP_MemBase]   ;get offset within application.
        mov     ebx,eax
exc21_9:
        mov     [edi],ebx                          ;store generated value.
        pop     fs
        ret
DebugSegmentDPMI endp

        assume ds:GROUP32
writechar:
        push ecx
        push eax
        mov edx,offset DbgOut
        mov [edx],al
        mov ecx,1
        mov ebx,[exc22_Handle]
        mov ah,40h
        int 21h
        pop edx
        pop ecx
        mov ah,2
wcpatch:
        int 21h
        retn

Bin2HexX:
        push    edi
        mov     edi,offset DbgOut+9
        push    ecx
        mov     edx,eax
bin2hexx_0:
        dec     edi
        mov     al,dl
        shr     edx,4
        and     al,0Fh
        add     al,'0'
        cmp     al,'9'
        jbe     @F
        add     al,7
@@:
        mov     [edi],al
        loop    bin2hexx_0
        pop     ecx
nextc:
        mov     al,[edi]
        call    writechar
        inc     edi
        loop    nextc
        pop     edi
        retn
writexxxx:
        mov  al,'x'
        call writechar
        loop writexxxx
        retn

;--- sort of printf()
;--- ESI->format str
;--- EDI->arguments

myprintf proc
nextchar:
        lodsb
        cmp al,0
        jz done
        cmp al,'%'
        jnz normchar
        lodsb
        sub al,'0'
        movzx ecx,al
        mov edx,[edi]
        add edi,4
        mov ax,[edx]
        cmp cl,2
        jz dispnum
        cmp cl,4
        jz dispnum
        mov eax,[edx]
        btr ecx,0
        jnc dispnum
        cmp eax,-1
        jnz dispnum
        call writexxxx
        jmp nextchar
dispnum:
        call Bin2HexX
        jmp nextchar
normchar:
        call writechar
        jmp nextchar
done:
        ret
myprintf endp
;-------------------------------------------------------------------------
;
;Do a debug dump. Very rough but it works for now....
;called by cwClose.
;checks UserTermFlag and DebugDump variables
;
DebugDisplay    proc    near

        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        @dprintf -1,<"DebugDisplay enter",10>
        cmp     UserTermFlag,0  ; see if user termination routine
        je      dd2             ; no

        mov     esi,OFFSET DebugBuffer
        mov     edi,DWORD PTR UserTermDump+0
        mov     ax,WORD PTR UserTermDump+4
        mov     ecx,eax
        lsl     ecx,ecx
        jnz     @F              ; invalid selector
        mov     es,eax
        mov     ecx,INFOLISTSIZE
        rep     movsb [edi],[esi]
@@:
        cmp     UserTermFlag,1  ; 1=16-bit, 2=32bit termination routine
        mov     UserTermFlag,0  ; reset flag, KEEP CPU FLAG STATUS
        jne     ut32            ; not
        mov     si,sp
        call    [UserTermRoutine16]
        jmp     dd2
ut32:
        mov     esi,esp
        call    [UserTermRoutine]
dd2:
        mov     ds,cs:apiDSeg16         ; restore ds if used
        assume ds:GROUP16
        cmp     DebugDump,0             ;register dump needed?
        jz      exc22_9
        mov     DebugDump,0

        mov     ds,cs:apiDSeg32
        assume ds:GROUP32

        cmp     EnableDebugDump,0       ; see if debug dumping enabled
        je      exc22_9

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
        xor     ecx,ecx
        mov     [exc22_Handle],ecx
        mov     edx,OFFSET NewCWErrName ; new error file name set with (undoc) cw func 0xff32?
;        cmp     ds:[edx],al            ; fixed - AL most likely isn't 00 here!
        cmp     [edx],cl                ; file name set?
        je      oldcwerr                ; no new error file name
        mov     ah,3ch
        int     21h
        jnc     debhand                 ; successfully created new error file
oldcwerr:
        mov     edx,offset DebugName    ; the default "cw.err" file name
        xor     ecx,ecx
        mov     ah,3ch
        int     21h
        jc      exc22_NoFile
debhand:
        mov     [exc22_Handle],eax


exc22_NoFile:
        ;Display debug info.
        ;
        mov     edx,offset Copyright
        mov     ah,9
        int     21h

        mov     edx,offset Copyright
        mov     ecx,SizeCopyright
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;
        ;Now print the results.
        ;
        mov     esi,offset DebugHeader
        mov     edi,offset stdregdump
        call    myprintf

        ;the rest is to be written to cw.err only.
        mov     b wcpatch,0c3h
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
        mov     edx,OFFSET LoadAddrDebugText
        call    DebugTextCopy
        xor     edx,edx
        mov     eax,fs:[EPSP_Struc.EPSP_MemBase]
        mov     cl,8
        call    Bin2Hex
        mov     DWORD PTR [edi],0a0d0a0dh
        add     edi,4
        call    WriteBlock
;
;Do a CS:EIP dump.
;
        mov     ecx,CSEIPDEBUGDUMPCOUNT
        mov     fs,DebugCS
        mov     esi,DebugEIP
        test    BYTE PTR apiSystemFlags,1
        jz      exc22_cseip32
        movzx   esi,si

exc22_cseip32:
        mov     eax,fs
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
        mov     edx,OFFSET CSEIPDebugText
        call    DumpBlock
exc22_cseip3:
;
; Do user-defined dump, if appropriate
;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16       ; allow access to user input variables

        movzx   ecx,DebugUserCount
        test    ecx,ecx         ; no user dump defined
        je      userdone
        mov     ax,DebugUserSel
        verr    ax
        jnz     userdone
        mov     fs,eax
        mov     esi,DebugUserOff
        mov     al,DebugAsciiFlag

        mov     ds,cs:apiDSeg32        ; restore ds for normal variable access
        assume ds:GROUP32
        mov     DumpAsciiFlag,al

        test    BYTE PTR apiSystemFlags,1
        jz      user32
        movzx   esi,si
user32:
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
        mov     edx,OFFSET UserDebugText
        call    DebugTextCopy
        xor     edx,edx
user0:                                  ;<----
        push    ecx
        mov     al,fs:[esi]
        cmp     DumpAsciiFlag,0         ; show as ASCII if so flagged
        je      userbin                 ; binary
        cmp     al,' '                  ; ensure that space or greater
        jae     userascii
        mov     al,'.'                  ; show a period for control characters
userascii:
        mov     [edi],al
        inc     edi
        jmp     usershow
userbin:
        mov     cl,2
        call    Bin2Hex
usershow:
        mov     b[edi]," "
        pop     ecx
        inc     edi
        inc     esi
        inc     edx
        and     dl,0fh
        jnz     user1
        mov     w[edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     [edi+0],eax
        mov     [edi+4],eax
        add     edi,8
        mov     [edi],al
        inc     edi
user1:
        dec     ecx
        jnz     user0                  ; ---->
        mov     DWORD PTR [edi],0a0d0a0dh
        add     edi,4
        call    WriteBlock
userdone:
        mov     ds,cs:apiDSeg32        ; restore ds for normal variable access
        assume ds:GROUP32
ssesp:
;
;Do a SS:ESP dump.
;
        cmp     DebugExceptionIndex,0ch ;stack problem?
        jz      exc22_ssesp3
        mov     ecx,SSESPDEBUGDUMPCOUNT
        mov     fs,DebugSS
        mov     esi,DebugESP
        test    BYTE PTR apiSystemFlags,1
        jz      exc22_ssesp32
        movzx   esi,si
exc22_ssesp32:
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
        mov     edx,OFFSET SSESPDebugText
        call    DumpBlock
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
        test    esi,esi                 ; see if any previous bytes
        je      ebpdump                 ; no, nothing to display
        cmp     esi,ecx                 ; see if stack frame as large as display byte dump
        jae     med2d                   ; yes
        mov     ecx,esi
med2d:
        sub     esi,ecx
        test    BYTE PTR apiSystemFlags,1
        jz      medssebp32
        movzx   esi,si
medssebp32:
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
        mov     edi,offset DbgOut
        mov     eax,20202020h
        mov     [edi+0],eax
        mov     [edi+4],eax
        add     edi,8
        mov     [edi],al
        inc     edi

        push    ecx
        and     ecx,0fh
        je      med3d
        mov     dl,16                   ; high bytes known zero
        sub     dl,cl                   ; save hex bytes left to display on row
        mov     cl,dl

medloop:
        mov     [edi+0],ax              ; 3-byte pads until at proper display position
        mov     [edi+2],al
        add     edi,3
        dec     ecx
        jne     medloop
med3d:
        pop     ecx

medssebp0:                              ;<----
        push    ecx
        mov     al,fs:[esi]
        mov     cl,2
        call    Bin2Hex
        pop     ecx
        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        test    dl,0fh
        jnz     medssebp1

        mov     WORD PTR [edi],0a0dh
        add     edi,2
        cmp     ecx,1
        je      medssebp1
        mov     eax,20202020h
        mov     [edi+0],eax
        mov     [edi+4],eax
        add     edi,8
        mov     [edi],al
        inc     edi
        ;
medssebp1:
        dec     ecx
        jne     medssebp0

;       mov     WORD PTR ds:[edi],0a0dh
;       add     edi,2
        ;
        call    WriteBlock
        ;
ebpdump:
        mov     ecx,SSEBPDEBUGDUMPCOUNT
        mov     fs,DebugSS
        mov     esi,DebugBuffer._ebp
        test    BYTE PTR apiSystemFlags,1
        jz      exc22_ssebp32
        movzx   esi,si
exc22_ssebp32:
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
        mov     edx,OFFSET SSEBPDebugText
        call    DumpBlock
        ;
exc22_ssebp3:
        pop     fs

        ;
        ;Do resource output stuff.
        ;

        push    fs
        push    es
        pushad
        mov     fs,apiDSeg16
        assume fs:GROUP16
        mov     es,fs:RealSegment
        mov     fs,fs:PSPSegment
        assume es:nothing
        assume fs:_cwEnd   ; make assembler assume a 16-bit segment
        cmp     fs:[EPSP_Struc.EPSP_Resource],0
        jz      exc22_rsrc_done

        mov     edx,offset ResHeader
        mov     ecx,ResHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h

        mov     edx,offset SelHeader
        mov     ecx,SelHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h

        mov     esi,fs:[EPSP_Struc.EPSP_Resource]

;--- 1. scan for Res_PSP/Res_SEL

exc22_LookLoop:                         ;<----
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_r0:                               ;<---- next rsrc type
        cmp     BYTE PTR es:[edi],Res_PSP
        jz      @F
        cmp     BYTE PTR es:[edi],Res_SEL
        jnz     exc22_r1
@@:
ifdef SRDPMISTATE
        mov     ax,fs:[EPSP_Struc.EPSP_DPMIMem]
        cmp     WORD PTR es:[esi],ax
        jz      exc22_r1
endif
        ;
exc22_SEL:
        pushad
        mov     edi,offset DbgOut
        mov     eax,es:[esi]
        mov     cl,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ebx,es:[esi]
        Sys     GetSelDet32
        push    ecx
        mov     eax,edx
        mov     cl,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        pop     eax
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
ifdef SRDPMISTATE
        cmp     fs:[EPSP_Struc.EPSP_DPMIMem],0
        jz      exc22_nodpmi
        pushad
        mov     bx,fs:[EPSP_Struc.EPSP_DPMIMem]
        Sys     GetSelDet32
        cmp     es:[esi],edx
        popad
        jz      exc22_s1
exc22_nodpmi:
endif
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
        mov     cl,4
        call    Bin2Hex
        jmp     exc22_r20
        ;
exc22_r2:
        mov     d[edi],'xxxx'
        add     edi,4
        ;
exc22_r20:
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad
        ;
exc22_r1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_r0                 ;---->
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]  ;link to next list.
        or      esi,esi
        jnz     exc22_LookLoop           ;---->
        ;
        mov     esi,offset TotalSelsText
        mov     edi,offset TotalSelsParms
        call    myprintf

        ;
        ;Now do memory blocks.
        ;

        mov     edx,offset MemHeader
        mov     ecx,MemHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;

;--- 2. scan for Res_MEM

        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
exc22_mLookLoop:                        ;<----
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_m0:                               ;<----
        cmp     BYTE PTR es:[edi],Res_MEM
        jnz     exc22_m1

        mov     eax,fs:[EPSP_Struc.EPSP_INTMem]
        cmp     es:[esi],eax
        jz      exc22_m1
ifdef SRDPMISTATE
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
endif
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
        mov     edi,offset DbgOut
        mov     eax,es:[esi+4]
        mov     cl,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi]
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+8]
        add     TotalLinearMem+0,eax
        push    eax
        add     eax,4095
        and     ax,0f000h
        add     TotalLinearMem+4,eax
        pop     eax
        inc     TotalLinearMem+8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad
        ;
exc22_m1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_m0                            ;---->
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_mLookLoop                     ;---->

        ;
        ;Now print MCB controlled blocks.
        ;

        cmp     fs:[EPSP_Struc.EPSP_mcbHead],0
        jz      exc22_nomcbdis
        mov     esi,fs:[EPSP_Struc.EPSP_mcbHead]
exc22_mcb0:                                         ;<----
        mov     edi,esi
        add     esi,MCBCHUNKLEN
exc22_mcb1:                                         ;<----
        cmp     es:[esi].McbStruc.mcbFreeUsed,"J"       ;Free block?
        jz      exc22_mcb2

        mov     eax,fs:[EPSP_Struc.EPSP_INTMem]
        sub     eax,MCBLEN
        cmp     esi,eax
        jz      exc22_mcb2
ifdef SRDPMISTATE
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
endif
        pushad
        mov     edi,offset DbgOut
        mov     eax,esi
        add     eax,MCBLEN
        mov     cl,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,esi
        add     eax,MCBLEN
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        inc     TotalLinearMem+8
        push    eax
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        pop     eax
        add     TotalLinearMem+0,eax
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad

exc22_mcb2:
        cmp     es:[esi].McbStruc.mcbNext,"M"   ;end of the chain?
        jz      exc22_mcb3
        mov     esi,es:[edi].McbChunkStruc.mcbChunkNext
        or      esi,esi
        jnz     exc22_mcb0                      ;---->
        jmp     exc22_nomcbdis
        ;
exc22_mcb3:
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     esi,eax
        jmp     exc22_mcb1                      ;---->
        ;
exc22_nomcbdis:
        ;Display totals.
        ;
        mov     esi,offset TotalMemText
        mov     edi,offset TotalMemParms
        call    myprintf
        
        ;
        ;Now do lock details.
        ;

        mov     edx,offset LockHeader
        mov     ecx,LockHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_lLookLoop:                        ;<----
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_l0:                               ;<----
        cmp     BYTE PTR es:[edi],Res_LOCK
        jnz     exc22_l1
        ;
exc22_LOCK:
        pushad
        mov     edi,offset DbgOut
        mov     eax,es:[esi]
        mov     cl,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+4]
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad
        ;
exc22_l1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_l0                            ;---->
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_lLookLoop                     ;---->

        ;
        ;Now do DOS memory details.
        ;

        mov     edx,offset DosMemHeader
        mov     ecx,DosMemHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_dmLookLoop:                       ;<----
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_dm0:                              ;<----
        cmp     BYTE PTR es:[edi],Res_DOSMEM
        jnz     exc22_dm1
        ;
        pushad
        mov     edi,offset DbgOut
        mov     eax,es:[esi]
        mov     cl,4
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
        mov     cl,8
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+4]
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad
        ;
exc22_dm1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_dm0                     ;---->
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_dmLookLoop              ;---->
        ;
        cmp     fs:[EPSP_Struc.EPSP_INTMem],0
        jz      exc22_rsrc_done

        ;
        ;Now do protected mode int details.
        ;
        mov     edi,offset intstuff
        mov     esi,fs:[EPSP_Struc.EPSP_INTMem]
intloop:        
        mov     edx,[edi+0*4]
        mov     ecx,[edi+1*4]
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;
        mov     ecx,[edi+2*4]
        mov     ebx,0
exc22_pv0:                                ;<----
        push    ebx
        push    ecx
        mov     eax,ebx
        mov     ebx,[edi+3*4]
        cmp     WORD PTR es:[esi+ebx],-1
        jz      exc22_pv1
        ;
        push    edi
        mov     edi,offset DbgOut
        mov     cl,2
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ax,WORD PTR es:[esi+ebx]
        mov     cl,4
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     ecx,ebx
        shl     ecx,1
        mov     eax,DWORD PTR es:[esi]
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        pop     edi
        ;
exc22_pv1:
        pop     ecx
        pop     ebx
        add     esi,[edi+3*4]
        add     esi,2
        inc     ebx
        loop    exc22_pv0
        add     edi,4*4
        cmp     edi,offset intstuff + 4*4*3
        jb      intloop
        ;
        ;Now do call-back details.
        ;
        mov     edx,offset CallBackHeader
        mov     ecx,CallBackHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
exc22_cbLookLoop:                       ;<----
        push    esi
        mov     ebp,ResNum              ;get number of entries.
        mov     edi,esi
        add     esi,size ResHead + ResNum
        add     edi,size ResHead
exc22_cb0:                              ;<----
        cmp     BYTE PTR es:[edi],Res_CALLBACK
        jnz     exc22_cb1
        ;
        pushad
        mov     edi,offset DbgOut
        mov     eax,es:[esi]
        shr     eax,16
        mov     cl,4
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,es:[esi+0]
        call    Bin2Hex
        mov     b[edi],' '
        inc     edi
        mov     eax,es:[esi+8]
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,es:[esi+4]
        mov     cl,8
        call    Bin2Hex
        ;
        mov     w[edi],0a0dh
        add     edi,2
        call    WriteBlock
        popad
        ;
exc22_cb1:
        add     esi,4
        inc     edi
        dec     ebp
        jnz     exc22_cb0                ;---->
        pop     edi
        mov     esi,es:[edi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     exc22_cbLookLoop         ;---->

        ;
        ;Print mouse event target details.
        ;

        mov     eax,MouseETarget+0
        mov     ebx,MouseETarget+4
        mov     ecx,eax
        or      ecx,ebx
        jz      exc22_rsrc_done
        push    eax
        push    ebx
        mov     edx,offset MouseEHeader
        mov     ecx,MouseEHeaderLen
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        pop     eax
        pop     ebx
        mov     edi,offset DbgOut
        mov     cl,4
        call    Bin2Hex
        mov     b[edi],':'
        inc     edi
        mov     eax,ebx
        mov     cl,8
        call    Bin2Hex
        mov     d[edi],0a0d0a0dh
        add     edi,4
        call    WriteBlock

exc22_rsrc_done:

        popad
        pop     es
        pop     fs
        ;
        mov     ds,apiDSeg16
        assume ds:GROUP16
exc22_9:
        mov     ax,ErrorNumber          ;Get the error number.
        cmp     ax,0
        jz      cw2_NoError

        mov     ds,cs:apiDSeg32
        assume ds:GROUP32

        movzx   ebx,ax
        xor     dx,dx
        mov     cx,10
        div     cx
        add     al,'0'
        mov     b[ErrorM00n+0],al
        add     dl,'0'
        mov     b[ErrorM00n+1],dl

        cmp     EnableDebugDump,0       ; if debug dump turned off, no screen i/o
        je      cw2_NoError

        mov     edx,[ErrorList+0]
        mov     ah,9
        int     21h
;
;Get a pointer to the appropriate error message and print it.
;
        mov     edx,[ErrorList+ebx*4]
        mov     ah,9
        int     21h
;
;Now exit with the error number as the DOS "errorlevel".
;
cw2_NoError:
        @dprintf -1,<"DebugDisplay exit",10>
        pop     ds
        assume ds:nothing
        ret
        ;
DebugDisplay    endp

;--- in: EDX=text to copy

DumpBlock proc near        
        call    DebugTextCopy
        xor     edx,edx
byteloop:              ;<----
        push    ecx
        mov     cl,2
        mov     al,fs:[esi]
        call    Bin2Hex
        pop     ecx
        mov     b[edi]," "
        inc     edi
        inc     esi
        inc     edx
        and     dl,0fh
        jnz     @F
        mov     WORD PTR [edi],0a0dh
        add     edi,2

        mov     eax,20202020h
        mov     [edi+0],eax
        mov     [edi+4],eax
        add     edi,8
        mov     [edi],al
        inc     edi
        ;
@@:
        loop    byteloop
        mov     DWORD PTR [edi],0a0d0a0dh
        add     edi,4
;       mov     b[edi],"$"
        ;
WriteBlock::
        mov     ecx,edi
        mov     edx,offset DbgOut
        sub     ecx,edx
        mov     ebx,[exc22_Handle]
        mov     ah,40h
        int     21h
        ret
DumpBlock endp

;-------------------------------------------------------------------------
;
;Convert number into ASCII Hex version.
;
;On Entry:-
;
;EAX    - Number to convert.
;CL     - Digits to do.
;DS:EDI - Buffer to put string in.
;
Bin2Hex proc    near
        push    eax
        push    ecx
        push    edx
        movzx   ecx,cl
        add     edi,ecx
        push    edi
        mov     edx,eax
exc23_0:
        dec     edi
        mov     al,dl
        shr     edx,4
        and     al,0Fh
        add     al,'0'
        cmp     al,'9'
        jbe     @F
        add     al,7
@@:
        mov     [edi],al
        loop    exc23_0
        pop     edi
        pop     edx
        pop     ecx
        pop     eax
        ret
Bin2Hex endp

;-------------------------------------------------------------------------
; copy debug text into output buffer
; upon entry ds:edi -> out buffer, ds:edx -> in buffer
;
DebugTextCopy   PROC    NEAR

        mov     edi,offset DbgOut
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

