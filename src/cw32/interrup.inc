;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Protected mode interrupt/exception handlers.
;for RAW/VCPI
;
; 1. all 256 IDT vectors are setup to call proc IntHandler
; 2. IntHandler jumps to either IntStack or KernalStack(Exc,Irq,int 1ch/23h/24h)
; 3a. IntStack jumps to InterruptTable[int#]
; 3b. KernalStack jumps to IntDispatch, which then jumps to
;     either InterruptTable[int#] or ExceptionTable[int#]

;-------------------------------------------------------------------------------

ISEXCEPTION	equ 10000h	;cw uses RF in eflags to internally indicate exception

_cwDPMIEMU$1 segment
	align 4
;
ExcFields label byte
DpmiEmuSystemFlags dw 0,0
;
; do NOT change order of these variables (you can add at the end), MED 01/08/96
ExceptionCode   dd 0
ExceptionFlags  dd 0    ;bit 16==1->exception
ExceptionIndex  dd 0
;
EXCFRX  struc
_edi    dd ?
_esi    dd ?
_ebp    dd ?
        dd ?
_ebx    dd ?
_edx    dd ?
_ecx    dd ?
_eax    dd ?
_gs     dd ?
_fs     dd ?
_es     dd ?
_ds     dd ?
EXCFRX  ends

ExceptionRegs EXCFRX <>
ExceptionEIP    dd ?
ExceptionCS     dd ?
ExceptionEFL    dd ?
ExceptionESP    dd ?
ExceptionSS     dd ?

ExceptionTR     dw ?
ExceptionCR0    dd ?
ExceptionCR2    dd ?
ExceptionCR3    dd ?
;
	align 4

ExceptionIntBuffer RealRegsStruc <>  ;used to display unhandled exception info
;
	align 4
;-------------------------------------------------------------------------------

InterruptTable  label fword
;
;Interrupt handler entry points (Int nn and IntR).
;
IntNum  = 0
      rept 256
        dd offset IntNN386Catch+IntNum*INTNN386SIZE
        dw DpmiEmuCS
IntNum  = IntNum+1
      endm

        org InterruptTable + 2fh * 6
        dd offset Raw2FPatch
        dw DpmiEmuCS

        org InterruptTable + 31h * 6
        dd offset RawDPMIPatch
        dw DpmiEmuCS

        org InterruptTable + 41h * 6
        dd offset Raw41Patch
        dw DpmiEmuCS

        org InterruptTable + 256 * 6

;-------------------------------------------------------------------------------
ExceptionTable  label fword
;
;Exception handler entry points (Processor exceptions).
;
IntNum  = 0
        rept 32
        dd offset ExcepNN386Catch+IntNum*EXCNN386SIZE
        dw DpmiEmuCS
IntNum  = IntNum+1
        endm

inter17_Call2   df 0

_cwDPMIEMU$1 ends

ExceptionHeader label byte
    db 13,10
    db "CauseWay Error 09 : Unrecoverable internal exception, program terminated.",13,10
    db 13,10
    db 'Exception: %2, Error code: %4',13,10
    db 13,10
    db 'EAX=%8 EBX=%8 ECX=%8 EDX=%8 ESI=%8',13,10
    db 'EDI=%8 EBP=%8 ESP=%8 EIP=%8 EFL=%8',13,10
    db 13,10
    db 'CS=%4 DS=%4 ES=%4 FS=%4 GS=%4 SS=%4',13,10
    db 13,10
    db 'CR0=%8 CR2=%8 CR3=%8 TR=%4',13,10
    db 13,10
    db 'SystemFlags=%8',13,10
    db 13,10,'$',0
;
bofs macro ofs
	exitm <offset ofs - offset ExcFields>
endm

stdregdump2 label byte
    db bofs(ExceptionIndex)
    db bofs(ExceptionCode)
    db bofs(ExceptionRegs._eax)
    db bofs(ExceptionRegs._ebx)
    db bofs(ExceptionRegs._ecx)
    db bofs(ExceptionRegs._edx)
    db bofs(ExceptionRegs._esi)
    db bofs(ExceptionRegs._edi)
    db bofs(ExceptionRegs._ebp)
    db bofs(ExceptionESP)
    db bofs(ExceptionEIP)
    db bofs(ExceptionEFL)
    db bofs(ExceptionCS)
    db bofs(ExceptionRegs._ds)
    db bofs(ExceptionRegs._es)
    db bofs(ExceptionRegs._fs)
    db bofs(ExceptionRegs._gs)
    db bofs(ExceptionSS)
    db bofs(ExceptionCR0)
    db bofs(ExceptionCR2)
    db bofs(ExceptionCR3)
    db bofs(ExceptionTR)
    db bofs(DpmiEmuSystemFlags)

	align 4

;-------------------------------------------------------------------------------
;
;initial entry points for the interrupt handlers.
;
INTCALLSIZE  equ 4    ; must be 4 or 8
INTNN386SIZE equ 4    ; must be 4 or 8
EXCNN386SIZE equ 4    ; must be 4 or 8

InterruptHandler label near
        rept 256
if INTCALLSIZE eq 4
        call near16 ptr IntHandler
else
        call IntHandler
        db 3 dup (-1)
endif
        endm

;-------------------------------------------------------------------------------
;--- default int handlers

IntNN386Catch   label near
        rept 256
if INTNN386SIZE eq 4
        call near16 ptr IntNN386
else
;        db 0e8h
;        dd offset IntNN386-($+4)
        call IntNN386
        db 3 dup (-1)
endif
        endm

;-------------------------------------------------------------------------------
;--- default exception handlers

ExcepNN386Catch label near
        rept 32
if EXCNN386SIZE eq 4
        call near16 ptr ExcepNN386
else
        call ExcepNN386
        db 3 dup (-1)
endif
        endm

;-------------------------------------------------------------------------------
;
;--- this is the generic handler for all IDT vectors;
;--- the vectors are interrupt vectors, selector is DpmiEmuCS0, so code runs in PL0.
;--- 1. retrieve the interrupt number and store it in ExceptionIndex.
;--- 2. check if it's an exception with error code
;--- 3. if it's with error code, and it's 0Dh, check if some instr. is to emulate;
;---    if so, emulate it and exit quickly.
;--- 4. decide whether to proceed with IntStack or KernalStack (exc, IRQ, int 1Ch/23h/24h)
;
        assume ds:nothing

IntHandler      proc    near
if INTCALLSIZE eq 4
        sub     esp, 2
endif
        push    ds
        push    eax
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        movzx   esp,sp          ;our stack never >64k.
if INTCALLSIZE eq 4
        movzx   eax,w[esp+(4+4)+2] ;get return address.
        sub     eax,offset InterruptHandler + 4
        shr     eax,2           ;convert it to an interrupt number.
else
        mov     eax,[esp+(4+4)] ;get return address.
        sub     eax,offset InterruptHandler + 5
        shr     eax,3           ;convert it to an interrupt number.
endif
        mov     ExceptionIndex,eax      ;/
;
;Check if this is an exception or interrupt (any error code)
;

IRET32PL struc
union
 struct
_eip	dd ?
_cs		dd ?
 ends
_cseip  df ?
ends
_efl	dd ?
_esp	dd ?
_ss		dd ?
IRET32PL ends
INTHFRM struc
_eax	dd ?	;eax
_ds		dd ?	;ds
		dd ?	;ret addr
_errC	dd ?	;error code
		IRET32PL <>
INTHFRM ends

;--- exception error code on stack?
        cmp     esp,tPL0StackSize - sizeof INTHFRM
        jnz     inter14_NoCode
;       and     w[esp].INTHFRM._efl,0011111111010101b	;clear NT
        and     w[esp].INTHFRM._efl,0011111111111111b	;clear NT

; MED 12/02/95
; if Exception Index is 0dh check if instruction is to be emulated.
        push    ds
        cmp     eax,0dh
        jne     mednoem                         ; not a GPF
        lds     eax,[esp+4].INTHFRM._cseip      ; get original CS:EIP into DS:EAX
        cmp     BYTE PTR [eax],0fh              ; first opcode byte
        jz      checkemu
mednoem:
        pop     ds
        mov     eax,[esp].INTHFRM._errC         ;get error code.
        mov     ExceptionCode,eax               ;/
        mov     eax,[esp].INTHFRM._efl          ;Get flags.
        or      eax,ISEXCEPTION                 ;RF used to indicate exception!?
        mov     ExceptionFlags,eax              ;Let dispatch know its an exception.
        mov     eax,cr2                         ;Grab this now to save more PL
        mov     ExceptionCR2,eax                ;switches for page faults.
        pop     eax
        pop     ds
        add     esp,4                           ;skip error code.
        jmp     inter14_SortedCode2

;--- no error code on stack

INTHFRM2 struc
_eax	dd ?	;eax
_ds		dd ?	;ds
		dd ?	;ret addr
		IRET32PL <>
INTHFRM2 ends

inter14_NoCode:
;       and     w[esp].INTHFRM2._efl,0011111111010101b	; clear NT
        and     w[esp].INTHFRM2._efl,0011111111111111b	; clear NT
        mov     eax,[esp].INTHFRM2._efl                 ;Get flags.
        and     eax,not ISEXCEPTION
        mov     ExceptionFlags,eax

        cmp ExceptionIndex,0            ; exc 00?
        jz inter14_ForceException

;--- force exception for int 6
        cmp ExceptionIndex,6            ; exc 06?
        jz inter14_ForceException

        cmp     ExceptionIndex,1        ;int 1
        jnz     inter14_SortedCode
inter14_ForceException:
        or      ExceptionFlags,ISEXCEPTION  ;force an exception.

inter14_SortedCode:
        pop     eax
        pop     ds
        assume ds:nothing
inter14_SortedCode2:
        add     esp,4           ;skip return address.
        ;
        ;Check which stack we should switch back to.
        ;
        cmp     w[esp].IRET32PL._ss,KernalSS
        jz      KernalStack             ;Already on system stack?
        test    ExceptionFlags,ISEXCEPTION  ;exception?
        jnz     KernalStack
        ;
        push    eax
        mov     eax,ExceptionIndex
        cmp     [eax][Int2CallCheck],-1  ;Hardware INT or int 1Ch, 23h, 24h?
        pop     eax
        jnz     KernalStack
        jmp     IntStack

        assume ds:nothing

;  byte[0] of instruction is 0Fh, check byte[1-2]:
;   mov eax,cr0 [0f 20 c0] or
;   mov eax,cr3 [0f 20 d8] or
;   mov cr0,eax [0f 22 c0] or
;   mov cr3,eax [0f 22 d8]
; MED 11/12/98:
;   RDMSR [0f 32]
; MED 04/05/99:
;   WBINVD [0f 09]
;   WRMSR [0f 30]
;   mov eax,cr4 [0f 20 e0]
;   mov cr4,eax [0f 22 e0]
; MED 05/02/2000:
;   mov ebx,cr4 [0f 20 e3]
;   mov eax,cr2 [0f 20 d0]
; if found, emulate it here and return.

checkemu:
        cmp     BYTE PTR [eax+1],20h            ; mov reg,crx
        jne     med6b
        cmp     BYTE PTR [eax+2],0c0h           ; mov eax,cr0?
        je      is_moveaxcr0
        cmp     BYTE PTR [eax+2],0d0h           ; mov eax,cr2?
        je      is_moveaxcr2
        cmp     BYTE PTR [eax+2],0d8h           ; mov eax,cr3?
        je      is_moveaxcr3
        cmp     BYTE PTR [eax+2],0e0h           ; mov eax,cr4?
        je      is_moveaxcr4
        cmp     BYTE PTR [eax+2],0e3h           ; mov ebx,cr4?
        je      is_movebxcr4
        jmp     mednoem
med6b:
        cmp     BYTE PTR [eax+1],22h            ; mov crx,reg?
        jne     med9b                           ; no match
        cmp     BYTE PTR [eax+2],0c0h           ; move cr0,eax?
        je      is_movcr0eax
        cmp     BYTE PTR [eax+2],0d8h           ; move cr3,eax?
        je      is_movcr3eax
        cmp     BYTE PTR [eax+2],0e0h           ; move cr4,eax?
        je      is_movcr4eax
        jmp     mednoem                         ; no match
med9b:
        cmp     BYTE PTR [eax+1],9              ; WBINVD?
        je      is_wbinvd
        cmp     BYTE PTR [eax+1],30h            ; WRMSR?
        je      is_wrmsr
        cmp     BYTE PTR [eax+1],32h            ; RDMSR?
        je      is_rdmsr
        jmp     mednoem

        .586

is_moveaxcr0:        
        mov     eax,cr0
        jmp     medemu3eax                      ; update original eax with cr0 value
is_moveaxcr2:        
        mov     eax,cr2
        jmp     medemu3eax                      ; update original eax with cr2 value
is_moveaxcr3:
        mov     eax,cr3
        jmp     medemu3eax                      ; update original eax with cr3 value
is_moveaxcr4:
        mov     eax,cr4
medemu3eax:
        mov     [esp+4].INTHFRM._eax,eax        ; update original eax with cr4 value
        jmp     medemu3
is_movebxcr4:
        mov     ebx,cr4
        jmp     medemu3
is_movcr0eax:        
        mov     eax,[esp+4].INTHFRM._eax
;        or      eax, 80000001h
        mov     cr0,eax
        jmp     medemu3
is_movcr3eax:
        mov     eax,[esp+4].INTHFRM._eax
        mov     cr3,eax
        jmp     medemu3
is_movcr4eax:
        mov     eax,[esp+4].INTHFRM._eax
        mov     cr4,eax
medemu3:
        mov     eax,3
        jmp     medemu
is_wbinvd:
        wbinvd
        jmp     medemu2
is_wrmsr:        
        mov     eax,[esp+4].INTHFRM._eax
        wrmsr
        jmp     medemu2
is_rdmsr:        
        rdmsr
        mov     [esp+4].INTHFRM._eax,eax
medemu2:
        mov     eax,2
medemu:
        add     [esp+4].INTHFRM._eip,eax        ; adjust EIP past emulated instruction
        pop     ds
        pop     eax
        pop     ds
        add     esp,8                           ; flush return address and error code off stack
        iretd

        .386p

IntHandler      endp


;-------------------------------------------------------------------------------
;
;It's a user stack and its not an exception or hardware interrupt so switch back
;to the original stack via a system stack to allow re-entrancy if original
;stack needs to be fetched from disk.
;
IntStack        proc    near

IRET16PL struc
union
 struct
_ip		dw ?
_cs		dw ?
 ends
_csip   dd ?
ends
_fl		dw ?
_sp		dw ?
_ss		dw ?
IRET16PL ends

INTSTKFRM struc
_eax	dd ?
_ebx	dd ?
_ds		dd ?
		IRET32PL <>
INTSTKFRM ends


        assume ds:nothing
        ;
        ;Get new stack address.
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:GROUP16
        mov     ebx,RawStackPos ;get next stack address.
        sub     RawStackPos,RawStackDif
        mov     ax,KernalSS
        mov     ds,eax
        assume ds:nothing
        ;
        ;Put old details onto new stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter15_iUse32
        sub     ebx, sizeof IRET16PL
        mov     eax,[esp].INTSTKFRM._eip
        mov     [ebx].IRET16PL._ip,ax
        mov     eax,[esp].INTSTKFRM._cs
        mov     [ebx].IRET16PL._cs,ax
        mov     eax,[esp].INTSTKFRM._efl
        mov     [ebx].IRET16PL._fl,ax
        mov     eax,[esp].INTSTKFRM._esp
        mov     [ebx].IRET16PL._sp,ax
        mov     eax,[esp].INTSTKFRM._ss
        mov     [ebx].IRET16PL._ss,ax
        jmp     inter15_iUse0
        ;
inter15_iUse32:
        sub     ebx,sizeof IRET32PL
        mov     eax,[esp].INTSTKFRM._eip
        mov     [ebx].IRET32PL._eip,eax
        mov     eax,[esp].INTSTKFRM._cs
        mov     [ebx].IRET32PL._cs,eax
        mov     eax,[esp].INTSTKFRM._efl
        mov     [ebx].IRET32PL._efl,eax
        mov     eax,[esp].INTSTKFRM._esp
        mov     [ebx].IRET32PL._esp,eax
        mov     eax,[esp].INTSTKFRM._ss
        mov     [ebx].IRET32PL._ss,eax
        ;
inter15_iUse0:
        ;Put new details into current stack.
        ;
        mov     eax,offset inter15_Int
        mov     [esp].INTSTKFRM._eip,eax
        mov     ax,DpmiEmuCS
        mov     [esp].INTSTKFRM._cs,eax
        pushfd
        pop     eax
        mov     [esp].INTSTKFRM._efl,eax
        mov     [esp].INTSTKFRM._esp,ebx
        mov     ax,KernalSS
        mov     [esp].INTSTKFRM._ss,eax
        pop     ds
        pop     ebx
        pop     eax
        iretd
        ;
inter15_Int:
        ;Now switch back to original stack.
        ;
        assume ds:nothing

        push    cs:ExceptionIndex       ;need to know the INT number.
        push    eax
        push    esi
        push    ds
        push    ebx

        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        add     RawStackPos,RawStackDif

        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter15_i2Use32
        movzx   ebx,sp
        lss     sp,d[ebx+5*4].IRET16PL._sp ;get original stack again.
        push    w[ebx+5*4].IRET16PL._fl
        push    w[ebx+5*4].IRET16PL._cs
        push    w[ebx+5*4].IRET16PL._ip
        xor     eax,eax
        mov     ax,[ebx+5*4].IRET16PL._fl
        jmp     inter15_i2Use0
        ;
inter15_i2Use32:
        mov     ebx,esp
        lss     esp,fword ptr [ebx+5*4].IRET32PL._esp
        push    d[ebx+5*4].IRET32PL._efl
        push    d[ebx+5*4].IRET32PL._cs
        push    d[ebx+5*4].IRET32PL._eip
        mov     eax,[ebx+5*4].IRET32PL._efl
inter15_i2Use0:
        and     ah,11111100b                 ;lose IF & TF
        push    eax                          ;int handler flags.
        ;
        mov     esi,[ebx+4*4]   ;get INT index.
        shl     esi,1
        lea     esi,[esi*2+esi] ;*6 for index into table.
        add     esi,offset InterruptTable
        movzx   eax,word ptr cs:[esi+4] ;int handler CS
        push    eax
        push    d cs:[esi+0]    ;int handler EIP.
        mov     eax,[ebx+3*4]   ;EAX
        mov     esi,[ebx+2*4]   ;ESI
        lds     ebx,[ebx]
        iretd                   ;pass control to INT handler.
IntStack        endp

        assume ds:nothing

;-------------------------------------------------------------------------------
;
;Either we were already on the system stack or we have an exception or hardware
;interrupt on our hands. Either way we need to switch to another piece of the
;system stack to make sure we're using a legitimate one.
;
KernalStack     proc    near

KRNSTKFRM struc
_eax	dd ?
_ebx	dd ?
_ds		dd ?
		IRET32PL <>
KRNSTKFRM ends

        assume ds:nothing
        ;
        ;Get new stack address.
        ;
        push    ds
        push    ebx
        push    eax
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:GROUP16
        mov     ebx,RawStackPos ;get next stack address.
        sub     RawStackPos,RawStackDif
        mov     ax,KernalSS
        mov     ds,eax
        assume ds:nothing
        ;
inter16_Update:
        ;Put old details onto new stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter16_Use32
        sub     ebx, sizeof IRET16PL
        mov     eax,[esp].KRNSTKFRM._eip
        mov     [ebx].IRET16PL._ip,ax
        mov     eax,[esp].KRNSTKFRM._cs
        mov     [ebx].IRET16PL._cs,ax
        mov     eax,[esp].KRNSTKFRM._efl
        mov     [ebx].IRET16PL._fl,ax
        mov     eax,[esp].KRNSTKFRM._esp
        mov     [ebx].IRET16PL._sp,ax
        mov     eax,[esp].KRNSTKFRM._ss
        mov     [ebx].IRET16PL._ss,ax
        jmp     inter16_Use0
        ;
inter16_Use32:
        sub     ebx,sizeof IRET32PL
        mov     eax,[esp].KRNSTKFRM._eip
        mov     [ebx].IRET32PL._eip,eax
        mov     eax,[esp].KRNSTKFRM._cs
        mov     [ebx].IRET32PL._cs,eax
        mov     eax,[esp].KRNSTKFRM._efl
        mov     [ebx].IRET32PL._efl,eax
        mov     eax,[esp].KRNSTKFRM._esp
        mov     [ebx].IRET32PL._esp,eax
        mov     eax,[esp].KRNSTKFRM._ss
        mov     [ebx].IRET32PL._ss,eax
        ;
inter16_Use0:
        ;Put new details into current stack.
        ;
        mov     eax,offset IntDispatch
        mov     [esp].KRNSTKFRM._eip,eax
        mov     eax,DpmiEmuCS
        mov     [esp].KRNSTKFRM._cs,eax
        pushfd
        pop     eax
        mov     [esp].KRNSTKFRM._efl,eax
        mov     [esp].KRNSTKFRM._esp,ebx
        mov     eax,KernalSS
        mov     [esp].KRNSTKFRM._ss,eax
        pop     eax
        pop     ebx
        pop     ds
        ;
        ;Pass control to interrupt dispatcher.
        ;
        iretd
KernalStack     endp


;-------------------------------------------------------------------------------
;
;Direct control to exception or interrupt handler.
;
IntDispatch     proc    near
        push    eax
        push    esi
        push    edi
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        and     w[ExceptionFlags],1111110011111111b   ; clear IF & TF
        mov     esi,ExceptionIndex      ;Get the exception number.
        shl     esi,1
        lea     esi,[esi*2+esi] ;*6
        test    ExceptionFlags,ISEXCEPTION
        jnz     inter17_Excep
        ;
        ;Dispatch normal interrupt.
        ;
        add     esi,offset InterruptTable
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        test    BYTE PTR DpmiEmuSystemFlags,SF_16BIT
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     eax
        jz      inter17_Use32Bit14
        push    WORD PTR cs:[ExceptionFlags]    ;Flags before entry.
        db      66h  ; make the following push a 16-bit push
        push    cs
        push    lowword offset inter17_Resume
        jmp     cs:[inter17_Call2]
        ;
inter17_Use32Bit14:
        push    cs:[ExceptionFlags]             ;EFlags before entry.
        push    cs
        push    offset inter17_Resume
        jmp     cs:[inter17_Call2]

IDSTKFRM struc
_ebx	dd ?
_ds		dd ?
_eax	dd ?
union
_efl	dd ?
_fl		dw ?
ends
IDSTKFRM ends

inter17_Resume:
        ;Return from normal int.
        ;
        pushfd
        cli                     ;stop interrupts interfering.
        push    eax
        push    ds
        push    ebx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        add     RawStackPos,RawStackDif ;update next stack.
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        mov     ebx,esp
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter17_Use32
        ;
        movzx   ebx,bx
        lss     sp,d[ebx+4*4].IRET16PL._sp ;get old stack address.
        and     [ebx+4*4].IRET16PL._fl,0000011100000000b ;retain IF & TF & DF.
        and     [ebx].IDSTKFRM._fl,1111100011111111b     ;lose IF & TF & DF.
        mov     ax,[ebx].IDSTKFRM._fl
        or      ax,[ebx+4*4].IRET16PL._fl
        push    ax                   ;EFlags
        push    [ebx+4*4].IRET16PL._cs
        push    [ebx+4*4].IRET16PL._ip
        mov     eax,[ebx].IDSTKFRM._eax
        lds     ebx,fword ptr [ebx].IDSTKFRM._ebx
        iret
        ;
inter17_Use32:
        lss     esp,fword ptr [ebx+4*4].IRET32PL._esp
        and     w[ebx+4*4].IRET32PL._efl,0000011100000000b ;retain IF & TF & DF.
        and     w[ebx].IDSTKFRM._fl,1111100011111111b      ;lose IF & TF & DF.
        mov     eax,[ebx].IDSTKFRM._efl
        or      eax,[ebx+4*4].IRET32PL._efl
        push    eax                   ;EFlags
        push    [ebx+4*4].IRET32PL._cs
        push    [ebx+4*4].IRET32PL._eip
        mov     eax,[ebx].IDSTKFRM._eax
        lds     ebx,fword ptr [ebx].IDSTKFRM._ebx
        iretd

        ;
inter17_Excep:
        ;Dispatch exception.
        ;
        assume ds:DPMIGRP
        add     esi,offset ExceptionTable
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        test    BYTE PTR DpmiEmuSystemFlags,SF_16BIT
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     eax
        jz      inter17_eUse32Bit14
        push    WORD PTR cs:[ExceptionCode]     ;EFlags before entry.
        db      66h
        push    cs
        push    lowword offset inter17_ResumeExc16
        jmp     cs:[inter17_Call2]
        ;
inter17_eUse32Bit14:
        push    cs:[ExceptionCode]              ;EFlags before entry.
        push    cs
        push    offset inter17_ResumeExc32
        jmp     cs:[inter17_Call2]
        ;

RESTKFRM struc
_ebx	dd ?
_ds		dd ?
_eax	dd ?
RESTKFRM ends
        ;
        ;Return from exception.
        ;
inter17_ResumeExc16:
        cli
        add     sp,2
        push    eax
        push    ds
        push    ebx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        add     RawStackPos,RawStackDif ;update next stack.
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        movzx   ebx,sp
        lss     sp,d[ebx+3*4].IRET16PL._sp ;get old stack address.
        push    [ebx+3*4].IRET16PL._fl
        push    [ebx+3*4].IRET16PL._cs
        push    [ebx+3*4].IRET16PL._ip
        mov     eax,[ebx].RESTKFRM._eax
        lds     ebx,fword ptr [ebx].RESTKFRM._ebx
        iret

inter17_ResumeExc32:
        cli
        add     esp,4
        push    eax
        push    ds
        push    ebx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        add     RawStackPos,RawStackDif ;update next stack.
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        mov     ebx,esp
        lss     esp,fword ptr [ebx+3*4].IRET32PL._esp
        push    [ebx+3*4].IRET32PL._efl
        push    [ebx+3*4].IRET32PL._cs
        push    [ebx+3*4].IRET32PL._eip
        mov     eax,[ebx].RESTKFRM._eax
        lds     ebx,fword ptr [ebx].RESTKFRM._ebx
        iretd
        ;
IntDispatch     endp

;-------------------------------------------------------------------------------
;
;Handle an INT nn instruction by retrieving registers from the stack and
;reflect to real mode.
;
;frame:
INTNN386FR struc
	dd 8 dup (?)	;pushad
	dw 10 dup (?)	;10*2 (RMCS fl,es,ds,fs,gs,ip,cs,sp,ss)
	dd 2 dup (?)    ;push ds - push es
newfl   dd ?        ;sub esp,4+4
dwCbIdx dd ?        ;-1 or, if IRQ, stored ICallBack #
retaddr dd ?        ;return address
union
i16	IRET16 <>
i32	IRET32 <>
ends
INTNN386FR ends

        assume ds:nothing

IntNN386        proc    far
if INTNN386SIZE eq 4
        sub     esp,4+4+2
else
        sub     esp,4+4
endif
        push    ds
        push    es
        sub     esp,10*2    ; room for a (dword aligned) RealRegsStruc
        pushad
        ;
        mov     ebp,esp
        test    BYTE PTR DpmiEmuSystemFlags,SF_16BIT
        jz      inter18_Use32Bit19
        movzx   ebp,bp
        movzx   eax,[ebp].INTNN386FR.i16._fl
        jmp     inter18_Use16Bit19
inter18_Use32Bit19:
        mov     eax,[ebp].INTNN386FR.i32._fl
inter18_Use16Bit19:
        mov     [ebp].INTNN386FR.newfl,eax
if INTNN386SIZE eq 4
        movzx   edx,w[ebp].INTNN386FR.retaddr+2
        sub     edx,offset IntNN386Catch + 4
        shr     edx,2
else
        mov     edx,[ebp].INTNN386FR.retaddr
        sub     edx,offset IntNN386Catch + 5
        shr     edx,3
endif
        mov     [ebp].INTNN386FR.dwCbIdx,-1
        ;
        mov     edi,ebp
        push    ss
        pop     es
        ;
        ;If this is a "hardware" interrupt we should mark it as
        ;busy if it isn't already marked.
        ;
        mov     ebx,edx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     al,[ebx][Int2CallCheck]
        or      al,al           ;Hardware?
        js      inter18_c2
        mov     bl,al
        bt      [ICallBackUsed],ebx
        jnc     inter18_c2
        bts     [ICallBackBusy],ebx    ;already busy?
        jc      inter18_c2
        mov     [ebp].INTNN386FR.dwCbIdx,ebx
inter18_c2:
        ;
        ;Now pass control to the INT simulator.
        ;
        mov     eax,[ebp].INTNN386FR.newfl
        mov     es:[edi].RealRegsStruc.Real_Flags,ax

;--- since "USEPMFLAGS equ 0" in rawvcpi2.inc, there's no need to modify EFL here
;        and     ah,11111000b   ; clear TF, IF & DF
;        push    eax
;        popfd

        mov     ebx,edx
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateInt
        ;
        cmp     [ebp].INTNN386FR.dwCbIdx,-1
        jz      inter18_NoCall
        mov     ebx,[ebp].INTNN386FR.dwCbIdx
        btr     [ICallBackBusy],ebx
inter18_NoCall:
        ;
        ;Update the flags.
        ;
        mov     ax,es:[edi].RealRegsStruc.Real_Flags
        and     ax,1000100011111111b            ;lose TF, IF, DF, IOPL, NT
        ;
        test    BYTE PTR DpmiEmuSystemFlags,SF_16BIT
        jz      inter18_Use32Bit
        mov     bx,[ebp].INTNN386FR.i16._fl
        and     bx,0111011100000000b            ;retain TF, IF, DF, IOPL, NT
        or      ax,bx
        mov     [ebp].INTNN386FR.i16._fl,ax
        popad
        add     esp,10*2
        pop     es
        pop     ds
        add     esp,4+4+4
        iret                    ;Switch back to calling program.
        ;
inter18_Use32Bit:
        mov     ebx,[ebp].INTNN386FR.i32._fl
        and     bx,0111011100000000b            ;retain TF, IF, DF, IOPL, NT
        or      ax,bx
        mov     w[ebp].INTNN386FR.i32._fl,ax
        popad
        add     esp,10*2
        pop     es
        pop     ds
        assume ds:nothing
        add     esp,4+4+4       ;skip newfl,dwCbIdx,retaddr
        iretd                   ;Switch back to calling program.
IntNN386        endp



;-------------------------------------------------------------------------------
;
;An unhandled low level exception has occured so terminate the program.
;
EXCFR16 struc
        EXCFRX <>
_ip     dw ?
_cs     dw ?
_fl     dw ?
EXCFR16 ends

EXCFR32 struc
        EXCFRX <>
_ip     dd ?
_cs     dd ?
_fl     dd ?
EXCFR32 ends

ExcepNN386      proc    far
if EXCNN386SIZE eq 4
        sub     esp, 2
endif        
        push    eax
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
if EXCNN386SIZE eq 4
        movzx   eax,w[esp+(4+4)+2]
        sub     eax,offset ExcepNN386Catch+4
        shr     eax,2
else
        mov     eax,[esp+(4+4)]
        sub     eax,offset ExcepNN386Catch+5
        shr     eax,3
endif
        mov     ExceptionIndex,eax
        pop     ds
        pop     eax
        add     esp,4           ;skip return address
        ;
        push    ds              ;push regs so to create an EXCFRX struct
        push    es
        push    fs
        push    gs
        pushad
        mov     ax,DpmiEmuDS
        mov     ds,eax
        test    BYTE PTR DpmiEmuSystemFlags,SF_16BIT
        jz      inter19_Use32Bit16
        movzx   eax,[esp].EXCFR16._fl
        jmp     inter19_Use16Bit16
inter19_Use32Bit16:
        mov     eax,[esp].EXCFR32._fl
inter19_Use16Bit16:
        mov     ExceptionEFL,eax
        ;Retrieve register values and get outa here.
        ;
        mov     ax,DpmiEmuDS
        mov     es,eax
        assume es:DPMIGRP
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        mov     esi,esp
        mov     edi,offset ExceptionRegs
        mov     ecx,sizeof EXCFRX / 4
        cld
        rep     movsd [edi],[esi]       ;copy registers off the stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter19_Use32Bit17
        xor     eax, eax
        add     esi,2+2+2               ;skip return address/flags.
        lodsw   [esi]                   ;copy IRET16 -> IRET32
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        lodsw   [esi]
        stosd   [edi]
        jmp     inter19_Use16Bit678
inter19_Use32Bit17:
        add     esi,4+4+4               ;skip return address/flags.
        movsd   [edi],[esi]             ;copy IRET32
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
inter19_Use16Bit678:
        push    es
        pop     ds
        assume es:nothing
        assume ds:DPMIGRP
;
;Now switch to PL0 to get CR0-3 values.
;
        mov     eax, offset inter19_pl0
        call    EmuCallPL0Proc
;
;Convert register values into ASCII ready for printing;
;use the transfer buffer.
;
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        movzx   edi,TransferReal
        pop     ds
        assume ds:DPMIGRP
        mov     ax,KernalZero
        mov     es,eax
        push    edi
        shl     edi,4
        mov     esi, offset ExceptionHeader
        mov     ebx, offset stdregdump2
        call    myprintf2
        pop     eax

        push    ds
        pop     es
;
;Print the message.
;
        mov     edi,offset ExceptionIntBuffer
        mov     RealRegsStruc.Real_DS[edi],ax
        mov     RealRegsStruc.Real_DX[edi],0
        mov     RealRegsStruc.Real_AX[edi],0900h
        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
;
;Now switch back to exit code.
;
if 0 ; _cwInit most likely doesn't exist anymore
        mov     ax,InitDS
        mov     ds,eax
        assume ds:GROUP16
        mov     WORD PTR IErrorNumber,0
        mov     ax,InitCS
        push    ax
        mov     ax,offset InitError
        push    ax
        retw
else
        mov     w[esp].EXCFRX._ds,KernalDS
        popad
        pop gs
        pop fs
        pop es
        pop ds
        assume ds:GROUP16
        jmp [TerminationHandler]
endif

        assume ds:DPMIGRP

inter19_pl0:        
        .386P
        str     ax              ;get TR
        mov     ExceptionTR,ax
        mov     eax,cr0
        mov     ExceptionCR0,eax
        mov     eax,cr2
        mov     ExceptionCR2,eax
        mov     eax,cr3
        mov     ExceptionCR3,eax
        retd
        .386

ExcepNN386      endp

        assume ds:nothing

;-------------------------------------------------------------------------------
;
;Convert number into ASCII Hex version.
;
;On Entry:-
;
;EAX    - Number to convert.
;ECX    - Digits to do.
;ES:EDI - Buffer to put string in.
;
Bin2HexE        proc    near
        add     edi,ecx
        push    edi
        mov     edx,eax
rv62_0:
        dec     edi
        mov     al,dl
        shr     edx,4
        and     al,0Fh
        add     al,'0'
        cmp     al,'9'
        jbe     @F
        add     al,7
@@:
        mov     es:[edi],al
        loop    rv62_0
        pop     edi
        ret
Bin2HexE        endp

;-------------------------------------------------------------------------------
;formatted output.
;DS:ESI - format string
;DS:EBX - argument vector
;ES:EDI - output buffer

myprintf2 proc
nextchar:
        lodsb
        cmp al,0
        jz done
        cmp al,'%'
        jnz normchar
        lodsb
        sub al,'0'
        movzx ecx,al
        movzx edx,byte ptr [ebx]
        inc ebx
        lea edx,[edx+offset ExcFields]
        mov eax,[edx]
dispnum:
        call Bin2HexE
        jmp nextchar
normchar:
        stosb
        jmp nextchar
done:
        ret
myprintf2 endp

;-------------------------------------------------------------------------------
;
;Get protected mode interrupt handler address.
;
;On Entry:-
;
;BL     - Interrupt vector number.
;
;On Exit:-
;
;CF set on error. (no errors looked for).
;
;CX:EDX - selector:offset of current handler.
;
;  DPMI dispatcher ensures that hiword(edx) is restored for 16-bit.
;
RawGetVector    proc    near
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        movzx   ebx,bl
        shl     ebx,1                   ;*2 (together with next line gives ebx=ebx*6)
        lea     ebx,[ebx*2+ebx+offset InterruptTable]
        mov     edx,[ebx+0]             ;get offset.
        mov     cx,[ebx+4]              ;get segment selector.
        pop     ds
        pop     ebx
        pop     eax
        ret
RawGetVector    endp


;-------------------------------------------------------------------------------
;
;Get real mode interrupt handler address.
;
;On Entry:-
;
;BL     - Interrupt vector number.
;
;On Exit:-
;
;CF set on error. (no errors looked for).
;
;CX:DX  - selector:offset of current handler.
;
RawGetRVector   proc    near
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,KernalZero
        mov     ds,eax
        movzx   ebx,bl
        shl     ebx,2
        mov     dx,[ebx+0]
        mov     cx,[ebx+2]
        pop     ds
        pop     ebx
        pop     eax
        ret
RawGetRVector   endp


;-------------------------------------------------------------------------------
;
;Get protected mode exception handler address.
;
;On Entry:-
;
;BL     - Exception vector number.
;
;On Exit:-
;
;CF set on error.
;
;CX:EDX - selector:offset of current handler.
;
;  DPMI dispatcher ensures that hiword(edx) is restored for 16-bit.
;
RawGetEVector   proc    near
        ;
        push    ds
        push    ebx
        push    eax                     ; push last so is easily accessible for changing
        mov     ax,DpmiEmuDS
        mov     ds,eax

        cmp     bl,14           ;Special case for 14
        je      Special14       ; MED 01/17/96
        cmp     bl,20h
        jc      inter8_Normal
        mov     ax,8021h        ; flag invalid value
        stc                     ; flag error
        jmp     inter8_err      ; don't get vector

Special14:
        cmp     w[OldExcep14+4],0       ;Virtual memory active?
        jz      inter8_Normal
        ;
        ;Vector 14 and VMM is installed.
        ;
        mov     edx,d[OldExcep14+0]
        mov     cx,w[OldExcep14+4]
        jmp     inter8_GotVect
        ;
inter8_Normal:
        movzx   ebx,bl
        shl     ebx,1
        lea     ebx,[ebx*2+ebx]         ;*6
        add     ebx,offset ExceptionTable
        mov     edx,[ebx+0]             ;get offset.
        mov     cx,[ebx+4]              ;get segment selector.
        clc
inter8_GotVect:
        pop     eax
inter8_err:
        pop     ebx
        pop     ds
        ret
RawGetEVector   endp


;-------------------------------------------------------------------------------
;
;Set protected mode interrupt handler address. Int 31h, ax=205h
;
;On Entry:-
;
;BL     - Interrupt vector number.
;CX:EDX - selector:offset of new handler.
;
;  DPMI dispatcher ensures that hiword(edx) is cleared for 16-bit.
;
;On Exit:-
;
;CF set on error.
;
RawSetVector    proc    near
        ;
        push    eax
        push    ebx
        push    edx
        push    ds
        push    es
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      inter9_use32_add
        movzx   edx,dx
        ;
inter9_use32_add:
        pushfd
        cli
        ;Check if its a hardware interrupt.
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        movzx   ebx,bl
        mov     al,[ebx][Int2CallCheck]
        or      al,al
        js      inter9_NotHardware

;*** MED 11/30/95
        cmp     bl,23h                  ; always allow 23h callback
        je      med2a
        cmp     bl,24h                  ; always allow 24h callback
        je      med2a
        test    NoPassFlag,0ffh         ; CAUSEWAY=NOPASS set?
        jne     inter9_NotHardware
        ;
med2a:
        pushad
        mov     edx,ebx                 ; save Int#
        mov     bl,al                   ; now BL=Callback# (1Ch->16, 23h->17, 24h->18)
        mov     ax,KernalZero
        mov     es,eax
        mov     eax,ebx                 ; save callback#
        cmp     cx,DpmiEmuCS            ; restoreing previous vector?
        jnz     inter9_Setting
;inter9_Restoreing:
        btr     [ICallBackUsed],ebx     ;Mark this one as un-used
        jnc     inter9_DoneHardware     ;do nothing if it wasn't used (not likely)
        mov     eax,[ebx*4][ICallBackOldVec] ;get original real mode vector.
        mov     es:[edx*4],eax
        jmp     inter9_DoneHardware
inter9_Setting:
        bts     [ICallBackUsed],ebx     ;this one in use?
        jc      inter9_DoneHardware
        mov     [ebx][ICallBackIntNo],dl;set interrupt number.
        mov     si,GROUP16
        add     si,ax
        shl     esi,16
        mov     si,offset RawIntCallBack
        shl     ax,4
        sub     si,ax
        mov     eax,es:[edx*4]                ;get old IVT value
        mov     es:[edx*4],esi                ;set new IVT value
        mov     [ebx*4][ICallBackOldVec],eax  ;store original real mode vector.
inter9_DoneHardware:
        popad
inter9_NotHardware:
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        shl     ebx,1           ;*2 (with next line's *3 it will be *6
        lea     ebx,[ebx*2+ebx+offset InterruptTable]
        mov     [ebx+0],edx             ;set offset.
        mov     [ebx+4],cx              ;set segment selector.
        pop     eax
        test    ah,2
        jz      @F
        sti
@@:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edx
        pop     ebx
        pop     eax
        ret
RawSetVector    endp


;-------------------------------------------------------------------------------
;
;Set real mode interrupt handler address. Int 31h, ax=201h
;
;On Entry:-
;
;BL     - Interrupt vector number.
;CX:DX  - selector:offset of new handler.
;
;On Exit:-
;
;CF set on error.
;
RawSetRVector   proc    near
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,KernalZero
        mov     ds,eax
        movzx   ebx,bl
        shl     ebx,2
        pushfd
        cli
        mov     [ebx+0],dx
        mov     [ebx+2],cx
        pop     eax
        test    ah,2
        jz      @F
        sti
@@:
        pop     ds
        pop     ebx
        pop     eax
        ret
RawSetRVector   endp


;-------------------------------------------------------------------------------
;
;Set protected mode exception handler address. Int 31h, ax=203h
;
;On Entry:-
;
;BL     - Exception vector number.
;CX:EDX - selector:offset of new handler.
;
;  DPMI dispatcher ensures that hiword(edx) is cleared for 16-bit.
;
;On Exit:-
;
;CF set on error.
;
RawSetEVector   proc    near
        ;
        push    ds
        push    ebx
        push    eax                     ; push last so is easily accessible for changing

        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        movzx   ebx,bl
        cmp     bl,14           ;Special case for 14
        je      Special14x
        cmp     bl,20h
        jc      inter11_Normal
        pop     eax
        mov     ax,8021h        ; flag invalid value
        stc                     ; flag error
        jmp     inter11_exit    ; don't set vector
Special14x:
        cmp     w[OldExcep14+4],0 ;Virtual memory active?
        jz      inter11_Normal
        mov     ebx,offset OldExcep14
        jmp     inter11_specific
        ;
inter11_Normal:
        shl     ebx,1           ;*2
        lea     ebx,[ebx*2+ebx] ;*6
        add     ebx,offset ExceptionTable
inter11_specific:
        mov     [ebx+0],edx             ;set offset.
        mov     [ebx+4],cx              ;set segment selector.
        clc
        pop     eax
inter11_exit:
        pop     ebx
        pop     ds
        assume ds:nothing
        ret
RawSetEVector   endp

