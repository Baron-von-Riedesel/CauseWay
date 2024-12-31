;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Protected mode interrupt/exception handlers.
;for RAW/VCPI
;

;-------------------------------------------------------------------------------

	align 4
;
DpmiEmuSystemFlags dd 0
;
; do NOT change order of these variables (you can add at the end), MED 01/08/96
ExceptionCode   dd 0
ExceptionFlags  dd 0    ;bit 16==1->exception
ExceptionIndex  dd 0
;

EXCFRX  struc
_ebp    dd ?
_edi    dd ?
_esi    dd ?
_edx    dd ?
_ecx    dd ?
_ebx    dd ?
_eax    dd ?
_gs     dw ?,?
_fs     dw ?,?
_es     dw ?,?
_ds     dw ?,?
EXCFRX  ends

ExceptionRegs EXCFRX <>
if 0
ExceptionEBP    dd ?
ExceptionEDI    dd ?
ExceptionESI    dd ?
ExceptionEDX    dd ?
ExceptionECX    dd ?
ExceptionEBX    dd ?
ExceptionEAX    dd ?
ExceptionGS     dw ?,?
ExceptionFS     dw ?,?
ExceptionES     dw ?,?
ExceptionDS     dw ?,?
endif
ExceptionEIP    dd ?
ExceptionCS     dw ?,?
ExceptionEFL    dd ?
ExceptionESP    dd ?
ExceptionSS     dw ?,?

ExceptionTR     dw ?
ExceptionCR0    dd ?
ExceptionCR2    dd ?
ExceptionCR3    dd ?
;
ExceptionHeader label byte
        db 13,10,"CauseWay Error 09 : Unrecoverable internal exception, program terminated.",13,10
        db 13,10,'Exception: '
ExceptionINum   db '00, Error code: '
ExceptionENum   db '0000',13,10,13,10
        db 'EAX='
ExceptionEAXt   db '00000000 '
        db 'EBX='
ExceptionEBXt   db '00000000 '
        db 'ECX='
ExceptionECXt   db '00000000 '
        db 'EDX='
ExceptionEDXt   db '00000000 '
        db 'ESI='
ExceptionESIt   db '00000000 '
        db 13,10
        db 'EDI='
ExceptionEDIt   db '00000000 '
        db 'EBP='
ExceptionEBPt   db '00000000 '
        db 'ESP='
ExceptionESPt   db '00000000 '
        db 'EIP='
ExceptionEIPt   db '00000000 '
        db 'EFL='
ExceptionEFLt   db '00000000 '
        db 13,10,13,10
        db 'CS='
ExceptionCSt    db '0000 '
        db 'DS='
ExceptionDSt    db '0000 '
        db 'ES='
ExceptionESt    db '0000 '
        db 'FS='
ExceptionFSt    db '0000 '
        db 'GS='
ExceptionGSt    db '0000 '
        db 'SS='
ExceptionSSt    db '0000 '
        db 13,10,13,10
        db 'CR0='
ExceptionCR0t   db '00000000 '
        db 'CR2='
ExceptionCR2t   db '00000000 '
        db 'CR3='
ExceptionCR3t   db '00000000 '
        db 'TR='
ExceptionTRt    db '0000'
        db 13,10,13,10
        db 'SystemFlags='
ExceptionSysFlags db '00000000 '
        db 13,10,13,10,'$'
ExceptionHeaderEnd      label byte
;

	align 4

ExceptionIntBuffer db size RealRegsStruc dup (?)
;
	align 4
;
;Generate the initial entry points for the interupt handlers.
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

InterruptTable  label fword
;
;Interupt handler entry points (Int nn and IntR).
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


;-------------------------------------------------------------------------------
ExcepNN386Catch label near
        rept 32
;        db 0e8h
;        dd offset ExcepNN386-($+4)
if EXCNN386SIZE eq 4
        call near16 ptr ExcepNN386
else
        call ExcepNN386
        db 3 dup (-1)
endif
        endm

;-------------------------------------------------------------------------------
;
;Get protected mode interupt handler address.
;
;On Entry:-
;
;BL     - Interupt vector number.
;
;On Exit:-
;
;CF set on error. (no errors looked for).
;
;CX:EDX - selector:offset of current handler.
;
RawGetVector    proc    near
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        movzx   ebx,bl
        imul    ebx,6
        add     ebx,offset InterruptTable
        mov     edx,[ebx+0]             ;get offset.
        mov     cx,[ebx+4]              ;get segment selector.
        pop     ds
        pop     ebx
        pop     eax
        ret
RawGetVector    endp


;-------------------------------------------------------------------------------
;
;Get real mode interupt handler address.
;
;On Entry:-
;
;BL     - Interupt vector number.
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
        imul    ebx,6
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
;Set protected mode interupt handler address.
;
;On Entry:-
;
;BL     - Interupt vector number.
;CX:EDX - selector:offset of new handler.
;
;On Exit:-
;
;CF set on error.
;
RawSetVector    proc    near
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    ds
        push    es
        mov     ax,DpmiEmuDS
        mov     ds,eax
        test    BYTE PTR DpmiEmuSystemFlags,1
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
        assume ds:_cwRaw
        movzx   ebx,bl
        mov     al,[ebx+Int2CallCheck]
        or      al,al
        jz      inter9_NotHardware

;*** MED 11/30/95
        cmp     bl,23h                  ; always allow 23h callback
        je      med2a
        cmp     bl,24h                  ; always allow 24h callback
        je      med2a
        test    BYTE PTR NoPassFlag,0ffh        ; see if not passing hardware interrupts from real to protected mode
        jne     inter9_NotHardware
        ;
med2a:
        cmp     cx,DpmiEmuCS            ;restoreing previous vector?
        jnz     inter9_Setting
inter9_Restoreing:
        pushad
        movzx   cx,bl
        sub     bl,al                   ; now BL=Callback# (1Ch->16, 23h->17, 24h->18)
        movzx   bx,bl
        mov     ax,size CallBackStruc
        mul     bx
        mov     bx,ax
        add     bx,offset CallBackTable
        test    [bx].CallBackStruc.CallBackFlags,1      ;this one in use?
        jz      inter9_DoneHardware     ;not likely.
        push    es
        mov     ax,KernalZero
        mov     es,eax
        mov     [bx].CallBackStruc.CallBackFlags,0      ;Mark this one as un-used.
        push    cx
        mov     dx,w[bx].CallBackStruc.CallBackReal+0   ;get original real mode vector.
        mov     cx,w[bx].CallBackStruc.CallBackReal+2
        pop     bx
        shl     bx,2
        mov     es:[bx+0],dx
        mov     es:[bx+2],cx
        pop     es
        jmp     inter9_DoneHardware
inter9_Setting:
        pushad
        movzx   cx,bl
        sub     bl,al
        movzx   bx,bl
        mov     ax,size CallBackStruc
        mul     bx
        mov     dx,bx
        mov     bx,ax
        add     bx,offset CallBackTable
        test    [bx].CallBackStruc.CallBackFlags,1      ;this one in use?
        jnz     inter9_DoneHardware
        push    es
        mov     ax,KernalZero
        mov     es,eax
        mov     [bx].CallBackStruc.CallBackNum,cl       ;set interupt number.
        mov     [bx].CallBackStruc.CallBackFlags,1+2    ;mark call back as used interupt.
        mov     ax,CallBackSize
        mul     dx
        mov     si,offset ICallBackList
        add     si,ax           ;index list of calls.
        push    bx
        mov     bx,cx
        shl     bx,2
        mov     dx,es:[bx+0]
        mov     cx,es:[bx+2]
        mov     es:[bx+0],si
        mov     WORD PTR es:[bx+2],seg _cwRaw
        pop     bx
        mov     w[bx].CallBackStruc.CallBackReal+0,dx   ;store original real mode vector.
        mov     w[bx].CallBackStruc.CallBackReal+2,cx
        pop     es
inter9_DoneHardware:
        popad
inter9_NotHardware:
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:_cwDPMIEMU
        movzx   ebx,bl
        imul    ebx,6
        add     ebx,offset InterruptTable
        mov     [ebx+0],edx             ;set offset.
        mov     [ebx+4],cx              ;set segment selector.
        pop     eax
        test    ah,2
        jz      @F
        sti
@@:
        pop     es
        pop     ds
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
RawSetVector    endp


;-------------------------------------------------------------------------------
;
;Set real mode interupt handler address.
;
;On Entry:-
;
;BL     - Interupt vector number.
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
;Set protected mode exception handler address.
;
;On Entry:-
;
;BL     - Exception vector number.
;CX:EDX - selector:offset of new handler.
;
;On Exit:-
;
;CF set on error.
;
RawSetEVector   proc    near
        ;
        push    ds
        push    es
        push    fs
        push    gs
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    eax                     ; push last so is easily accessible for changing

        mov     ax,DpmiEmuDS
        mov     ds,eax
        movzx   eax,bl
        push    eax
        mov     ebx,eax
        shl     ebx,1           ;*2
        mov     eax,ebx
        shl     ebx,1           ;*4
        add     ebx,eax         ;*6
        pop     eax
        add     ebx,offset ExceptionTable
        ;
        cmp     al,14           ;Special case for 14

;       jnz     @@Normal
        je      Special14x      ; MED 01/17/96
        cmp     al,20h
        jc      inter11_Normal
        stc                     ; flag error
        pop     eax
        mov     ax,8021h        ; flag invalid value
        push    eax
        jmp     inter11_GotVect ; don't set vector

Special14x:
        cmp     w[OldExcep14+4],0       ;Virtual memory active?
        jz      inter11_Normal
        ;
        ;Vector 14 and VMM is still installed.
        ;
        mov     d[OldExcep14+0],edx
        mov     w[OldExcep14+4],cx
        jmp     inter11_GotVect
        ;
inter11_Normal:
        clc             ; MED 01/17/96, flag no error

        mov     [ebx+0],edx             ;set offset.
        mov     [ebx+4],cx              ;set segment selector.
inter11_GotVect:
        pop     eax
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     gs
        pop     fs
        pop     es
        pop     ds
        ret
RawSetEVector   endp


;-------------------------------------------------------------------------------
;
;Allocate a real mode call back address.
;
;On Entry:-
;
;DS:ESI - Protected mode code.
;ES:EDI - Real mode register structure.
;
;On Exit:-
;
;Carry set on error, else,
;
;CX:DX  - Real mode address allocated.
;
RAWGetCallBack  proc near
        ;
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     eax,ds
        push    eax
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        pop     eax
        ;
;;MED 02/16/96
        mov     ebx,offset CallBackTable+((size CallBackStruc)*(16+3))
        mov     ecx,MaxCallBacks-(16+3)
        mov     edx,16+3
;        mov     ebx,offset CallBackTable+((size CallBackStruc)*(16+4))
;        mov     ecx,MaxCallBacks-(16+4)
;        mov     edx,16+4

inter12_0:
        test    CallBackStruc.CallBackFlags[ebx],1              ;this one in use?
        jz      inter12_1
        add     ebx,size CallBackStruc
        inc     edx
        dec     ecx
        jnz     inter12_0
        jmp     inter12_9
        ;
inter12_1:
        pushad
        xor     eax,eax
        mov     cx,1
        int     31h
        mov     w[ebx].CallBackStruc.CallBackStackSel,ax
        popad
        jc      inter12_9
        pushad
        mov     bx,w[ebx].CallBackStruc.CallBackStackSel
        mov     eax,8
        xor     ecx,ecx
        or      edx,-1
        int     31h
        popad
        ;
        mov     d[ebx].CallBackStruc.CallBackProt+0,esi ;store protected mode code address.
        mov     w[ebx].CallBackStruc.CallBackProt+4,ax
        mov     d[ebx].CallBackStruc.CallBackRegs+0,edi ;store register table address.
        mov     w[ebx].CallBackStruc.CallBackRegs+4,es
        mov     ax,CallBackSize
        mul     dx
        mov     esi,offset CallBackList
        movzx   eax,ax
        add     esi,eax         ;index list of calls.
        mov     [ebx].CallBackStruc.CallBackOff,si      ;store call back address.
        mov     [ebx].CallBackStruc.CallBackFlags,1     ;flag this entry in use.
        mov     cx,_cwRaw       ;get real mode code seg.
        mov     dx,si           ;get real mode offset.
        clc
        jmp     inter12_10
        ;
inter12_9:
        stc
inter12_10:
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
        assume ds:_cwDPMIEMU
RAWGetCallBack endp


;-------------------------------------------------------------------------------
;
;Release a previously allocated real mode call back address.
;
;On Entry:-
;
;CX:DX  - Real mode call back address.
;
RAWRelCallBack proc near
        ;
        pushad
        push    ds
        push    es
        push    fs
        push    gs
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        ;
        mov     esi,offset CallBackTable
        mov     ebx,MaxCallBacks
inter13_0:
        test    [esi].CallBackStruc.CallBackFlags,1
        jz      inter13_1
        cmp     dx,[esi].CallBackStruc.CallBackOff
        jnz     inter13_1
        mov     [esi].CallBackStruc.CallBackFlags,0
        mov     bx,[esi].CallBackStruc.CallBackStackSel
        mov     ax,1
        int     31h
        clc
        jmp     inter13_2
        ;
inter13_1:
        add     esi,size CallBackStruc
        dec     ebx
        jnz     inter13_0
        stc
        ;
inter13_2:
        pop     gs
        pop     fs
        pop     es
        pop     ds
        popad
        ret
        assume ds:_cwDPMIEMU
RAWRelCallBack endp


;-------------------------------------------------------------------------------
;
;--- this is the generic handler for all IDT vectors
;Need to retrieve the interupt number.
;
IntHandler      proc    near
if INTCALLSIZE eq 4
        sub     esp, 2
endif
        push    ds
        push    eax
        mov     ax,DpmiEmuDS            ;make our data addresable.
        mov     ds,eax           ;/
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
_eip	dd ?
_cs		dd ?
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

        cmp     esp,tPL0StackSize-4-((4+4)+(4)+(4)+(4+4+4)+(4+4))
        ;                            |    |   |     |      |
        ; EAX:DS --------------------/    |   |     |      |
        ;                                 |   |     |      |
        ; Return address -----------------/   |     |      |
        ;                                     |     |      |
        ; Error code -------------------------/     |      |
        ;                                           |      |
        ; EIP:CS:Eflags ----------------------------/      |
        ;                                                  |
        ; ESP:SS ------------------------------------------/
        ;
        jnz     inter14_NoCode
        and     w[esp].INTHFRM._efl,0011111111010101b	;clear NT

; MED 12/02/95
; check if Exception Index is 0dh
;  if so and instruction at CS:EIP is:
;   mov eax,cr0 [0f 20 c0]  or
;   mov cr0,eax [0f 22 c0] or
;   mov eax,cr3 [0f 20 d8] or
;   mov cr3,eax [0f 22 d8]
;  then emulate it here and return
; MED 11/12/98, emulate RDMSR [0f 32]
; MED 04/05/99, emulate WBINVD [0f 09]
;   WRMSR [0f 30]
;   mov eax,cr4 [0f 20 e0]
;   mov cr4,eax [0f 22 e0]
; MED 05/02/2000, mov ebx,cr4 [0f 20 e3]
;   mov eax,cr2 [0f 20 d0]
        push    ds
        cmp     eax,0dh
        jne     mednoem                         ; not a GPF

;        mov     ax,ss:[esp+(4+4)+(4)+(4+4)+4]   ; ax==original CS
        mov     ds,[esp+4].INTHFRM._cs          ; ds==original CS
        mov     eax,[esp+4].INTHFRM._eip        ; eax==original EIP

        cmp     BYTE PTR [eax],0fh              ; first opcode byte
        jne     mednoem                         ; no match
        cmp     BYTE PTR [eax+1],20h            ; mov reg,crx
        jne     med6b
        cmp     BYTE PTR [eax+2],0c0h           ; mov eax,cr0?
        jne     med2b
.586
        mov     eax,cr0
        jmp     medemu3eax                      ; update original eax with cr0 value
med2b:
        cmp     BYTE PTR [eax+2],0d0h           ; mov eax,cr2?
        jne     med3b
        mov     eax,cr2
        jmp     medemu3eax                      ; update original eax with cr2 value
med3b:
        cmp     BYTE PTR [eax+2],0d8h           ; mov eax,cr3?
        jne     med4b
        mov     eax,cr3
        jmp     medemu3eax                      ; update original eax with cr3 value
med4b:
        cmp     BYTE PTR [eax+2],0e0h           ; mov eax,cr4?
        jne     med5b
        mov     eax,cr4
medemu3eax:
        mov     ss:[esp+4].INTHFRM._eax,eax     ; update original eax with cr4 value
        jmp     medemu3
med5b:
        cmp     BYTE PTR [eax+2],0e3h           ; mov ebx,cr4?
        jne     mednoem
        mov     ebx,cr4
        jmp     medemu3

med6b:
        cmp     BYTE PTR [eax+1],22h            ; mov crx,reg?
        jne     med9b                           ; no match
        cmp     BYTE PTR [eax+2],0c0h           ; move cr0,eax?
        jne     med7b                           ; no match
        mov     eax,[esp+4].INTHFRM._eax
;        or      eax, 80000001h
        mov     cr0,eax
        jmp     medemu3
med7b:
        cmp     BYTE PTR [eax+2],0d8h           ; move cr3,eax?
        jne     med8b                           ; no match
        mov     eax,[esp+4].INTHFRM._eax
        mov     cr3,eax
        jmp     medemu3
med8b:
        cmp     BYTE PTR [eax+2],0e0h           ; move cr4,eax?
        jne     mednoem                         ; no match
        mov     eax,[esp+4].INTHFRM._eax
        mov     cr4,eax
medemu3:
        mov     eax,3
        jmp     medemu

med9b:
        cmp     BYTE PTR [eax+1],9              ; WBINVD?
        jne     med10b
        wbinvd
        jmp     medemu2
med10b:
        cmp     BYTE PTR [eax+1],30h            ; WRMSR?
        jne     med11b
        mov     eax,[esp+4].INTHFRM._eax
        wrmsr
        jmp     medemu2
med11b:
        cmp     BYTE PTR [eax+1],32h            ; RDMSR?
        jne     mednoem
        rdmsr
        mov     [esp+4].INTHFRM._eax,eax

.386p

medemu2:
        mov     eax,2
medemu:
        add     [esp+4].INTHFRM._eip,eax        ; adjust EIP past emulated instruction
        pop     ds
        pop     eax
        pop     ds
        add     esp,8                           ; flush return address and error code off stack
        iretd

mednoem:
        pop     ds

        mov     eax,[esp].INTHFRM._errC         ;get error code.
        mov     ExceptionCode,eax               ;/
        mov     eax,[esp].INTHFRM._efl          ;Get flags.
        or      eax,65536
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
        and     w[esp].INTHFRM2._efl,0011111111010101b	; clear NT
        mov     eax,[esp].INTHFRM2._efl                 ;Get flags.
        and     eax,not 65536
        mov     ExceptionFlags,eax

        cmp ExceptionIndex,0            ; exc 00?
        jz inter14_ForceException

;--- force exception for int 6
        cmp ExceptionIndex,6            ; exc 06?
        jz inter14_ForceException

        cmp     ExceptionIndex,1        ;int 1
        jnz     inter14_SortedCode
inter14_ForceException:
        or      ExceptionFlags,65536    ;force an exception.

inter14_SortedCode:
        pop     eax
        pop     ds
inter14_SortedCode2:
        add     esp,4           ;skip return address.
        ;
        ;Check which stack we should switch back to.
        ;
        push    ds
        push    eax

INTHFRM3 struc
_eax	dd ?	;eax
_ds		dd ?	;ds
		IRET32PL <>
INTHFRM3 ends

        cmp     w[esp].INTHFRM3._ss,KernalSS
        jz      KernalStack             ;Already on system stack?
        mov     ax,DpmiEmuDS
        mov     ds,eax
        test    ExceptionFlags,65536    ;exception?
        jnz     KernalStack
        ;
        push    ebx
        mov     ebx,ExceptionIndex
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:_cwRaw
        cmp     b[ebx+Int2CallCheck],0  ;Hardware INT or int 1Ch, 23h, 24h?
        pop     ebx
        assume ds:_cwDPMIEMU
        jnz     KernalStack
        jmp     IntStack
IntHandler      endp


;-------------------------------------------------------------------------------
;
;It's a user stack and its not an exception or hardware interupt so switch back
;to the original stack via a system stack to allow re-entrancy if original
;stack needs to be fetched from disk.
;
IntStack        proc    near
        pop     eax
        pop     ds
        ;
        ;Get new stack address.
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:_cwRaw
        mov     ebx,RawStackPos ;get next stack address.
        sub     RawStackPos,RawStackDif
        mov     ax,KernalSS
        mov     ds,eax
        assume ds:nothing
        ;
        ;Put old details onto new stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      inter15_iUse32
        sub     ebx, 5*2
        mov     eax,[esp+(4+4+4)+(0*4)] ;EIP
        mov     [ebx+0*2],ax
        mov     eax,[esp+(4+4+4)+(1*4)] ;CS
        mov     [ebx+1*2],ax
        mov     eax,[esp+(4+4+4)+(2*4)] ;EFlags
        mov     [ebx+2*2],ax
        mov     eax,[esp+(4+4+4)+(3*4)] ;ESP
        mov     [ebx+3*2],ax
        mov     eax,[esp+(4+4+4)+(4*4)] ;SS
        mov     [ebx+4*2],ax
        jmp     inter15_iUse0
        ;
inter15_iUse32:
        sub     ebx,5*4
        mov     eax,[esp+(4+4+4)+(0*4)] ;EIP
        mov     [ebx+0*4],eax
        mov     eax,[esp+(4+4+4)+(1*4)] ;CS
        mov     [ebx+1*4],eax
        mov     eax,[esp+(4+4+4)+(2*4)] ;EFlags
        mov     [ebx+2*4],eax
        mov     eax,[esp+(4+4+4)+(3*4)] ;ESP
        mov     [ebx+3*4],eax
        mov     eax,[esp+(4+4+4)+(4*4)] ;SS
        mov     [ebx+4*4],eax
        ;
inter15_iUse0:
        ;Put new details into current stack.
        ;
        mov     eax,offset inter15_Int
        mov     [esp+(4+4+4)+(0*4)],eax ;EIP
        mov     ax,DpmiEmuCS
        mov     [esp+(4+4+4)+(1*4)],eax ;CS
        pushfd
        pop     eax
        mov     [esp+(4+4+4)+(2*4)],eax ;EFlags
        mov     [esp+(4+4+4)+(3*4)],ebx ;ESP
        mov     ax,KernalSS
        mov     [esp+(4+4+4)+(4*4)],eax ;SS
        pop     ds
        pop     ebx
        pop     eax
        assume ds:_cwDPMIEMU
        iretd
        ;
inter15_Int:
        ;Now switch back to original stack.
        ;
        assume ds:nothing
        push    cs:ExceptionIndex       ;need to know the INT number.
        push    eax
        push    ebx
        push    esi
        push    ds
        mov     eax,ss
        mov     ds,eax
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      inter15_i2Use32
        movzx   ebx,sp
        lss     sp,[ebx+(4+4+4+4+4)+(2+2+2)] ;get original stack again.
        push    w[ebx+(4+4+4+4+4)+(2+2)]     ;get flags.
        push    w[ebx+(4+4+4+4+4)+(2)]       ;get CS
        push    w[ebx+(4+4+4+4+4)+(0)]       ;get IP
        xor     eax,eax
        mov     ax,[ebx+(4+4+4+4+4)+(2+2)]   ;get flags again.
        jmp     inter15_i2Use0
        ;
inter15_i2Use32:
        mov     ebx,esp
        lss     esp,[ebx+(4+4+4+4+4)+(4+4+4)];get original stack again.
        push    d[ebx+(4+4+4+4+4)+(4+4)]     ;get flags.
        push    d[ebx+(4+4+4+4+4)+(4)]       ;get CS
        push    d[ebx+(4+4+4+4+4)+(0)]       ;get IP
        mov     eax,[ebx+(4+4+4+4+4)+(4+4)]  ;get flags again.
inter15_i2Use0:
        and     ah,11111100b                 ;lose IF & TF
        push    eax                          ;int handler flags.
        ;
        mov     eax,[ebx+(4+4+4+4)]     ;get INT index.
        shl     eax,1
        mov     esi,eax
        shl     eax,1
        add     esi,eax         ;*6 for index into table.
        add     esi,offset InterruptTable
        movzx   eax,word ptr cs:[esi+4] ;int handler CS
        push    eax
        push    d cs:[esi+0]    ;int handler EIP.
        mov     eax,[ebx+(4+4+4)]  ;EAX
        push    d[ebx+(4+4)]    ;EBX
        mov     esi,[ebx+(4)]   ;ESI
        push    d[ebx+(0)]      ;DS
        mov     bx,KernalDS
        mov     ds,ebx
        assume ds:_cwRaw
        add     RawStackPos,RawStackDif
        assume ds:_cwDPMIEMU
        pop     ds
        pop     ebx
        iretd                   ;pass control to INT handler.
IntStack        endp


;-------------------------------------------------------------------------------
;
;Either we were already on the system stack or we have an exception or hardware
;interupt on our hands. Either way we need to switch to another piece of the
;system stack to make sure we're using a legitimate one.
;
KernalStack     proc    near
        pop     eax
        pop     ds
        ;
        ;Get new stack address.
        ;
        push    eax
        push    ebx
        push    ds
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:_cwRaw
        mov     ebx,RawStackPos ;get next stack address.
        sub     RawStackPos,RawStackDif
        mov     ax,KernalSS
        mov     ds,eax
        assume ds:nothing
        ;
inter16_Update:
        ;Put old details onto new stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      inter16_Use32
        mov     eax,[esp+(4+4+4)+(4+4+4+4)]
        sub     ebx,2
        mov     [ebx],ax                ;SS
        mov     eax,[esp+(4+4+4)+(4+4+4)]
        sub     ebx,2
        mov     [ebx],ax                ;ESP
        mov     eax,[esp+(4+4+4)+(4+4)]
        sub     ebx,2
        mov     [ebx],ax                ;EFlags
        mov     eax,[esp+(4+4+4)+(4)]
        sub     ebx,2
        mov     [ebx],ax                ;CS
        mov     eax,[esp+(4+4+4)+(0)]
        sub     ebx,2
        mov     [ebx],ax                ;EIP
        jmp     inter16_Use0
        ;
inter16_Use32:
        mov     eax,[esp+(4+4+4)+(4+4+4+4)]
        sub     ebx,4
        mov     [ebx],eax               ;SS
        mov     eax,[esp+(4+4+4)+(4+4+4)]
        sub     ebx,4
        mov     [ebx],eax               ;ESP
        mov     eax,[esp+(4+4+4)+(4+4)]
        sub     ebx,4
        mov     [ebx],eax               ;EFlags
        mov     eax,[esp+(4+4+4)+(4)]
        sub     ebx,4
        mov     [ebx],eax               ;CS
        mov     eax,[esp+(4+4+4)+(0)]
        sub     ebx,4
        mov     [ebx],eax               ;EIP
        ;
inter16_Use0:
        ;Put new details into current stack.
        ;
        mov     eax,offset IntDispatch
        mov     [esp+(4+4+4)+(0)],eax   ;EIP
        xor     eax,eax
        mov     ax,DpmiEmuCS
        mov     [esp+(4+4+4)+(4)],eax   ;CS
        pushfd
        pop     eax
        mov     [esp+(4+4+4)+(4+4)],eax ;EFlags
        mov     [esp+(4+4+4)+(4+4+4)],ebx       ;ESP
        xor     eax,eax
        mov     ax,KernalSS
        mov     [esp+(4+4+4)+(4+4+4+4)],eax ;SS
        pop     ds
        pop     ebx
        pop     eax
        assume ds:_cwDPMIEMU
        ;
        ;Pass control to interupt dispatcher.
        ;
        iretd
KernalStack     endp


;-------------------------------------------------------------------------------
;
;Direct control to exception or interupt handler.
;
IntDispatch     proc    near
        push    eax
        push    esi
        push    edi
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        and     w[ExceptionFlags],1111110011111111b   ; clear IF & TF
        mov     esi,ExceptionIndex      ;Get the exception number.
        add     esi,esi         ;*2
        mov     eax,esi
        add     esi,esi         ;*4
        add     esi,eax         ;*6
        test    ExceptionFlags,65536
        jnz     inter17_Excep
        ;
        ;Dispatch normal interrupt.
        ;
        add     esi,offset InterruptTable
        test    BYTE PTR DpmiEmuSystemFlags,1
        jz      inter17_Use32Bit14
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        mov     eax,offset inter17_Resume
        mov     w[inter17_Call20],ax
        mov     w[inter17_Call20+2],cs
        pop     ds
        pop     edi
        pop     esi
        pop     eax
        assume ds:nothing
        push    WORD PTR cs:[ExceptionFlags]    ;EFlags before entry.
        push    WORD PTR cs:[inter17_Call20+2]  ;CS
        push    WORD PTR cs:[inter17_Call20]    ;EIP
        jmp     FWORD PTR cs:[inter17_Call2]
        assume ds:_cwDPMIEMU
        ;
inter17_Use32Bit14:
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        mov     d[inter17_Call20+0],offset inter17_Resume
        mov     w[inter17_Call20+4],cs
        pop     ds
        pop     edi
        pop     esi
        pop     eax
        assume ds:nothing
        push    DWORD PTR cs:[ExceptionFlags]   ;EFlags before entry.
        push    DWORD PTR cs:[inter17_Call20+4] ;CS
        push    DWORD PTR cs:[inter17_Call20+0] ;EIP
        jmp     FWORD PTR cs:[inter17_Call2]
        assume ds:_cwDPMIEMU
        ;

;; MED 08/13/96
;       nop             ; make debugger EIP adjustment from debug int benign?

inter17_Resume:
        ;Return from normal int.
        ;
        pushfd
        cli                     ;stop interupts interfering.
        push    eax
        push    ebx
        push    ds
        mov     eax,ss
        mov     ds,eax
        mov     ebx,esp
        assume ds:nothing
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        assume ds:_cwDPMIEMU
        jz      inter17_Use32
        ;
        movzx   ebx,bx
        lss     sp,d[ebx+(4+4+4+4)+(2+2+2)] ;get old stack address.
        and     w[ebx+(4+4+4+4)+(2+2)],0000011100000000b ;retain IF & TF & DF.
        and     w[ebx+(4+4+4)],1111100011111111b        ;lose IF & TF & DF.
        mov     ax,[ebx+(4+4+4)]
        or      ax,[ebx+(4+4+4+4)+(2+2)]
        push    ax              ;EFlags.
        mov     ax,[ebx+(4+4+4+4)+(2)]
        push    ax              ;CS
        mov     ax,[ebx+(4+4+4+4)+(0)]
        push    ax              ;EIP
        mov     eax,[ebx+(4+4)]
        push    eax             ;EAX
        mov     eax,[ebx+(4)]
        push    eax             ;EBX
        mov     eax,[ebx+(0)]
        push    eax             ;DS
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        add     RawStackPos,RawStackDif ;update next stack.
        assume ds:_cwDPMIEMU
        pop     ds
        pop     ebx
        pop     eax
;        db 66h
        iret
        ;
inter17_Use32:
        lss     esp,[ebx+(4+4+4+4)+(4+4+4)] ;get old stack address.
        and     w[ebx+(4+4+4+4)+(4+4)],0000011100000000b ;retain IF & TF & DF.
        and     w[ebx+(4+4+4)],1111100011111111b        ;lose IF & TF & DF.
        mov     eax,[ebx+(4+4+4)]
        or      eax,[ebx+(4+4+4+4)+(4+4)]
        push    eax             ;EFlags.
        mov     eax,[ebx+(4+4+4+4)+(4)]
        push    eax             ;CS
        mov     eax,[ebx+(4+4+4+4)+(0)]
        push    eax             ;EIP
        mov     eax,[ebx+(4+4)]
        push    eax             ;EAX
        mov     eax,[ebx+(4)]
        push    eax             ;EBX
        mov     eax,[ebx+(0)]
        push    eax             ;DS
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        add     RawStackPos,RawStackDif ;update next stack.
        assume ds:_cwDPMIEMU
        pop     ds
        pop     ebx
        pop     eax
        iretd

        ;
inter17_Excep:
        ;Dispatch exception.
        ;
        add     esi,offset ExceptionTable
        test    BYTE PTR DpmiEmuSystemFlags,1
        jz      inter17_eUse32Bit14
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        mov     eax,offset inter17_ResumeExp
        mov     w[inter17_Call20+0],ax
        mov     w[inter17_Call20+2],cs
        pop     ds
        pop     edi
        pop     esi
        pop     eax
        assume ds:nothing
        push    WORD PTR cs:[ExceptionCode]     ;EFlags before entry.
        push    WORD PTR cs:[inter17_Call20+2]  ;CS
        push    WORD PTR cs:[inter17_Call20+0]  ;EIP
        jmp     FWORD PTR cs:[inter17_Call2]
        assume ds:_cwDPMIEMU
        ;
inter17_eUse32Bit14:
        mov     eax,[esi+0]
        mov     d[inter17_Call2+0],eax
        mov     ax,[esi+4]
        mov     w[inter17_Call2+4],ax
        mov     d[inter17_Call20+0],offset inter17_ResumeExp
        mov     w[inter17_Call20+4],cs
        pop     ds
        pop     edi
        pop     esi
        pop     eax
        assume ds:nothing
        push    DWORD PTR cs:[ExceptionCode]    ;EFlags before entry.
        push    DWORD PTR cs:[inter17_Call20+4] ;CS
        push    DWORD PTR cs:[inter17_Call20+0] ;EIP
        jmp     FWORD PTR cs:[inter17_Call2]
        assume ds:_cwDPMIEMU
        ;
inter17_ResumeExp:
        ;Return from exception.
        ;
        cli
        assume ds:nothing
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        assume ds:_cwDPMIEMU
        jz      inter17_ExpUse32
        ;
        add     sp,2
        pushfd
        push    eax
        push    ebx
        push    ds
        mov     eax,ss
        mov     ds,eax
        mov     ebx,esp
        movzx   ebx,bx
        lss     sp,d[ebx+(4+4+4+4)+(2+2+2)] ;get old stack address.
        mov     ax,[ebx+(4+4+4+4)+(2+2)]
        push    ax              ;EFlags.
        mov     ax,[ebx+(4+4+4+4)+(2)]
        push    ax              ;CS
        mov     ax,[ebx+(4+4+4+4)+(0)]
        push    ax              ;EIP
        mov     eax,[ebx+(4+4)]
        push    eax             ;EAX
        mov     eax,[ebx+(4)]
        push    eax             ;EBX
        mov     eax,[ebx+(0)]
        push    eax             ;DS
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        add     RawStackPos,RawStackDif ;update next stack.
        assume ds:_cwDPMIEMU
        pop     ds
        pop     ebx
        pop     eax
;        db 66h
        iret
        ;
inter17_ExpUse32:
        add     esp,4
        pushfd
        push    eax
        push    ebx
        push    ds
        mov     eax,ss
        mov     ds,eax
        mov     ebx,esp
        lss     esp,[ebx+(4+4+4+4)+(4+4+4)] ;get old stack address.
        mov     eax,[ebx+(4+4+4+4)+(4+4)]
        push    eax             ;EFlags.
        mov     eax,[ebx+(4+4+4+4)+(4)]
        push    eax             ;CS
        mov     eax,[ebx+(4+4+4+4)+(0)]
        push    eax             ;EIP
        mov     eax,[ebx+(4+4)]
        push    eax             ;EAX
        mov     eax,[ebx+(4)]
        push    eax             ;EBX
        mov     eax,[ebx+(0)]
        push    eax             ;DS
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        add     RawStackPos,RawStackDif ;update next stack.
        assume ds:_cwDPMIEMU
        pop     ds
        pop     ebx
        pop     eax
        iretd
        ;
	align 4
inter17_Call2   df 0
inter17_Call20  df 0
                dw 0  ; ensure that hiword(dword ptr [Inter17_call20+4]) is 0000
IntDispatch     endp


;-------------------------------------------------------------------------------
;
;Handle an INT nn instruction by retrieving registers from the stack and
;reflect to real mode.
;
;frame:
INTNN386FR struc
	dd 8 dup (?)	;pushad
    dw 10 dup (?)	;10*2
    dd 4 dup (?)    ;push ds - push gs
newfl dd ?          ;sub esp,4+4
xxx   dd ?
retaddr dd ?        ;return address
INTNN386FR ends

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

IntNN386        proc    far
if INTNN386SIZE eq 4
        sub     esp,4+4+2
else
        sub     esp,4+4
endif
        push    ds
        push    es
        push    fs
        push    gs
        sub     esp,10*2
        pushad
        ;
        mov     ax,DpmiEmuDS            ;make our data addresable.
        mov     ds,eax           ;/
        mov     ebp,esp
        test    BYTE PTR DpmiEmuSystemFlags,1
        jz      inter18_Use32Bit19
        movzx   ebp,bp
        movzx   eax,[ebp+sizeof INTNN386FR].IRET16._fl
        jmp     inter18_Use16Bit19
inter18_Use32Bit19:
        mov     eax,[ebp+sizeof INTNN386FR].IRET32._fl
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
        mov     [ebp].INTNN386FR.xxx,0
        ;
        mov     edi,ebp
        push    ss
        pop     es
        ;
        ;If this is a "hardware" interrupt we should mark it as
        ;busy if it isn't already marked.
        ;
        mov     ebx,edx
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        mov     al,[ebx+Int2CallCheck]
        or      al,al           ;Hardware?
        jz      inter18_c2
        sub     bl,al
        imul    ebx, sizeof CallBackStruc	;is 24
        add     ebx,offset CallBackTable
        test    CallBackStruc.CallBackFlags[ebx],1
        jz      inter18_c2
        test    CallBackStruc.CallBackFlags[ebx],128    ;already busy?
        jnz     inter18_c2
        or      CallBackStruc.CallBackFlags[ebx],128    ;mark it as busy.
        mov     [ebp].INTNN386FR.xxx,ebx
        assume ds:_cwDPMIEMU
inter18_c2:
        pop     ds
        ;
        ;Now pass control to the INT simulator.
        ;
        mov     eax,[ebp].INTNN386FR.newfl
        and     ah,11111000b   ; clear TF, IF & DF
        push    eax
        popfd
        mov     ebx,edx
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        call    EmuRawSimulateInt
        ;
        cmp     [ebp].INTNN386FR.xxx,0
        jz      inter18_NoCall
        push    ds
        mov     esi,[ebp].INTNN386FR.xxx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:_cwRaw
        and     CallBackStruc.CallBackFlags[esi],255-128
        assume ds:_cwDPMIEMU
        pop     ds
inter18_NoCall:
        ;
        ;Update the flags.
        ;
        mov     ax,es:RealRegsStruc.Real_Flags[edi]
        and     ax,1000100011111111b            ;lose TF, IF, DF, IOPL, NT
        ;
        test    BYTE PTR DpmiEmuSystemFlags,1
        jz      inter18_Use32Bit
        mov     bx,[ebp+sizeof INTNN386FR].IRET16._fl
        and     bx,0111011100000000b            ;retain TF, IF, DF, IOPL, NT
        or      ax,bx
        mov     [ebp+sizeof INTNN386FR].IRET16._fl,ax
        popad
        add     sp,10*2
        pop     gs
        pop     fs
        pop     es
        pop     ds
        add     sp,4+4+4
        iret                    ;Switch back to calling program.
        ;
inter18_Use32Bit:
        mov     bx,w[ebp+sizeof INTNN386FR].IRET32._fl
        and     bx,0111011100000000b            ;retain TF, IF, DF, IOPL, NT
        or      ax,bx
        mov     w[ebp+sizeof INTNN386FR].IRET32._fl,ax
        popad
        add     esp,10*2
        pop     gs
        pop     fs
        pop     es
        pop     ds
        add     esp,4+4+4
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
        mov     ax,DpmiEmuDS            ;make our data addresable.
        mov     ds,eax           ;/
        test    BYTE PTR DpmiEmuSystemFlags,1
        jz      inter19_Use32Bit16
        movzx   eax,w[esp+(4+4+4+4)+(4+4+4+4+4+4+4)+(2+2)]
        mov     ExceptionEFL,eax
        jmp     inter19_Use16Bit16
inter19_Use32Bit16:
        mov     eax,[esp+(4+4+4+4)+(4+4+4+4+4+4+4)+(4+4)]
        mov     ExceptionEFL,eax
        ;
inter19_Use16Bit16:
        ;Retrieve register values and get outa here.
        ;
        mov     ax,DpmiEmuDS            ;make sure our data is addresable.
        mov     es,eax           ;/
        assume es:_cwDPMIEMU
        mov     eax,ss
        mov     ds,eax
        assume ds:nothing
        mov     esi,esp
        mov     edi,offset ExceptionRegs
        mov     ecx,sizeof EXCFRX
        cld
        rep     movsb [edi],[esi]       ;copy registers off the stack.
        ;
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      inter19_Use32Bit17
        xor     eax, eax
        add     esi,2+2+2               ;skip return address/flags.
        lodsw   [esi]                  ;copy IP, CS, FL, SP, SS
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
        movsd   [edi],[esi]            ;copy EIP, CS, EFL, ESP, SS
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
        movsd   [edi],[esi]
inter19_Use16Bit678:
        push    es
        pop     ds
        assume es:nothing
        assume ds:_cwDPMIEMU
;
;Now switch to PL0 to get CR0-3 values.
;
        mov     eax, offset inter19_pl0
        call    EmuCallPL0Proc
        jmp     @F
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
@@:
;
;Convert register values into ASCII ready for printing.
;
        mov     eax,ExceptionIndex
        mov     ecx,2
        mov     edi,offset ExceptionINum
        call    Bin2HexE
        mov     eax,ExceptionCode       ;high word undefined upto 486.
        mov     ecx,4
        mov     edi,offset ExceptionENum
        call    Bin2HexE
        ;
        movzx   eax,ExceptionTR
        mov     ecx,4
        mov     edi,offset ExceptionTRt
        call    Bin2HexE
        mov     eax,ExceptionCR0
        mov     ecx,8
        mov     edi,offset ExceptionCR0t
        call    Bin2HexE
        mov     eax,ExceptionCR2
        mov     ecx,8
        mov     edi,offset ExceptionCR2t
        call    Bin2HexE
        mov     eax,ExceptionCR3
        mov     ecx,8
        mov     edi,offset ExceptionCR3t
        call    Bin2HexE
        ;
        mov     eax,ExceptionRegs._eax
        mov     ecx,8
        mov     edi,offset ExceptionEAXt
        call    Bin2HexE
        mov     eax,ExceptionRegs._ebx
        mov     ecx,8
        mov     edi,offset ExceptionEBXt
        call    Bin2HexE
        mov     eax,ExceptionRegs._ecx
        mov     ecx,8
        mov     edi,offset ExceptionECXt
        call    Bin2HexE
        mov     eax,ExceptionRegs._edx
        mov     ecx,8
        mov     edi,offset ExceptionEDXt
        call    Bin2HexE
        mov     eax,ExceptionRegs._esi
        mov     ecx,8
        mov     edi,offset ExceptionESIt
        call    Bin2HexE
        mov     eax,ExceptionRegs._edi
        mov     ecx,8
        mov     edi,offset ExceptionEDIt
        call    Bin2HexE
        mov     eax,ExceptionRegs._ebp
        mov     ecx,8
        mov     edi,offset ExceptionEBPt
        call    Bin2HexE
        mov     eax,ExceptionESP
        mov     ecx,8
        mov     edi,offset ExceptionESPt
        call    Bin2HexE
        mov     eax,ExceptionEIP
        mov     ecx,8
        mov     edi,offset ExceptionEIPt
        call    Bin2HexE
        mov     eax,ExceptionEFL
        mov     ecx,8
        mov     edi,offset ExceptionEFLt
        call    Bin2HexE
        ;
        movzx   eax,ExceptionCS
        mov     ecx,4
        mov     edi,offset ExceptionCSt
        call    Bin2HexE
        movzx   eax,ExceptionRegs._ds
        mov     ecx,4
        mov     edi,offset ExceptionDSt
        call    Bin2HexE
        movzx   eax,ExceptionRegs._es
        mov     ecx,4
        mov     edi,offset ExceptionESt
        call    Bin2HexE
        movzx   eax,ExceptionRegs._fs
        mov     ecx,4
        mov     edi,offset ExceptionFSt
        call    Bin2HexE
        movzx   eax,ExceptionRegs._gs
        mov     ecx,4
        mov     edi,offset ExceptionGSt
        call    Bin2HexE
        movzx   eax,ExceptionSS
        mov     ecx,4
        mov     edi,offset ExceptionSSt
        call    Bin2HexE
        mov     edi,offset ExceptionSysFlags
        mov     eax,DpmiEmuSystemFlags
        mov     ecx,8
        call    Bin2HexE
;
;Copy register details into the transfer buffer.
;
        push    ds
        mov     ax,MainDS
        mov     ds,eax
        assume ds:_cwMain
        movzx   edi,TransferReal
        assume ds:_cwDPMIEMU
        pop     ds
        shl     edi,4
        mov     esi,offset ExceptionHeader
        mov     ecx,offset ExceptionHeaderEnd-ExceptionHeader
        mov     ax,KernalZero
        mov     es,eax
        rep     movsb
        push    ds
        pop     es
;
;Print the message.
;
        push    ds
        mov     ax,MainDS
        mov     ds,eax
        assume ds:_cwMain
        mov     ax,TransferReal
        assume ds:_cwDPMIEMU
        pop     ds
        mov     edi,offset ExceptionIntBuffer
        mov     RealRegsStruc.Real_DS[edi],ax
        mov     RealRegsStruc.Real_EDX[edi],0
        mov     RealRegsStruc.Real_EAX[edi],0900h
        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
;
;Now switch back to exit code.
;
        mov     ax,InitDS
        mov     ds,eax
        assume ds:_cwInit
        mov     WORD PTR IErrorNumber,0
        mov     ax,InitCS
        push    ax
        mov     ax,offset InitError
        push    ax
        retw
        assume ds:_cwDPMIEMU
ExcepNN386      endp
