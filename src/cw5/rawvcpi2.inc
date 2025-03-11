
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Second level section for RAW/VCPI stuff. This holds the higher level management
;code like interrupt/exception handlers, memory management and DPMI emulation.
;This gets moved into extended memory during initialisation to keep conventional
;memory foot print down.
;

DbgStruc struc
_Address     dd ?
_Size        db ?
_Type        db ?
_Flags       db ?   ; bit 7: 1=watchpoint in use
             db ?
DbgStruc ends

_cwDPMIEMU$1 segment
rv51_Total  dd ?
DbgTable    DbgStruc 4 dup (<0,0,0,0>)
DbgSize2Dr  db 0,0,1,0,3   ;bits 2+3 for DR7
DbgType2Dr  db 0,1,3       ;bits 0+1 for DR7
if 0
;--- vector to jump to previous int 31h handler
;--- the problem is: this is raw/vcpi mode, there is no "previous" 
;--- handler for int 31h, it's routed to real-mode!

DefaultInt31 label fword
        dd offset IntNN386Catch+(31h*INTNN386SIZE)
        dw DpmiEmuCS
endif
_cwDPMIEMU$1 ends

;-------------------------------------------------------------------------------
;
;Simulate real mode interupt.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;BL     - Interrupt number.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateInt proc     near
        push    cx
        push    bx
        mov     bh,0
        mov     cx,0
        jmp     RAWSimulate
RawSimulateInt  endp


;-------------------------------------------------------------------------------
;
;Simulate real mode interupt.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;BL     - Interupt number.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateInt2 proc    near
        push    cx
        push    bx
        mov     bh,0
        jmp     RAWSimulate
RawSimulateInt2 endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCall proc near
        push    cx
        push    bx
        mov     bh,1
        mov     cx,0
        jmp     RAWSimulate
RawSimulateFCall endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCall2 proc near
        push    cx
        push    bx
        mov     bh,1
        jmp     RAWSimulate
RawSimulateFCall2 endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call with IRET stack frame.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCallI proc near
        push    cx
        push    bx
        mov     bh,2
        jmp     RAWSimulate
RawSimulateFCallI endp


;-------------------------------------------------------------------------------
;
; Simulate either a real mode INT or far call.
; BH: 0=INT, 1=far call, 2=far call with IRET frame
; BL: int# if BH==0
; CX: word params to copy to real-mode stack
; ES:EDI=RMCS
; SS:EBP=word params src if CX != 0 
; this "proc" is never called, only jumped to, with
; registers BX & CX on stack.

; A CauseWay peculiarity is that the DPMI emulator works
; on the application stack. That's also true for the real-mode
; code support functions 0x300-0x302. To be safe, assume 128
; bytes below (E)SP to be used by the emulator.

;--- stack frame

RV29FR struct
    dw ?    ; rv29_ ...
    dw ?
    dd ?
    dd ?
    dd ?,?,?,?	;seg regs
    dd ?,?,?,?,?,?,?,?	;pushad
wFlgs dd ?
wBX dw ?   ;not used
wCX dw ?   ;not used
RV29FR ends

RAWSimulate     proc    near
        pushfd                  ;Preserve IF state.
        cli                     ;Stop INTs interfering.
        cld                     ;make sure direction is right.
        pushad
        push    ds
        push    es
        push    fs
        push    gs

        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     fs,eax
        push    [rv29_tVCPI_SP]
        push    [rv29_IntAdd]
        push    [rv29_CallAdd]
        push    [rv29_ourstack]
        mov     [rv29_ourstack],0
;
;setup the real mode stack.
;
        mov     eax,es:RealRegsStruc.Real_SSSP[edi]     ;check if stack is being
        shld    esi,eax,16
        or      eax, eax
        jnz     rv29_GotStack
;
;Caller isn't supplying a stack so we will.
;
        mov     eax,RawStackPos
        sub     RawStackPos,RawStackDif ;update for re-entry.
        mov     si,RawStackReal
        or      [rv29_ourstack],-1
;
;Point to the real mode stack.
;
rv29_GotStack:
        movzx   eax,ax
        movzx   esi,si
        sub     eax,(4+4)+(4+4)		; make room for pm ss:esp and rmcs es:edi
        mov     w[rv29_tVCPI_SP+0],ax
        mov     w[rv29_tVCPI_SP+2],si
        mov     edx,esi             ; save rm SP in DX
        shl     esi,4
        add     esi,eax             ; ESI = linear address rm SS:SP
;
;Store current stack pointer on v86 stack.
;
        mov     eax,esp
        test    BYTE PTR SystemFlags,1
        jz      rv29_noextendstack
        movzx   eax,ax
rv29_noextendstack:
        mov     fs:[esi+0],eax
        mov     fs:[esi+4],ss 
;
;Store table address on v86 stack.
;
        mov     fs:[esi+8],edi
        mov     fs:[esi+12],es
;
;Copy stacked parameters.
;
        or      cx,cx
        jz      rv29_NoStacked
        push    eax
        movzx   eax,cx
        shl     eax,1
        add     ebp,eax
rv29_copystack0:
        sub     ebp,2
        sub     esi,2
        mov     ax,[ebp]
        mov     fs:[esi],ax
        loopw   rv29_copystack0
        pop     eax
;
;Put flags onto the real mode stack.
;
rv29_NoStacked:
        mov     ebp,eax
        mov     eax,[ebp].RV29FR.wFlgs
        or      bh,bh           ;int or far?
        jnz     rv29_NoIF
        and     ah,11111100b    ;clear TF and IF
rv29_NoIF:
        sub     esi,2
        mov     fs:[esi],ax
        ;
        ;See if the CS:IP is supplied or needs fetching.
        ;
        or      bh,bh
        jz      rv29_IsInt
        ;
        mov     ecx,es:RealRegsStruc.Real_CSIP[edi]
        mov     [rv29_CallAdd],offset rv29_fcall
        cmp     bh,2
        jnz     rv29_NotInt
        mov     [rv29_CallAdd],offset rv29_fcalli
        jmp     rv29_NotInt
        ;
rv29_IsInt:
        ;See if this is a busy interrupt call back.
        ;
        xor     bh,bh
        mov     bp,bx
        shl     bp,2
        mov     al,[Int2CallCheck+bx]
        or      al,al
        jz      rv29_c3
        sub     bl,al           ;gives callback# in bx
        shl     bx,3            ;*8
        mov     ax,bx
        shl     bx,1            ;*16
        add     bx,ax           ;*24 (=size CallBackStruc)
        add     bx,offset CallBackTable
        test    CallBackStruc.CallBackFlags[bx],80h     ;this entry in use?
        jz      rv29_c3
        mov     ecx,CallBackStruc.CallBackReal[bx]
        jmp     rv29_c2
        ;
rv29_c3:
        ;Get interupt address to put on stack.
        ;
        mov     ecx,DWORD PTR fs:[bp]
        ;
rv29_c2:
        mov     [rv29_CallAdd],offset rv29_int
        ;
rv29_NotInt:
        sub     esi,4
        mov     fs:[esi],ecx
;
;Copy register values onto real mode stack (es:edi -> fs:esi)
;8*4 = PUSHAD
;2   = Flags
;4*2 = seg regs DS,ES,FS,GS
;2   = align to dword

        sub     esi,8*4+6*2 ;extra +2 for movsD
        push    esi
        push    ds

        xchg    esi,edi
        push    es
        push    fs
        pop     es
        pop     ds
        mov     ecx,(8*4+6*2)/4
        rep     movsd [edi],[esi]

        pop     ds
        pop     esi
;
;Get ss:sp values into DX:CX
;
        mov     ecx,edx
        mov     eax,ecx
        shl     eax,4
        mov     edx,esi
        sub     edx,eax
;
;Switch back to real/v86 mode.
;
        push    offset rv29_contrm
        jmp     [fProtected2Real]

_cwRaw segment
rv29_contrm:
;
;Fetch registers off the stack.
;
        assume ds:nothing
        popad
        pop     WORD PTR cs:[rv29_IntAdd]       ;lose dummy.
        pop     es
        pop     ds
        pop     fs
        pop     gs
        pop     WORD PTR cs:[rv29_IntAdd]       ;lose dummy.
        ;
        pop     cs:[rv29_IntAdd]
        jmp     cs:[rv29_CallAdd]
        ;
rv29_fcall:
        popf
rv29_fcalli:
rv29_int:
        call    cs:[rv29_IntAdd]
        pushf
        cli
        cld
        pop     WORD PTR cs:[rv29_IntAdd]
        and     WORD PTR cs:[rv29_IntAdd],0000110011010101b
;
;Switch back to old stack.
;
;        mov     ss,WORD PTR cs:[rv29_tVCPI_SP+2]
;        mov     sp,WORD PTR cs:[rv29_tVCPI_SP+0]
         lss     sp,cs:[rv29_tVCPI_SP]
;
;Save all registers.
;
;        push    WORD PTR cs:[rv29_IntAdd]       ;save dummy.
        push    gs
        push    fs
        push    ds
        push    es
        push    WORD PTR cs:[rv29_IntAdd]       ;save flags.
        pushad
;
;Make our data addresable again and store stack values.
;
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16
        mov     w[rv29_tVCPI_SP+0],sp
        mov     w[rv29_tVCPI_SP+2],ss
        mov     bp,sp
;
;Retrieve protected mode stack address.
;8*4=pushad, 5*2=push segeregs + flags, 2=dummy
;
        mov     edx,d[bp+8*4+5*2+0]
        mov     cx, w[bp+8*4+5*2+4]
;
;switch back to protected mode.
;
        push    DpmiEmuCS
        push    lowword offset rv29_contpm
        jmp     [Real2Protected]
;
_cwRaw ends

rv29_contpm:
        mov     ax,KernalZero   ;/
        mov     fs,eax
;
;Retreive v86 stack address.
;
        movzx   esi,w[rv29_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv29_tVCPI_SP+0]
        add     esi,eax
;
;Retrieve table address.
;
        les     edi,fs:[esi+8*4+5*2+2*4]
;
;Copy new register values into table.
;

        push    edi
        mov     ecx,(8*4+4*2)/4      ;copy just the 8 regs and 4 words
        rep     movsd [edi],fs:[esi]
        movsw   [edi],fs:[esi]       ;another word (register GS)
        pop     edi

        mov     ebx,[esp].RV29FR.wFlgs
        and     bx,1111001100101010b
        or      es:[edi].RealRegsStruc.Real_Flags,bx
        ;
        cmp     [rv29_ourstack],0
        jz      rv29_nostackadjust
        add     RawStackPos,RawStackDif ;update for re-entry.
rv29_nostackadjust:
        pop     [rv29_ourstack]
        pop     [rv29_CallAdd]
        pop     [rv29_IntAdd]
        pop     [rv29_tVCPI_SP]
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        popad
        popfd
        clc
        ;
        pop     bx
        pop     cx
        ret
        ;

RAWSimulate     endp

;-------------------------------------------------------------------------------

_cwRaw segment

RCBFRM struct
_ds		dw ?
_bp		dw ?
_ax		dw ?
_fl		dw ?
_ret	dw ?
RCBFRM ends

RawCallBack     proc    near
        pushf
        cli
        ;
        ;Check if Windows enhanced mode has been started.
        ;
        cmp     cs:InWindows,0
        jz      rv30_Normal
        popf
        add     sp,2    ; skip near call return address
        retf
        ;
rv30_Normal:
        push    ax
        push    bp
        push    ds
        mov     ax,cs
        mov     ds,ax
        assume ds:GROUP16
        mov     bp,sp
        mov     ax,[bp].RCBFRM._ret ;get return address
        mov     rv3x_Return,ax
        mov     ax,[bp].RCBFRM._fl  ;get flags
        mov     [bp].RCBFRM._ret,ax ;ovewrite return address.
        add     bp,sizeof RCBFRM    ;correct for stacked registers.
        mov     rv30_StackAdd+0,bp
        mov     rv30_StackAdd+2,ss
        pop     ds
        pop     bp
        pop     ax
        add     sp,2            ;remove local return address.
        ;
        push    gs
        push    fs
        push    ds
        push    es
        pushad
        mov     ax,cs
        mov     ds,ax           ;make our data addresable.
        mov     [VCPI_SP+0],sp
        mov     [VCPI_SP+2],ss
        ;
        ;switch to protected mode.
        ;
        mov     cx,KernalSS
        mov     edx,RawStackPos
        sub     RawStackPos,RawStackDif
        push    DpmiEmuCS
        push    lowword offset rv30_contpm
        jmp     [Real2Protected]
_cwRaw ends
        ;
rv30_contpm:    
        movzx   eax,rv3x_Return         ;get return address.
        sub     eax,CallBackSize        ;back to start of call back entry.
        sub     eax,offset CallBackList ;offset from start of list.
        xor     edx,edx
        mov     ebx,CallBackSize
        div     ebx                     ;entry number.
        mov     ebx,size CallBackStruc
        mul     ebx                     ;get offset into table.
        mov     ebx,offset CallBackTable
        add     ebx,eax                 ;point to this entry.

        movzx   esi,[VCPI_SP+2] ;point to stacked registers.
        shl     esi,4
        movzx   eax,[VCPI_SP+0]
        add     esi,eax
        mov     ax,KernalZero   ;/
        mov     fs,eax
        ;
        les     edi,[ebx].CallBackStruc.CallBackRegs  ;get register structure.
        mov     edx,edi
        mov     ecx,8
        cld
        rep     movsd es:[edi],fs:[esi]
        mov     ax,fs:[esi+2*4]
        stosw   es:[edi]            ;flags
        movsd   es:[edi],fs:[esi]   ;ES,DS
        movsd   es:[edi],fs:[esi]   ;FS,GS

        mov     ax,rv3x_Return
        stosw   es:[edi]            ;IP
        mov     ax,GROUP16
        stosw   es:[edi]            ;CS
        mov     eax,d rv30_StackAdd
        stosd   es:[edi]            ;SS:SP
        mov     edi,edx
        ;
        test    BYTE PTR SystemFlags,1
        jz      rv30_Use32Bit12
        push    w[ebx].CallBackStruc.CallBackProt+4
        push    w[ebx].CallBackStruc.CallBackProt+0
        jmp     rv30_Use16Bit12
rv30_Use32Bit12:
        push    d[ebx].CallBackStruc.CallBackProt+4
        push    d[ebx].CallBackStruc.CallBackProt+0
rv30_Use16Bit12:
        mov     ebp,esp
        ;
        ;Setup stack referance.
        ;
        movzx   ebx,[ebx].CallBackStruc.CallBackStackSel
        push    ebx
        push    es
        push    edi

        movzx   eax,es:RealRegsStruc.Real_SS[edi]
        shl     eax,4
        mov     di,KernalZero
        mov     es,edi
        and     ebx,not 7
        mov     edi,d GDTVal+2
        add     edi,ebx
        mov     es:[edi+2],ax   ;store low word of linear base.
        shr     eax,16
        mov     es:[edi+4],al   ;store mid byte of linear base.
        mov     es:[edi+7],ah   ;store high byte of linear base.

        pop     edi
        pop     es
        test    BYTE PTR SystemFlags,1
        pop     ds              ;load DS with stack sel
        assume ds:nothing
        movzx   esi,es:RealRegsStruc.Real_SP[edi]
        pushfd                  ;push a dword to keep stack aligned
        jz      rv30_Use32Bit13
        db 66h                  ;change "fword ptr" to "PF16 ptr"
rv30_Use32Bit13:
        call    fword ptr [ebp]
        cli
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        movzx   esi,es:RealRegsStruc.Real_SS[edi]       ;point to stacked registers.
        mov     ebp,esi
        shl     esi,4
        movzx   eax,es:RealRegsStruc.Real_SP[edi]
        sub     eax,8*4+4*2+2+2*2  ;size PUSHAD, 4 seg regs, flags, CS:IP
        mov     edx,eax            ;DX=realmode SP
        add     esi,eax
        xchg    esi,edi
        mov     eax,es
        mov     fs,eax
        mov     ax,KernalZero
        mov     es,eax
        mov     ecx,8
        cld
        rep     movsd es:[edi],fs:[esi]
        lodsw   fs:[esi]           ;flags
        movsd   es:[edi],fs:[esi]  ;ES,DS
        movsd   es:[edi],fs:[esi]  ;FS,GS
        stosw   es:[edi]           ;flags
        movsd   es:[edi],fs:[esi]  ;CS:IP
        ;
        ;Switch back to real/v86 mode.
        ;
        mov     ecx,ebp
        push    offset rv30_contrm
        jmp     [fProtected2Real]
_cwRaw segment
rv30_contrm:
        add     RawStackPos,RawStackDif
        popad
        pop     es
        pop     ds
        pop     fs
        pop     gs
        popf
        retf
        ;
RawCallBack     endp

;-------------------------------------------------------------------------------
;--- internal callback for IRQs, INTs 1Ch/23h/24h

RawICallBack    proc    near
        cli
        pop     cs:rv3x_Return               ;get return address.
        ;
        ;Check if this call back is busy.
        ;
        push    ax
        push    bx
        push    dx

        mov     ax,cs:rv3x_Return            ;get return address.
        sub     ax,CallBackSize ;back to start of call back entry.
        sub     ax,offset ICallBackList ;offset from start of list.
        xor     dx,dx
        mov     bx,CallBackSize
        div     bx              ;entry number.
        mov     bx,size CallBackStruc
        mul     bx              ;get offset into table.
        mov     bx,offset CallBackTable
        add     bx,ax           ;point to this entry.
        mov     cs:[rv31_CallTab],bx
        ;
        cmp     cs:InWindows,0
        jnz     rv31_ForceOld
        ;
        test    cs:[bx].CallBackStruc.CallBackFlags,80h  ;call back busy?
        jz      rv31_NotBusy
        ;
        ;This is a busy int call back so pass control to old real mode vector.
        ;
rv31_ForceOld:
        mov     ax,WORD PTR cs:[bx].CallBackStruc.CallBackReal+0
        mov     WORD PTR cs:[rv31_tVCPI_SP+0],ax
        mov     ax,WORD PTR cs:[bx].CallBackStruc.CallBackReal+2        ;fetch old real mode vector address.
        mov     WORD PTR cs:[rv31_tVCPI_SP+2],ax
        pop     dx
        pop     bx
        pop     ax
        jmp     DWORD PTR cs:[rv31_tVCPI_SP]    ;pass onto old handler.
        ;
        ;
rv31_NotBusy:
        or      cs:[bx].CallBackStruc.CallBackFlags,80h  ;mark it as busy.
        mov     bx,sp
        mov     ax,ss:[bx+(2+2+2)+(2+2)]
        and     ah,00001100b               ; clear TF,IF,IOPL,NT
        mov     WORD PTR cs:[rv31_FlagsStore+0],ax
        pop     dx
        pop     bx
        pop     ax
        ;
        ;Switch to new stack.
        ;
        mov     w cs:[rv31_tVCPI_SP+0],sp  ;store current stack.
        mov     w cs:[rv31_tVCPI_SP+2],ss
        mov     ss,cs:RawStackReal         ;use new stack.
        mov     esp,cs:RawStackPos
        sub     cs:RawStackPos,RawStackDif
        push    cs:[rv31_tVCPI_SP]         ;put old stack onto new one.
        ;
        ;Preserve registers.
        ;
        push    eax   ;+32
        push    ebx   ;+28
        push    ecx   ;+24
        push    edx   ;+20
        push    esi   ;+16
        push    edi   ;+12
        push    ebp   ;+8
        push    ds    ;+6
        push    es    ;+4
        push    fs    ;+2
        push    gs    ;+0
        ;
        ;Make our data addresable.
        ;
        mov     ax,cs
        mov     ds,ax           ;make our data addresable.
        assume ds:GROUP16
        mov     w[rv31_tVCPI_SP+0],sp
        mov     w[rv31_tVCPI_SP+2],ss
        ;
        ;Switch to protected mode.
        ;
        mov     cx,KernalSS
        movzx   edx,sp
;       mov     edx,RawStackPos
;       sub     RawStackPos,RawStackDif
        push    DpmiEmuCS
        push    lowword offset rv31_contpm
        jmp     [Real2Protected]
_cwRaw ends

rv31_contpm:
        ;
        ;Get protected mode code address.
        ;
        mov     bx,[rv31_CallTab]
        movzx   ebx,[bx].CallBackStruc.CallBackNum  ;get int number.
        shl     ebx,1           ;*2
        lea     ebx,[ebx*2+ebx] ;*6
        add     ebx,offset InterruptTable
;        push    ds
;        mov     ax,DpmiEmuDS
;        mov     ds,eax
;        assume ds:DPMIGRP
        mov     edx,cs:[ebx+0]             ;get offset.
        mov     cx,cs:[ebx+4]              ;get segment selector.
;        pop     ds
;        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        jz      rv31_Use32Bit12
        mov     w[rv31_CallB016+0],dx
        mov     w[rv31_CallB016+2],cx
        jmp     rv31_Use16Bit12
rv31_Use32Bit12:
        mov     d[rv31_CallB032+0],edx
        mov     w[rv31_CallB032+4],cx
rv31_Use16Bit12:
        ;
        ;Retrieve register values.
        ;
        mov     ax,KernalZero
        mov     es,eax
        movzx   esi,w[rv31_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv31_tVCPI_SP+0]
        add     esi,eax         ;Point to stacked registers.
        pushfd
        add     esp,2
        pop     w[rv31_FlagsStore+2]
        mov     eax,es:[esi+32]
        mov     ebx,es:[esi+28]
        mov     ecx,es:[esi+24]
        mov     edx,es:[esi+20]
        mov     edi,es:[esi+12]
        mov     ebp,es:[esi+8]
        mov     esi,es:[esi+16]
        push    [rv31_tVCPI_SP]
        push    [rv31_CallTab]
        push    ds

        test    BYTE PTR SystemFlags,1
        jz      rv31_Use32Bit13

;--- todo: explain the extra 3 (d)words on the stack

        push    w[rv31_FlagsStore+0] ;fl dummy return flags.
        db 66h                     ;push CS as word!
        push    cs                 ;cs dummy return address.
        push    w[rv31_zero]       ;ip
        push    w[rv31_FlagsStore+0]
        call    [rv31_CallB016]    ;far16 call - IRET frame
        lea     esp,[esp+(2*3)]    ;skip the extra words
        jmp     rv31_Use16Bit13
rv31_Use32Bit13:
        push    [rv31_FlagsStore]  ;efl dummy return flags.
        push    cs                 ;cs dummy return address.
        push    d[rv31_zero]       ;eip
        push    [rv31_FlagsStore]
        call    [rv31_CallB032]    ;far32 call - IRETD frame
        lea     esp,[esp+(4*3)]    ;skip the extra dwords
rv31_Use16Bit13:
        pop     ds

        pushfd
        cli
        pop     [rv31_FlagsStore]

        pop     [rv31_CallTab]
        pop     [rv31_tVCPI_SP]

        push    esi
        push    eax
        mov     ax,KernalZero
        mov     es,eax
        movzx   esi,w[rv31_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv31_tVCPI_SP+0]
        add     esi,eax         ;Point to stacked registers.
        ;
        ;Set new register values.
        ;
        pop     d es:[esi+32]
        mov     es:[esi+28],ebx
        mov     es:[esi+24],ecx
        mov     es:[esi+20],edx
        pop     d es:[esi+16]
        mov     es:[esi+12],edi
        mov     es:[esi+8],ebp
        ;
        ;Update flags.
        ;
        movzx   eax,WORD PTR es:[esi+4*2+7*4+0]  ;old real-mode SP
        movzx   esi,WORD PTR es:[esi+4*2+7*4+2]  ;old real-mode SS
        shl     esi,4
        add     esi,eax
        mov     ax,w[rv31_FlagsStore+0]
        and     ah,11111000b           ;clear TF, IF & DF
        and     WORD PTR es:[esi+(2+2)],0000011100000000b
        or      es:[esi+(2+2)],ax
        ;
        mov     bx,[rv31_CallTab]      ;restore call back structure.
;        and     [bx].CallBackStruc.CallBackFlags,255-128 ;clear busy flag.
        and     [bx].CallBackStruc.CallBackFlags,not 80h ;clear busy flag.
        ;
        ;Switch back to real/v86 mode.
        ;
        mov     dx,w[rv31_tVCPI_SP+0]
        mov     cx,w[rv31_tVCPI_SP+2]
        push    offset rv31_contrm
        jmp     [fProtected2Real]

_cwRaw segment

rv31_contrm:
;       add     RawStackPos,RawStackDif
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        lss     sp,[esp]                ;restore original stack.
        add     cs:RawStackPos,RawStackDif
        iret
;
RawICallBack    endp

_cwRaw ends

;-------------------------------------------------------------------------------
;--- call a PL0 proc thru a call gate.
;--- used by DPMI functions 0Bxx to access debug registers DR0-DR7
;--- and exception handler to access control registers CR0-CR4
;--- it's the only place where call gate DpmiEmuPL3toPL0 is used.
;--- in: AX=loword(offset proc) to call

EmuCallPL0Proc  proc near
        pushfd
        cli
        push    es
        push    GDTData
        pop     es
        mov     word ptr es:[DpmiEmuPL3toPL0 and 0fff8h], ax	; adjust offset in call gate
        pop     es
        db 9ah
        dd 0
        dw DpmiEmuPL3toPL0
        popfd
        ret
EmuCallPL0Proc  endp

;-------------------------------------------------------------------------------
;--- called in RAW/VCPI mode only

EmuCR3Flush     proc    near
        push    eax
        mov     eax, offset ecf_pl0
        call    EmuCallPL0Proc
        pop     eax
        ret
        ;
ecf_pl0:
        mov     eax,cr3
        mov     cr3,eax         ;flush page cache.
        retd

EmuCR3Flush     endp

;-------------------------------------------------------------------------------
;
;DPMI int 31h service simulation patch. This provides DPMI API services in a
;Raw/Vcpi environment.
;stack: IRET16/IRET32 frame
;DS not set.
;
RawDPMIPatch    proc    near
        push    offset rv46_Done
        cmp     ah,1
        jb      rv46_DPMI_00xx
        jz      rv46_DPMI_01xx
        cmp     ah,3
        jb      rv46_DPMI_02xx
        jz      rv46_DPMI_03xx
        cmp     ah,5
        jb      rv46_DPMI_0400
        jz      rv46_DPMI_05xx
        cmp     ah,7
        jb      rv46_DPMI_0600
        jz      rv46_DPMI_0700
        cmp     ah,9
        jb      rv46_DPMI_0800
        jz      rv46_DPMI_0900
        cmp     ah,0bh
        jb      rv46_DPMI_0A00
        jz      rv46_DPMI_0B00
        jmp     rv46_NotOurs
        ;
rv46_DPMI_00xx:
        cmp     al,01h
        jb      rv46_DPMI_0000  ;Allocate LDT descriptors
        jz      rv46_DPMI_0001  ;Free LDT descriptors
        cmp     al,03h
        jb      rv46_DPMI_0002  ;Real segment to protected selector
        jz      rv46_DPMI_0003  ;Get selector increment value
        cmp     al,06h
        jb      rv46_NotOurs    ;4 (locksel) & 5 (unlocksel) not impl.
        jz      rv46_DPMI_0006  ;Get selector base address
        cmp     al,08h
        jb      rv46_DPMI_0007  ;Set selector base address
        jz      rv46_DPMI_0008  ;Set segment limit
        cmp     al,0Ah
        jb      rv46_DPMI_0009  ;Set access rights bytes
        jz      rv46_DPMI_000A  ;create data alias of CS
        cmp     al,0Ch
        jb      rv46_DPMI_000B  ;fetch descriptor
        jz      rv46_DPMI_000C  ;put descriptor
        cmp     al,0Dh
        jz      rv46_DPMI_000D  ;allocate specific LDT descriptor
        jmp     rv46_NotOurs
rv46_DPMI_01xx:
        cmp     al,01h
        jb      rv46_DPMI_0100  ;allocate DOS memory
        jz      rv46_DPMI_0101  ;free DOS memory
        cmp     al,02h          ;re-size DOS memory
        jz      rv46_DPMI_0102
        jmp     rv46_NotOurs
rv46_DPMI_02xx:
        cmp     al,01h
        jb      rv46_DPMI_0200  ;get real mode vector
        jz      rv46_DPMI_0201  ;set real mode vector
        cmp     al,03h
        jb      rv46_DPMI_0202  ;get exception vector
        jz      rv46_DPMI_0203  ;set exception vector
        cmp     al,05h
        jb      rv46_DPMI_0204  ;get vector
        jz      rv46_DPMI_0205  ;set vector
        jmp     rv46_NotOurs
rv46_DPMI_03xx:
        cmp     al,01h
        jb      rv46_DPMI_0300  ;simulate int
        jz      rv46_DPMI_0301  ;call real-mode proc with retf frame
        cmp     al,03h
        jb      rv46_DPMI_0302  ;call real-mode proc with iret frame
        jz      rv46_DPMI_0303  ;get CallBack
        cmp     al,05h
        jb      rv46_DPMI_0304  ;release CallBack
        jz      rv46_DPMI_0305  ;get state save/restore address
        cmp     al,06h
        jz      rv46_DPMI_0306  ;get raw mode switch address
        jmp     rv46_NotOurs

rv46_DPMI_0000:
        call    RawGetDescriptors
        ret
        ;
rv46_DPMI_0001:
        call    RawRelDescriptor
        ret
        ;
rv46_DPMI_0002:
        push    ebx             ;save/restore regs to preserve hiword(eax)
        push    eax
        call    RawReal2ProtSel
        pop     ebx
        mov     bx,ax
        mov     eax,ebx
        pop     ebx
        ret
        ;
rv46_DPMI_0003:
        mov     ax,8
        clc
        ret
        ;
rv46_DPMI_0006:
        call    RawGetSelBase
        ret
        ;
rv46_DPMI_0007:
        call    RawSetSelBase
        ret
        ;
rv46_DPMI_0008:
        call    RawSetSelLimit
        ret
        ;
rv46_DPMI_0009:
        call    RawSetSelType
        ret
        ;
rv46_DPMI_000A:
        push    ebx
        push    eax
        push    ecx
        mov     cx,1
        call    RawGetDescriptors
        pop     ecx
        jnc     rv46_000A_0
        pop     eax
        pop     ebx
        ret
;
rv46_000A_0:
        push    eax
        push    ebx
        push    edi
        push    es
        push    eax
        mov     edi,offset RawSelBuffer
        mov     ax,KernalDS
        mov     es,eax
        call    RawBGetDescriptor       ;copy original details.
        mov     BYTE PTR es:[edi+5],DescPresent+DescPL3+DescMemory+DescRWData
        pop     ebx
        call    RawBPutDescriptor       ;set new descriptor details.
        pop     es
        pop     edi
        pop     ebx
        pop     eax
;
        pop     ebx
        mov     bx,ax
        mov     eax,ebx
        pop     ebx
        clc
        ret
        ;
rv46_DPMI_000B:
        call    RawBGetDescriptor
        ret
        ;
rv46_DPMI_000C:
        call    RawBPutDescriptor
        ret
        ;
rv46_DPMI_000D:
        stc
        ret
        ;
rv46_DPMI_0100:
        push    ebp
        push    eax
        push    ebx
        push    edx
        call    RawGetDOSMemory
        jc      rv46_0100_0
        pop     ebp
        mov     bp,dx
        mov     edx,ebp
        pop     ebx
        pop     ebp
        mov     bp,ax
        mov     eax,ebp
        pop     ebp
        ret
rv46_0100_0:
        pop     edx
        pop     ebp
        mov     bp,bx
        mov     ebx,ebp
        pop     ebp
        mov     bp,ax
        mov     eax,ebp
        pop     ebp
        ret
        ;
rv46_DPMI_0101:
        push    ebx
        push    eax

; MED 02/03/2003, Hey, they fixed this later, but I'm leaving the comment and
;  code here as a snapshot in the past of a display of semi-righteous anger
;  and to keep CW compatible with all those people using old Watcom versions.
;  Heck, it was intended for internal viewing only at the time. (RIP Powersoft)
; MED 03/25/97
; Watcom 11.0 is excessively stupid and attempts at startup to release
;  memory used by the stack and DGROUP via DOS memory free call.  Windows 95
;  fails this attempt with a carry flag set (but invalid error code, ax not
;  updated).  CauseWay will now have to check if the release selector in
;  DX is the same as SS and fail the call if so.  Thanks Powersoft.
        mov     eax,ss
        cmp     ax,dx
        pop     eax
        push    eax
        jne     med2_0101       ; not attempting to release SS selector
        stc                             ; flag failure, but no error code update
        jmp     med3_0101

med2_0101:
        call    RawRelDOSMemory
        jc      rv46_0101_0

med3_0101:
        pop     eax
        pop     ebx
        ret
rv46_0101_0:
        pop     ebx
        mov     bx,ax
        mov     eax,ebx
        pop     ebx
        ret
        ;
rv46_DPMI_0102:
        push    ebp
        push    eax
        push    ebx
        call    RawResizeDOSMemory
        jc      rv46_0102_0
        pop     ebx
        pop     eax
        pop     ebp
        ret
rv46_0102_0:
        pop     ebp
        mov     bp,bx
        mov     ebx,ebp
        pop     ebp
        mov     bp,ax
        mov     eax,ebp
        pop     ebp
        ret
        ;
rv46_DPMI_0200:
        call    RawGetRVector
        ret
        ;
rv46_DPMI_0201:
        call    RawSetRVector
        ret
        ;
rv46_DPMI_0202:
        push    eax
        push    edx
        call    RawGetEVector
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        pop     eax
        jz      rv46_0202_0
        mov     ax,dx
        mov     edx,eax
rv46_0202_0:
        pop     eax
        clc
        ret
        ;
rv46_DPMI_0203:
        push    edx
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_0203_0
        movzx   edx,dx
rv46_0203_0:
        call    RawSetEVector
        pop     edx
        ret
        ;
rv46_DPMI_0204:
        push    eax
        push    edx
        call    RawGetVector
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        pop     eax
        jz      rv46_0204_0
        mov     ax,dx
        mov     edx,eax
rv46_0204_0:
        pop     eax
        clc
        ret
        ;
rv46_DPMI_0205:
        push    edx
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_0205_0
        movzx   edx,dx
rv46_0205_0:
        call    RawSetVector
        pop     edx
        ret
        ;
rv46_DPMI_0300:
        push    eax
        mov     eax, offset RawSimulateInt2
        jmp     rv46_DPMI_030x

rv46_DPMI_0301:
        push    eax
        mov     eax, offset RawSimulateFCall2
        jmp     rv46_DPMI_030x

rv46_DPMI_0302:
        push    eax
        mov     eax, offset RawSimulateFCallI
rv46_DPMI_030x:
;
;Simulate the int/far call retf/far call iret
;
        push    edi
        push    ebp
        lea     ebp,[esp+(4*4)+(4+4+4)]
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_030x_0a
        movzx   edi,di
        movzx   ebp,bp
        sub     ebp,(2+2+2)
rv46_030x_0a:
;        push    es:RealRegsStruc.Real_CSIP[edi]   ; do not modify CS:IP and SS:SP members
;        push    es:RealRegsStruc.Real_SSSP[edi]
        call    eax
;        pop     es:RealRegsStruc.Real_SSSP[edi]
;        pop     es:RealRegsStruc.Real_CSIP[edi]
        jc      rv46_030x_9
;
;Mask real mode register structure flags.
;
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_030x_0
        mov     ax,[ebp-2]     ;get original flags.
        jmp     rv46_030x_1
rv46_030x_0:
        mov     ax,[ebp-4]     ;get original flags.
rv46_030x_1:
        and     ax,0000111000000000b        ;retain IF, DF, OF
        and     es:RealRegsStruc.Real_Flags[edi],1111000111111111b      ;lose IF, DF, OF
        or      es:RealRegsStruc.Real_Flags[edi],ax
rv46_030x_9:
        pop     ebp
        pop     edi
        pop     eax
        ret
        ;
rv46_DPMI_0303:
        push    eax
        push    ecx
        push    edx
        call    RawGetCallBack
        pop     eax
        mov     ax,dx
        mov     edx,eax
        pop     eax
        mov     ax,cx
        mov     ecx,eax
        pop     eax
        ret
        ;
rv46_DPMI_0304:
        call    RawRelCallBack
        ret
        ;
rv46_DPMI_0305:
        mov     ax,4            ;size of buffer in bytes ( CW saves/restores just a DWORD var: RawStackPos)
        ;BX:CX = real-mode entry for function
        mov     bx,seg StateSaveRM
        mov     cx,offset StateSaveRM
        ;SI:(E)DI = protected-mode entry for function
        mov     si,KernalCS
        mov     di,offset StateSavePM
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jnz     @F
        movzx   edi,di
@@:
        clc
        ret
        ;
rv46_DPMI_0306:
        ;BX:CX = real-mode entry for function
        mov     bx,seg SwitchProcRM
        mov     cx,offset SwitchProcRM
        ;SI:(E)DI = protected-mode entry for function
        mov     si,KernalCS
        mov     di,offset SwitchProcPM
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jnz     @F
        movzx   edi,di
@@:
        clc
        ret
        ;
rv46_DPMI_0400:
        cmp     al,00h          ;get DPMI version?
        jnz     rv46_NotOurs
        mov     ah,0
        mov     al,90           ; changed from 90h to 90 decimal, MED 01/24/96

; MED 01/25/96
;       mov     bx,00000111b
        mov     bx,00000011b
; see if should turn on virtual memory supported bit 2
        mov     dx,cs:DpmiEmuSystemFlags
        and     dx,SF_VMM               ; isolate VMM bit
        shl     dx,1                    ; shift to bit 2 position
        or      bx,dx                   ; turn on VMM bit if set in SystemFlags

; MED 01/25/96
;       mov     cl,3
        push    eax
        push    edx
        pushfd
        pop     eax
        mov     edx,eax
        xor     eax,00040000H
        push    eax
        popfd
        pushfd
        pop     eax
        and     eax,00040000H
        and     edx,00040000H
        mov     cl,3            ; flag 386
        cmp     eax,edx
        je      medcpu

        push    edx
        popfd
        pushfd
        pop     eax
        mov     edx,eax
        xor     eax,00200000H
        push    eax
        popfd
        pushfd
        pop     eax
        push    edx
        popfd
        and     eax,00200000H
        and     edx,00200000H
        mov     cl,4            ; flag 486
        cmp     eax,edx
        je      medcpu

        mov     cl,5            ; flag 586/Pentium

medcpu:
        pop     edx
        pop     eax

        mov     dh,08h
        mov     dl,70h
        clc
        ret
        ;
rv46_DPMI_05xx:
        cmp     al,01h          ;get memory information?
        jb      rv46_DPMI_0500
        jz      rv46_DPMI_0501
        cmp     al,03h          ;get memory block?
        jb      rv46_DPMI_0502
        jz      rv46_DPMI_0503
        jmp     rv46_NotOurs
rv46_DPMI_0502:
        call    RawRelMemory
        ret
rv46_DPMI_0503:
        push    eax
        mov     eax,offset RawResizeMemory
        jmp     rv46_501_503
rv46_DPMI_0501:        
        push    eax
        mov     eax,offset RawGetMemory
rv46_501_503:
        push    esi
        push    edi
        push    ebx
        push    ecx
        call    eax
        jc      rv46_501_503_err
        pop     eax            ;ensure hiword(ecx/ebx/esi/edi) aren't modified
        mov     ax,cx
        mov     ecx,eax
        pop     eax
        mov     ax,bx
        mov     ebx,eax
        pop     eax
        mov     ax,di
        mov     edi,eax
        pop     eax
        mov     ax,si
        mov     esi,eax
        pop     eax
        ret
rv46_501_503_err:
        pop     ecx
        pop     ebx
        pop     edi
        pop     esi
        pop     eax
        ret
        ;
rv46_DPMI_0600:
        cmp     al,00h          ;lock memory?
        jnz     rv46_DPMI_0601
        call    RawLockMemory
        ret
        ;
rv46_DPMI_0601:
        cmp     al,01h          ;un-lock memory?
        jnz     rv46_DPMI_0602
        call    RawUnLockMemory
        ret
        ;
rv46_DPMI_0602:
        cmp     al,02h          ;mark real mode region as swapable?
        jnz     rv46_DPMI_0603
        clc
        ret
        ;
rv46_DPMI_0603:
        cmp     al,03h          ;re-lock real mode region?
        jnz     rv46_DPMI_0604
        clc
        ret
        ;
rv46_DPMI_0604:
        cmp     al,04h          ;get page size?
        jnz     rv46_NotOurs
        xor     bx,bx
        mov     cx,4096
        clc
        ret
        ;
rv46_DPMI_0700:
rv46_DPMI_0701:
rv46_DPMI_0702:
        cmp     al,02h          ;mark page as demand pageing?
        jnz     rv46_DPMI_0703
        clc
        ret
        ;
rv46_DPMI_0703:
        cmp     al,03h          ;discard page contents?
        jnz     rv46_NotOurs
        call    RawDiscardPages
        ret
        ;
rv46_DPMI_0800:
        cmp     al,00h          ;map physical to linear?
        jnz     rv46_DPMI_0801
        push    eax
        push    ebx
        push    ecx
        call    RawMapPhys2Lin
        pop     eax
        mov     ax,cx
        mov     ecx,eax
        pop     eax
        mov     ax,bx
        mov     ebx,eax
        pop     eax
        ret
        ;
rv46_DPMI_0801:
        cmp     al,01h          ;un-map physical to linear?
        jnz     rv46_NotOurs
        call    RawUnMapPhys2Lin
        ret
        ;
rv46_DPMI_0900:                 ;enable/disable/get virtual interupts.
        cmp     al,3
        jae     rv46_NotOurs
        push    ebp
        lea     ebp,[esp+2*4+2*4]
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_1
        movzx   ebp,bp
        sub     ebp,2*2
rv46_1:
        cmp     al,01h
        ja      rv46_0902
        jz      rv46_0901
rv46_0900:                      ;disable virtual IF
        btr     d[ebp],9
        jmp     rv46_3
rv46_0901:                      ;enable virtual IF
        bts     d[ebp],9
        jmp     rv46_3
rv46_0902:                      ;get virtual IF
        bt      d[ebp],9
rv46_3:
        setc    al
        pop     ebp
        clc
        ret
        ;
rv46_DPMI_0A00:
        cmp     al,00h          ;get vendor specific API?
        jnz     rv46_NotOurs

;MED, 11/30/95
; tell inquiring Watcom that CauseWay is DOS4/GW so that it sets up
;  the FPU emulation properly
        push    edi             ; maintain in case of failure (can be changed otherwise)
        push    esi
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ds,[Group32DS]
        assume ds:GROUP32
        cmp     Int21hDOS4GFlag,0
        pop     ds
        assume ds:nothing
        je      DPMI_0A00_NotDOS4G

        mov     edi,OFFSET RationalCopyright
DPMI_0A00_loop:
        lodsb
        cmp     al,cs:[edi]
        jne     DPMI_0A00_NotDOS4G
        inc     edi
        cmp     al,0
        jne     DPMI_0A00_loop
        pop     esi
        pop     edi
        ; es:edi -> dummy extension entry point
        push    cs
        pop     es
        mov     edi,OFFSET DPMI_0A00_APIEntryPoint
        clc
        ret
DPMI_0A00_NotDOS4G:
        pop     esi
        pop     edi
        mov     ax,8001h
        stc
        ret

; dummy entry point
DPMI_0A00_APIEntryPoint:
        stc
        retf
_cwDPMIEMU$1 segment
RationalCopyright       DB      "RATIONAL DOS/4G",0
_cwDPMIEMU$1 ends
;
;Set hardware break point.
;BX:CX=linear address, DX=size & type
;
rv46_DPMI_0B00:
        cmp     al,00h          ;set debug watch point?
        jnz     rv46_DPMI_0B01
        push    eax
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    ebx
        ;
        ;Check size value
        ;
        cmp     dl,0
        jz      rv46_0B00_9
        cmp     dl,3
        jz      rv46_0B00_9
        cmp     dl,4+1
        jnc     rv46_0B00_9
        ;
        ;Check type value
        ;
        cmp     dh,2+1
        jnc     rv46_0B00_9
        ;
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        shl     ebx,16
        mov     bx,cx
        ;
        ;Find a free table entry.
        ;
        mov     esi,offset DbgTable
        mov     ecx,4
        xor     edi,edi
rv46_0B00_0:
        test    [esi].DbgStruc._Flags,128  ;free?
        jz      rv46_0B00_1
        inc     edi
        add     esi,size DbgStruc
        loop    rv46_0B00_0
        jmp     rv46_0B00_9
rv46_0B00_1:
        mov     [esi].DbgStruc._Flags,128
        mov     [esi].DbgStruc._Address,ebx
        mov     [esi].DbgStruc._Size,dl
        mov     [esi].DbgStruc._Type,dh
        ;
        ;Set length/type/enable
        ;
        movzx   eax,dh
        movzx   edx,dl
        mov     dl,[edx+DbgSize2Dr]
        shl     dl,2
        mov     al,[eax+DbgType2Dr]
        or      dl,al

        mov     ecx,edi
        shl     ecx,2     ;4 bits per WP in hiword(dr7)
        add     ecx,16
        mov     eax,1111b
        shl     edx,cl
        shl     eax,cl

        mov     ecx,edi
        shl     ecx,1     ;2 bits per WP in lobyte(dr7)
        mov     al,11b
        shl     al,cl
        or      dl,al

        mov     ecx,eax
        not     ecx
        ;
        ;Program the hardware.
        ;
        mov     eax, offset rv46_0B00_pl0
        call    EmuCallPL0Proc
        ;
        pop     ebx
        mov     bx,di
        clc
        jmp     rv46_0B00_10
rv46_0B00_9:
        pop     ebx
        stc
rv46_0B00_10:
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        ret

rv46_0B00_pl0:        
        mov     eax,dr6
        btr     eax,edi
        mov     dr6,eax
        lea     eax,[edi*4+offset rv46_0B00_d0]
        call    eax
        mov     eax,dr7
        and     eax,ecx
        or      eax,edx
        mov     dr7,eax
        retd
rv46_0B00_d0:
        mov     dr0,ebx
        retn
rv46_0B00_d1:
        mov     dr1,ebx
        retn
rv46_0B00_d2:
        mov     dr2,ebx
        retn
rv46_0B00_d3:
        mov     dr3,ebx
        retn
        ;
        assume ds:nothing
;
;Release hardware break point.
;
rv46_DPMI_0B01:
        cmp     al,01h          ;clear debug watch point?
        jnz     rv46_DPMI_0B02
        pushad
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        ;
        ;Check handle range.
        ;
        movzx   ebx,bx
        cmp     ebx,4
        jnc     rv46_0B01_9
        ;
        ;Point to Dbg entry.
        ;
        mov     eax,size DbgStruc
        mul     ebx
        lea     esi,[eax+offset DbgTable]
        test    [esi].DbgStruc._Flags,128
        jz      rv46_0B01_9
        ;
        ;Free Dbg entry.
        ;
        mov     [esi].DbgStruc._Flags,0
        ;
        ;Update hardware.
        ;
        mov     eax, offset rv46_0B01_pl0
        call    EmuCallPL0Proc
        ;
        clc
        jmp     rv46_0B01_10
        ;
rv46_0B01_9:
        stc
rv46_0B01_10:
        pop     ds
        assume ds:nothing
        popad
        ret

rv46_0B01_pl0:
        mov     ecx,ebx
        shl     ecx,1
        mov     eax,11b
        shl     eax,cl
        not     eax
        mov     ecx,dr7
        and     ecx,eax
        mov     dr7,ecx
        retd
;
;Get state of break point.
;out: ax=0000/0001
;
rv46_DPMI_0B02:
        cmp     al,02h          ;get debug watch point state?
        jnz     rv46_DPMI_0B03
        push    eax
        ;
        ;Check handle range.
        ;
        cmp     bx,4
        jnc     rv46_0B02_9
        ;
        mov     ax,size DbgStruc
        imul    ax,bx
        movzx   eax,ax
        test    [DbgTable+eax]._Flags,128   ;Active?
        jz      rv46_0B02_9
        ;
        ;Check state in hardware.
        ;
        mov     eax, offset rv46_0B02_pl0
        call    EmuCallPL0Proc
        ;
        ;check bit we want.
        ;
        bt      ax,bx
        pop     eax
        setc    al
        xor     ah,ah
        ret
rv46_0B02_9:
        stc
        pop     eax
        ret

rv46_0B02_pl0:
        mov     eax,dr6
        retd
;
;Reset state of hardware breakpoint.
;
rv46_DPMI_0B03:
        cmp     al,03h          ;reset debug watch point?
        jnz     rv46_NotOurs
        push    eax
        ;
        ;Check handle range.
        ;
        cmp     bx,4
        jnc     rv46_0B03_9
        ;
        mov     ax,size DbgStruc
        imul    ax,bx
        movzx   eax,ax
        test    [DbgTable+eax]._Flags,128   ;Active?
        jz      rv46_0B03_9
        ;
        ;Update state in hardware.
        ;
        mov     eax, offset rv46_0B03_pl0
        call    EmuCallPL0Proc
        ;
        clc
        jmp     rv46_0B03_10
        ;
rv46_0B03_9:
        stc
rv46_0B03_10:
        pop     eax
        ret

rv46_0B03_pl0:        
        mov     eax,dr6
        btr     ax,bx
        mov     dr6,eax
        retd
        ;
rv46_Done:
        ;Now update stacked flags.
        ;
        push    eax
        setc    al
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_Use32Bit8
        push    ebp
        movzx   ebp,sp
        and     b[ebp+2*4+(2+2)],not 1
        or      [ebp+2*4+(2+2)],al          ;modify stack flags.
        pop     ebp
        pop     eax
        iret
rv46_Use32Bit8:
        and     b[esp+4+(4+4)],not 1
        or      [esp+4+(4+4)],al            ;modify stack flags.
        pop     eax
        iretd
        ;
rv46_NotOurs:
if 0
        ;Not a function recognised by us so pass control to previous handler.
        add     esp, 4
        jmp     cs:[DefaultInt31]           ;pass it onto previous handler.
else
        ;just return with Carry set.
        stc
        ret
endif
        ;

rv46_DPMI_0500:

DPMI_MINFO struct
maxBlock      dd ?  ;0	virtual memory (bytes)
freeUnlocked  dd ?  ;4	pages (max unlocked allocation)
maxLockable   dd ?  ;8	pages (max locked allocation)
totalAdrSpace dd ?  ;12 pages
unlocked      dd ?  ;16 pages
freePhys      dd ?  ;20 pages
totalPhys     dd ?  ;24 pages
freeAdrSpace  dd ?  ;28 pages
swapFile      dd ?  ;32
rsvd          dd ?,?,?
DPMI_MINFO ends

        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi

        call    RawGetMemoryMax   ;get max virtual memory in EBX

        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_0
        movzx   edi,di
rv46_0:
        mov     es:[edi].DPMI_MINFO.maxBlock,ebx
        shr     ebx,12
        mov     es:[edi].DPMI_MINFO.freeUnlocked,ebx

        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16

        call PhysicalGetPages           ;get number of un-claimed pages.
        add     edx,NoneLockedPages     ;include currently un-locked pages.
        mov eax,edx
        shr eax,10                      ;work out page tables needed for this.
        shl eax,1                       ;2 pages per table.
        sub edx,eax                     ;lose page tables.
        cmp edx,ebx
        jc rv46_500_0
        mov edx,ebx
rv46_500_0:
        mov     es:[edi].DPMI_MINFO.maxLockable,edx

; MED 01/25/96
;       mov     eax,LinearLimit
;       sub     eax,LinearBase
;       shr     eax,12
;       mov     es:d[edi+0ch],eax

        mov     eax,NoneLockedPages
        mov     es:[edi].DPMI_MINFO.unlocked,eax

        ;
        ;Get free disk space remaining.
        ;
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi

        xor     edx,edx
        cmp     VMMHandle,0
        jz      rv46_500_1

;--- v5.0: no DOS access if InDos flag is set
        mov     eax,SDAaddress
        push    es
        mov     es,RealSegment
        cmp     es:[eax].SDA.bInDos,0
        pop     es
        jnz     rv46_500_1

        mov     dl,VMMName      ;get drive letter for this media.
        sub     dl,'A'          ;make it real.
        inc     dl              ;adjust for current type select.
        mov     ah,36h          ;get free space.
        int     21h             ;/
        xor     edx,edx
        cmp     ax,-1           ;invalid drive?
        jz      rv46_500_1
        mul     cx              ;Get bytes per cluster.
        mul     bx              ;Get bytes available.
        shl     edx,16
        mov     dx,ax
        add     edx,SwapFileLength      ;include current size.
        shr     edx,12
        ;
        ;Work out how much of the VMM space is extra.
        ;
        mov     eax,LinearLimit
        sub     eax,LinearBase
        shr     eax,12
        sub     edx,eax
        ;
rv46_500_1:
        mov     ebx,edx         ; MED 01/25/96
;       add     edx,FreePages
        mov     edx,FreePages   ; MED 01/25/96

        push    edx
        call    PhysicalGetPages

; MED 01/25/96
        pop     eax                     ; save edx value off of stack
        pop     edi                     ; restore original edi value
        push    eax             ; restore edx value to stack
        test    ebx,ebx ; see if any virtual memory
        jnz     med2
        mov     ebx,edx
med2:
        mov     eax,LinearLimit
        sub     eax,LinearBase
        shr     eax,12
        add     ebx,eax
        mov     es:[edi].DPMI_MINFO.totalAdrSpace,ebx

        mov     eax,edx
        pop     edx
        add     edx,eax
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        mov     es:[edi].DPMI_MINFO.freePhys,edx

; MED 01/25/96
;       mov     es:d[edi+1ch],edx
        mov     eax,es:[edi].DPMI_MINFO.totalAdrSpace
        sub     eax,medAllocPages

; MED 02/15/96
        mov     edx,MaxMemLin
        shr     edx,12
        sub     edx,medAllocPages
        cmp     eax,edx                         ; see if greater than MAXMEM choke-off point
        jbe     med3
        mov     eax,edx

med3:
        mov     es:[edi].DPMI_MINFO.freeAdrSpace,eax
        add     es:[edi].DPMI_MINFO.unlocked,edx      ; MED 01/25/96

        mov     eax,TotalPages
        add     eax,TotalPhysical       ; MED 01/25/96
        mov     es:[edi].DPMI_MINFO.totalPhys,eax

        mov     eax,SwapFileLength
        shr     eax,12
        mov     es:[edi].DPMI_MINFO.swapFile,eax

        mov     es:[edi].DPMI_MINFO.rsvd+0,-1
        mov     es:[edi].DPMI_MINFO.rsvd+4,-1
        mov     es:[edi].DPMI_MINFO.rsvd+8,-1

        pop     ds
        assume ds:nothing

        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        clc
        retn

RawDPMIPatch    endp


;-------------------------------------------------------------------------------
;--- PROCs that are called from other segments (GROUP16)

if RELXMSINRM eq 0 and VCPIPMCALL eq 0
_ffRawSimulateFCall proc far
        call    RawSimulateFCall
        ret
_ffRawSimulateFCall endp
endif

_ffPhysicalGetPage proc far
        call    PhysicalGetPage
        ret
_ffPhysicalGetPage endp

_ffPhysicalGetPages proc far
        call    PhysicalGetPages
        ret
_ffPhysicalGetPages endp


;-------------------------------------------------------------------------------
;
;Get a page of physical memory from one of the possible sources.
;
;On Exit:-
;
;CL     - Flags to apply to page.
;EDX    - Physical address of page if any.
;
PhysicalGetPage proc near
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        push    gs
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     MaxMemPhys,0
        assume ds:DPMIGRP
        stc
        jz      rv50_9
        ;
        call    GetVCPIPage
        jnc     rv50_8
        call    GetXMSPage
        jnc     rv50_8
        call    GetINT15Page
        jnc     rv50_8
ifndef NOLOWMEM
        call    GetCONVPage
        jnc     rv50_8
endif
        jmp     rv50_9
        ;
rv50_8:
;        and     edx,not 4095
        and     dx,0f000h
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        dec     MaxMemPhys
        dec     TotalPhysical
        jns     rv50_nowrap
        mov     TotalPhysical,0
rv50_nowrap:
        clc
        ;
rv50_9:
        pop     gs
        pop     fs
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
PhysicalGetPage endp


;-------------------------------------------------------------------------------
;
;Get number of memory pages suppliable by all current memory types.
;
;On Exit:-
;
;EDX    - Pages supported.
;
PhysicalGetPages proc near
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        push    gs
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        ;
        mov     [rv51_Total],0
        call    GetVCPIPages
        add     [rv51_Total],edx

; MED, 11/11/99
        test    edx,edx
        jne     pgp2                    ; VCPI available memory exists
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     VCPIHasNoMem,1  ; flag no memory, so XMS gets a crack at it
        pop     ds
        assume ds:DPMIGRP
pgp2:

        call    GetXMSPages
        add     [rv51_Total],edx

; MED, 11/11/99
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     VCPIHasNoMem,0  ; reset flag
        pop     ds
        assume ds:DPMIGRP

        call    GetINT15Pages
        add     [rv51_Total],edx
ifndef NOLOWMEM
        call    GetCONVPages
        jc      pgp3                    ; error allocating pages, MED, 11/15/99
        add     [rv51_Total],edx
pgp3:
endif
        mov     edx,[rv51_Total]
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     edx,MaxMemPhys
        jc      rv51_0
        mov     edx,MaxMemPhys
rv51_0: mov     TotalPhysical,edx
        clc                     ;exit with success.
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
        ;

PhysicalGetPages endp

;-------------------------------------------------------------------------------
;
;Attempt to allocate a page of VCPI memory.
;
GetVCPIPage     proc    near
        push    eax
        push    edi
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     ProtectedType,PT_VCPI
        jnz     rv52_9
if VCPIPMCALL
        mov     ax, lowword offset allocvcpi
        call    EmuCallPL0Proc
        or      ah,ah
        stc
        jnz     rv52_9
else
        push    es
        push    ebx
        mov     es,eax
        mov     ax,0DE04h               ;allocate 4k page.
        mov     edi,offset MemIntBuffer
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_IP[edi],offset Int67h
        mov     RealRegsStruc.Real_CS[edi],GROUP16
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
        pop     ebx
        pop     es
        mov     eax,RealRegsStruc.Real_EAX[edi]
        or      ah,ah           ;get anything?
        stc
        jnz     rv52_9
        mov     edx,RealRegsStruc.Real_EDX[edi]
endif
        mov     ecx,1           ;mark it as VCPI memory.
        clc
rv52_9:
        pop     ds
        pop     edi
        pop     eax
        ret
if VCPIPMCALL
allocvcpi:
        assume ds:GROUP16
        mov ax, 0DE04h
        jmp [VCPI_Entry]
endif
        assume ds:nothing
GetVCPIPage     endp

;-------------------------------------------------------------------------------
;
;Find out how many pages VCPI could supply.
;
;On Exit:-
;
;EDX    - Pages available.
;
GetVCPIPages    proc    near
        push    eax
        push    ds
        xor     edx,edx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     ProtectedType,PT_VCPI
        jnz     rv53_9
if VCPIPMCALL
        mov     ax, lowword offset getfreevcpipages
        call    EmuCallPL0Proc
else
        push    edi
        push    es
        push    ebx
        push    ecx
        mov     es,eax
        mov     ax,0DE03h               ;get number of free pages.
        mov     edi,offset MemIntBuffer
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_IP[edi],offset Int67h
        mov     RealRegsStruc.Real_CS[edi],GROUP16
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
        pop     ecx
        pop     ebx
        pop     es
        mov     eax,RealRegsStruc.Real_EAX[edi]
        mov     edx,RealRegsStruc.Real_EDX[edi]
        pop     edi
endif
        or      ah,ah           ;get anything?
        jz      rv53_10
rv53_9:
        xor     edx,edx
        stc
        ;
rv53_10:
        @dprintf DOPT_PHYSMEM,<"GetVCPIPages: %lu",10>,edx
        pop     ds
        pop     eax
        ret
if VCPIPMCALL
getfreevcpipages:
        assume ds:GROUP16
        mov ax, 0DE03h
        jmp [VCPI_Entry]
endif
        assume ds:nothing

GetVCPIPages    endp



;-------------------------------------------------------------------------------
;
;Attempt to allocate another page of XMS memory.
;
GetXMSPage      proc    near
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        cmp     XMSPresent,0
        jz      rv54_9
        ;
rv54_3:                                        ;<--- restart
        mov     esi,offset XMSList
        xor     edi,edi
        mov     ecx,XMSITEMS
rv54_0:                                        ;<----
        cmp     [esi].XMSMEMITEM.dwBase,0      ;This entry in use?
        jz      rv54_1
        ;
        ;Anything left in this block?
        ;
        mov     eax,[esi].XMSMEMITEM.dwBase    ;get current base.
        add     eax,4096                       ;next page.
        cmp     eax,[esi].XMSMEMITEM.dwEnd
        jc      rv54_GotOne
        jmp     rv54_2          ;nothing left in this block.
        ;
rv54_1:
        or      edi,edi         ;already got a free entry?
        jnz     rv54_2
        mov     edi,esi
rv54_2:
        add     esi,sizeof XMSMEMITEM          ;next entry.
        loop    rv54_0
        ;
        ;Need to try and allocate a new block.
        ;
        mov     esi,edi         ;get free entry number.
        or      esi,esi
        jz      rv54_9
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     ah,08h          ;query free memory
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall                 ;get size of largest block.
        mov     eax,RealRegsStruc.Real_EAX[edi]  ; returns AX=size of largest free block in kB
        or      ax,ax
        jz      rv54_9
        cmp     ax,4096/1024
        jc      rv54_9
        ;
        ;Limit to pre-defined size.
        ;
        cmp     ax,XMSBlockSize
        jc      rv54_SizeOK
        mov     ax,XMSBlockSize
rv54_SizeOK:
        push    eax
        movzx   eax,ax
        add     eax,3
        shr     eax,2
        cmp     eax,MaxMemPhys
        pop     eax
        jc      rv54_nomaxlimit
        mov     eax,MaxMemPhys
        shl     eax,2
rv54_nomaxlimit:
        ;
        push    eax
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EDX[edi],eax
        mov     ah,9                  ;alloc EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall      ;allocate memory.
        pop     ebp
        mov     eax,RealRegsStruc.Real_EAX[edi]
        cmp     ax,1
        jnz     rv54_9
        mov     edx,RealRegsStruc.Real_EDX[edi] ;get the handle.
        mov     [esi].XMSMEMITEM.wHdl,dx
        push    edx
        mov     ah,0ch                  ;lock EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
;        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall        ;lock block & get address.
        pop     ecx
        mov     edx,RealRegsStruc.Real_EDX[edi]
        mov     ebx,RealRegsStruc.Real_EBX[edi]
        mov     eax,RealRegsStruc.Real_EAX[edi]
        cmp     ax,1
        jnz     rv54_9          ;should never happen.
        ;
        ;BP    - Block size in K.
        ;DX:BX - Block linear address.
        ;
        movzx   edi,dx          ;get base into 1 reg.
        shl     edi,16          ;/
        mov     di,bx           ;/
        mov     ebx,edi         ;copy into high address.
        movzx   ebp,bp          ;fetch size.
        shl     ebp,10          ;*1024 (1k)
        add     ebx,ebp         ;get real top.
        add     edi,4095                ;round up to next page.
;        and     edi,0FFFFFFFFh-4095     ;/
        and     di,0f000h
;        and     ebx,0FFFFFFFFh-4095     ;round down to nearest page.
        and     bx,0f000h
        mov     [esi].XMSMEMITEM.dwBase,edi
        mov     [esi].XMSMEMITEM.dwEnd,ebx   ;store base and end.
        jmp     rv54_3          ;start again.
        ;
rv54_GotOne:
        ;Update table entry indicated and return physical address.
        ;
        mov     edx,[esi].XMSMEMITEM.dwBase
        add     [esi].XMSMEMITEM.dwBase,4096
        ;
        xor     ecx,ecx
        clc
        jmp     rv54_10
        ;
rv54_9: stc
        ;
rv54_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
GetXMSPage      endp


;-------------------------------------------------------------------------------
;
;Determine how many pages of XMS memory could be allocated.
;
GetXMSPages     proc    near
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        mov     [XMSTotal],0

; MED, 11/11/99
;       cmp     ProtectedType,PT_VCPI
        cmp     VCPIHasNoMem,0  ; see if VCPI provided no memory, bail if it did

        jz      rv55_9
        cmp     XMSPresent,0
        jz      rv55_9
        ;
        mov     edi,offset XMSTempList
        mov     esi,edi
        mov     ecx,XMSITEMS
        xor     eax,eax
        push    ds
        pop     es
        cld
        rep     stosd
rv55_0:                                 ;<--- next EMB
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        ;
        mov     ah,08h                  ;query free mem
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
        mov     eax,RealRegsStruc.Real_EAX[edi] ; ax=size of largest free block
        or      ax,ax
        jz      rv55_2
        ;
        ;Limit to pre-defined size.
        ;
        cmp     ax,XMSBlockSize
        jc      rv55_SizeOK
        mov     ax,XMSBlockSize
rv55_SizeOK:
        push    eax
        mov     RealRegsStruc.Real_EDX[edi],eax
        mov     ah,9                    ; alloc EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
;        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
        pop     ebp
        mov     eax,RealRegsStruc.Real_EAX[edi]
        or      ax,ax
        jz      rv55_2
        mov     edx,RealRegsStruc.Real_EDX[edi] ;get the handle.
        mov     [esi+0],dx
        mov     w[esi+2],1
        add     esi,4
        mov     ah,0ch                   ; lock EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
;        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall         ;lock block & get address.
        mov     edx,RealRegsStruc.Real_EDX[edi]
        mov     ebx,RealRegsStruc.Real_EBX[edi]
        mov     eax,RealRegsStruc.Real_EAX[edi]
        or      ax,ax
        jz      rv55_2          ;should never happen.
        ;
        ;BP    - Block size in K.
        ;DX:BX - Block linear address.
        ;
        movzx   edi,dx          ;get base into 1 reg.
        shl     edi,16          ;/
        mov     di,bx           ;/
        mov     ebx,edi         ;copy into high address.
        movzx   ebp,bp          ;fetch size.
        shl     ebp,10          ;*1024 (1k)
        add     ebx,ebp         ;get real top.
        add     edi,4096-1      ;round up to next page.
        shr     edi,12
        shr     ebx,12
        sub     ebx,edi
        js      rv55_1
        dec     ebx
        js      rv55_1
        add     [XMSTotal],ebx
rv55_1:
        cmp     esi,offset XMSTempList + XMSITEMS*4
        jnz     rv55_0
        ;
rv55_2: ;Now release all memory blocks again.
        ;
        jmp     rv55_4
rv55_3:                            ;<---- next block
        cmp     d[esi],0
        jz      rv55_4
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     dx,[esi+0]
        mov     ah,0dh                  ;unlock EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
;        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
        mov     ah,0ah                  ;free EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
;        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall
rv55_4:
        sub     esi, 4
        cmp     esi, offset XMSTempList
        jnc     rv55_3
        ;
        ;Now find out how much of existing blocks is free.
        ;
        mov     esi,offset XMSList
        mov     ecx,XMSITEMS
rv55_5:
        cmp     [esi].XMSMEMITEM.dwBase,0      ;This entry in use?
        jz      rv55_6
        ;
        ;Anything left in this block?
        ;
        mov     eax,[esi].XMSMEMITEM.dwBase    ;get current base.
        add     eax,4096                ;next page.
        cmp     eax,[esi].XMSMEMITEM.dwEnd
        jnc     rv55_6          ;nothing left in this block.
        mov     eax,[esi].XMSMEMITEM.dwEnd
        sub     eax,[esi].XMSMEMITEM.dwBase
        shr     eax,12          ;free pages remaining
        dec     eax
        add     [XMSTotal],eax
        ;
rv55_6: add     esi,sizeof XMSMEMITEM
        loop     rv55_5
        @dprintf DOPT_PHYSMEM,<"GetXMSPages: %lu",10>,edx
        ;
rv55_9: ;Now return pages found.
        ;
        mov     edx,[XMSTotal]
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
GetXMSPages     endp


;-------------------------------------------------------------------------------
GetINT15Page    proc    near
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     XMSPresent,0
        jnz     rv56_9
        ;
rv56_3:                      ;<---- next try
        mov     esi,offset Int15Table
        mov     ecx,INT15ITEMS
        xor     edi,edi
rv56_0:                      ;<---- next item
        mov     eax,[esi+0]
        cmp     eax,0
        jz      rv56_1
        add     eax,4096
        cmp     eax,[esi+4]
        jc      rv56_GotOne
        jmp     rv56_2
rv56_1:
        or      edi,edi
        jnz     rv56_2
        mov     edi,esi
rv56_2:
        add     esi,4+4
        loop    rv56_0
        ;
        ;Need to get another block of memory.
        ;
        or      edi,edi
        jz      rv56_9
        cmp     ILevel,INT15ITEMS
        jnc     rv56_9          ;can't cope with any more.
        push    edi
        mov     cx,KernalZero
        mov     es,ecx
        movzx   edi,WORD PTR es:[(VDiskSigVec*4)+2]
        shl     edi,4
        mov     eax,100000h     ;Start of extended memory.
        add     edi,VDiskSigOffs
        mov     esi,offset VDiskSig
        mov     cx,VDiskSigLen
        push    edi
        cld
        repe    cmpsb
        pop     edi
        jne     rv56_GotBottom
        xor     eax,eax
        mov     al,es:[edi+VDiskHiOffs]
        shl     eax,16
        mov     ax,es:[edi+VDiskLoOffs]
rv56_GotBottom:
        mov     esi,eax
        ;
        ;Work out top of memory.
        ;
        push    esi

Big1Check2:
        mov     ah,88h          ; get top of extended memory
        cmp     Big1Flag,0      ; see if using alternate 0e801h means to get memory
        je      use88h2
        mov     ax,0e801h
use88h2:
        mov     bl,15h
        mov     edi,offset MemIntBuffer
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateInt

        test    BYTE PTR RealRegsStruc.Real_Flags[edi],1        ; see if carry returned
        je      GIProcess2      ; nope
        cmp     Big1Flag,0
        je      GIProcess2      ; not using alternate extended memory, process anyway
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check2      ; and retry

GIProcess2:
        cmp     Big1Flag,0
        je      use88hResult2   ; use results from 88h function

; using results from 0e801h function
        movzx   eax,WORD PTR RealRegsStruc.Real_EAX[edi]
        movzx   ebx,WORD PTR RealRegsStruc.Real_EBX[edi]
        push    eax
        or      eax,ebx         ; see if ax=bx=0, if so use cx,dx return instead
        pop     eax
        jne     GIComputeBig1Size2
        movzx   eax,WORD PTR RealRegsStruc.Real_ECX[edi]
        movzx   ebx,WORD PTR RealRegsStruc.Real_EDX[edi]
        push    eax
        or      eax,ebx         ; see if cx=dx=0, if so use regular 88h function
        pop     eax
        jne     GIComputeBig1Size2
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check2      ; and retry

GIComputeBig1Size2:
        shl     ebx,6           ; 64K chunks to 1K
        add     eax,ebx         ; add to 1K chunks below 64M
        jmp     GIComputeBytes2

use88hResult2:
        movzx   eax,WORD PTR RealRegsStruc.Real_EAX[edi]

GIComputeBytes2:
        shl     eax,10          ; * 1024
        add     eax,100000h     ;add in 1 meg base address.
        dec     eax
;        and     eax,0FFFFFFFFh-4095     ;round down to nearest page.
        and     ax,0F000h       ;round down to nearest page.
        mov     ebx,eax
        pop     esi
        pop     edi
        ;
        ;ESI - base.
        ;EBX - limit.
        ;
        cmp     esi,ebx
        jnc     rv56_9          ;No more available.
        mov     ecx,ebx
        sub     ecx,esi         ;block size.
        cmp     ecx,4096                ;check enough for 1 page.
        jc      rv56_9
        ;
        pushad
        cmp     Int15Size,0             ;set size yet?
        jnz     rv56_GotSize
        mov     eax,ecx         ;get proposed maximum size.
        mov     ecx,8           ;number of chunks.
        xor     edx,edx
        div     ecx             ;get chunk size.
        inc     eax
        or      Int15Size,-1            ;set chunk size to use.
;        and     eax,not 4095
        and     ax,0f000h
        jz      rv56_GotSize
        mov     Int15Size,eax   ;set chunk size to use.
rv56_GotSize:
        popad
        cmp     ecx,Int15Size
        jc      rv56_SizeOK
        cmp     ExtALLSwitch,0
        jnz     rv56_SizeOK
        mov     ecx,Int15Size
rv56_SizeOK:

        mov     eax,ecx
        add     eax,4095
        shr     eax,12
        cmp     eax,MaxMemPhys
        jc      rv56_nomaxlimit
        mov     ecx,MaxMemPhys
        shl     ecx,12
rv56_nomaxlimit:

        sub     ebx,ecx         ;new int 15 value.
        ;
        ;EDI -> Int15Table[] 
        ;EBX - base.
        ;ECX - size.
        ;
        mov     [edi+0],ebx     ;store base address.
        mov     [edi+4],ebx
        add     [edi+4],ecx     ;set end address.
        ;
        movzx   esi,ILevel
if 0
        shl     esi,3           ; assumes sizeof ITABSTRUC = 8
        add     esi,offset ITable
else
        lea     esi,[esi*sizeof ITABSTRUC+offset ITable] ;will cause an error if sizeof ITABSTRUC isn't 1/2/4/8
endif
;       dec     ebx             ;move back to previous byte.
        sub     ebx,100000h     ;remove starting point.
        shr     ebx,10          ;convert to K.
        mov     [esi].ITABSTRUC.dwMemSiz,ebx ;set new base value.
        mov     bl,15h*4
        movzx   ebx,bl
        push    es
        mov     ax,KernalZero
        mov     es,eax
        mov     eax,es:[ebx]
        mov     [esi].ITABSTRUC.dwOldVec,eax ;store old vector.
        movzx   edx,ILevel
        mov     dx,[Int15PatchTable+edx*2]
        mov     cx,GROUP16      ;segment to use.
        pushfd
        cli
        mov     es:[ebx+0],dx
        mov     es:[ebx+2],cx
        popfd
        pop     es
        inc     ILevel          ;move to next level.
        jmp     rv56_3
        ;
rv56_GotOne:
        ;Update table entry and exit.
        ;
        mov     edx,[esi+0]             ;get base address.
        add     d[esi],4096             ;move pointer along.
        xor     ecx,ecx
        clc
        jmp     rv56_10
        ;
rv56_9: stc
        ;
rv56_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
GetINT15Page    endp


;-------------------------------------------------------------------------------
GetINT15Pages   proc    near
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     [INT15hTotal],0
        ;
        cmp     ProtectedType,PT_VCPI
        jnc     rv57_9
        cmp     XMSPresent,0
        jnz     rv57_9
        ;
        ;Setup initial simulated int 15 values.
        ;

Big1Check1:
        mov     ah,88h          ; get top of extended memory
        cmp     Big1Flag,0      ; see if using alternate 0e801h means to get memory
        je      use88h1
        mov     ax,0e801h
use88h1:
        mov     bl,15h
        mov     edi,offset MemIntBuffer
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateInt

        test    BYTE PTR RealRegsStruc.Real_Flags[edi],1        ; see if carry returned
        je      GIProcess1      ; nope
        cmp     Big1Flag,0
        je      GIProcess1      ; not using alternate extended memory, process anyway
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check1      ; and retry

GIProcess1:
        cmp     Big1Flag,0
        je      use88hResult1   ; use results from 88h function

; using results from 0e801h function
        movzx   eax,RealRegsStruc.Real_AX[edi]
        movzx   ebx,RealRegsStruc.Real_BX[edi]

        push    eax
        or      eax,ebx         ; see if ax=bx=0, if so use cx,dx return instead
        pop     eax
        jne     GIComputeBig1Size1
        movzx   eax,RealRegsStruc.Real_CX[edi]
        movzx   ebx,RealRegsStruc.Real_DX[edi]
        push    eax
        or      eax,ebx         ; see if cx=dx=0, if so use regular 88h function
        pop     eax
        jne     GIComputeBig1Size1
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check1      ; and retry

GIComputeBig1Size1:
        shl     ebx,6           ; 64K chunks to 1K
        add     eax,ebx         ; add to 1K chunks below 64M
        jmp     GIComputeBytes1

use88hResult1:
        movzx   eax,RealRegsStruc.Real_AX[edi]

GIComputeBytes1:

;       mov     w[Int15hValue],ax
        mov     [Int15hValue],eax

        mov     ax,ILevel
        mov     [INT15hLevel2],ax
        ;
rv57_0: ;Need to get another block of memory.
        ;
        cmp     [INT15hLevel2],INT15ITEMS
        jnc     rv57_1          ;can't cope with any more.
        mov     ax,KernalZero
        mov     es,eax
        movzx   edi,WORD PTR es:[(VDiskSigVec*4)+2]
        shl     edi,4
        mov     eax,100000h             ;Start of extended memory.
        add     edi,VDiskSigOffs
        mov     esi,offset VDiskSig
        mov     cx,VDiskSigLen
        push    edi
        cld
        repe    cmpsb
        pop     edi
        jne     rv57_GotBottom
        xor     eax,eax
        mov     al,es:[edi+VDiskHiOffs]
        shl     eax,16
        mov     ax,es:[edi+VDiskLoOffs]
rv57_GotBottom:
        mov     esi,eax
        ;
        ;Work out top of memory.
        ;
;       push    esi

;       movzx   eax,w[Int15hValue]      ;get pretend value.
        mov     eax,[Int15hValue]       ;get pretend value.

        shl     eax,10          ; * 1024
        add     eax,100000h             ;add in 1 meg base address.
        dec     eax
;        and     eax,0FFFFFFFFh-4095     ;round down to nearest page.
        and     ax,0F000h       ;round down to nearest page.
        mov     ebx,eax
;       pop     esi

        ;
        ;ESI - base.
        ;EBX - limit.
        ;
        cmp     esi,ebx
        jnc     rv57_1          ;No more available.
        mov     ecx,ebx
        sub     ecx,esi         ;block size.
        cmp     ecx,4096                ;check enough for 1 page.
        jc      rv57_1
        ;
        pushad
        cmp     Int15Size,0             ;set size yet?
        jnz     rv57_GotSize
        mov     eax,ecx         ;get proposed maximum size.
        mov     ecx,8           ;number of chunks.
        xor     edx,edx
        div     ecx             ;get chunk size.
        mov     Int15Size,-1            ;default to maximum.
        cmp     eax,4096                ;too small?
        jc      rv57_GotSize
        mov     Int15Size,eax   ;set chunk size to use.
rv57_GotSize:
        popad
        cmp     ecx,Int15Size
        jc      rv57_SizeOK
        mov     ecx,Int15Size
rv57_SizeOK:
        sub     ebx,ecx         ;new int 15 value.
        ;
        ;EBX - base.
        ;ECX - size.
        ;
        shr     ecx,12          ;get number of pages.
        add     [INT15hTotal],ecx
        ;
        dec     ebx             ;move back to previous byte.
        sub     ebx,100000h             ;remove starting point.
        shr     ebx,10          ;convert to K.

;       mov     w[Int15hValue],bx       ;set new base value.
        mov     [Int15hValue],ebx       ;set new base value.

        inc     [INT15hLevel2]  ;move to next level.
        jmp     rv57_0
        ;
rv57_1: ;Now include any remains of existing blocks.
        ;
        mov     esi,offset Int15Table
        mov     ecx,INT15ITEMS
rv57_2:
        mov     eax,[esi+0]
        cmp     eax,0
        jz      rv57_3
        add     eax,4096
        cmp     eax,[esi+4]
        jnc     rv57_3
        mov     eax,[esi+4]
        sub     eax,[esi+0]
        shr     eax,12          ;free pages remaining
        dec     eax
        add     [INT15hTotal],eax
rv57_3:
        add     esi,4+4
        loop    rv57_2
        @dprintf DOPT_PHYSMEM,<"GetINT15Pages: %lu",10>,INT15hTotal
        ;
rv57_9:
        mov     edx,[INT15hTotal]
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
GetINT15Pages   endp


ifndef NOLOWMEM
;-------------------------------------------------------------------------------
;
;Attempt to allocate another page of conventional memory.
;
GetCONVPage     proc    near
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        cmp     ConvSaveSize,-1
        jz      rv58_9
        ;
        mov     ConvSavePara,0
        cmp     ConvSaveSize,0
        jz      rv58_200
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4800h     ;allocate memory block.
        mov     ax,ConvSaveSize
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_Flags[edi],1
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        jnz     rv58_9          ;if not enough for user buffer then don't allocate any more.
        mov     ConvSavePara,ax ;store para we saved.
rv58_200:
rv58_3:
        mov     esi,offset ConventionalList
        xor     edi,edi
        mov     ecx,CONVITEMS
rv58_0:
        cmp     [esi].DOSMEMITEM.wAddr,0     ;This entry in use?
        jz      rv58_1
        push    ecx
        push    esi
        push    edi
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4a00h     ;resize memory block.
        mov     ax,[esi].DOSMEMITEM.wAddr               ;get real mode segment
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     ax,[esi].DOSMEMITEM.wSize               ;get current size.
        add     ax,4096/16
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        test    es:RealRegsStruc.Real_Flags[edi],1
        pop     edi
        pop     esi
        pop     ecx
        jz      rv58_GotOne
        ;
        ;Make sure block goes back to original size.
        ;
        push    ecx
        push    esi
        push    edi
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4a00h     ;resize memory block.
        mov     ax,[esi].DOSMEMITEM.wAddr               ;get real mode segment
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     ax,[esi].DOSMEMITEM.wSize               ;get current size.
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set new size desired.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        pop     edi
        pop     esi
        pop     ecx
        jmp     rv58_2
        ;
rv58_1:
        or      edi,edi         ;already got a free entry?
        jnz     rv58_2
        mov     edi,esi
rv58_2:
        add     esi,sizeof DOSMEMITEM    ;next entry.
        dec     ecx
        jnz     rv58_0
        ;
        ;Need to allocate a new block.
        ;
        mov     esi,edi         ;get free entry number.
        or      esi,esi
        jz      rv58_9
        push    esi
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4800h     ;allocate memory block.
        mov     es:RealRegsStruc.Real_BX[edi],1         ;set new size desired.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        pop     esi
        test    es:RealRegsStruc.Real_Flags[edi],1
        jnz     rv58_9
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        mov     [esi].DOSMEMITEM.wAddr,ax               ;store it in the table.
        movzx   eax,ax
        shl     eax,4           ;linear address.
        mov     ebx,eax
        add     eax,4095
;        and     eax,0ffffffffh-4095     ;round up to next page.
        and     ax,0f000h       ;round up to next page.
        sub     eax,ebx
        shr     eax,4
        mov     [esi].DOSMEMITEM.wSize,ax   ;store new size.
        jmp     rv58_3          ;start again.
        ;
rv58_GotOne:
        ;Update table entry indicated and return physical address.
        ;
        movzx   eax,[esi].DOSMEMITEM.wAddr ;Get block base segment.
        add     ax,[esi].DOSMEMITEM.wSize  ;Add old length.
        add     [esi].DOSMEMITEM.wSize,4096/16 ;update length.
        shl     eax,4           ;linear address.
        shr     eax,12          ;get page number.
        mov     bx,KernalZero
        mov     es,bx
        mov     esi,PTMAPADDR           ;base of page alias's.
        mov     eax,es:[esi+eax*4]      ;get physical address.
;        and     eax,0ffffffffh-4095     ;lose user bits.
        and     ax,0f000h       ;lose user bits.
        mov     edx,eax
        ;
        xor     ecx,ecx
        clc
        jmp     rv58_10
        ;
rv58_9: stc
        ;
rv58_10:
        pushfd
        cmp     ConvSavePara,0  ;did we save any memory?
        jz      rv58_100
        pushad
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4900h     ;release memory block.
        mov     ax,ConvSavePara
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        popad
        mov     ConvSavePara,0
rv58_100:
        popfd
        ;
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
GetCONVPage     endp


;-------------------------------------------------------------------------------
;
;Determine how many pages of conventional memory could be allocated.
;
GetCONVPages    proc    near
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        mov     [ConvTotal],0
        ;
        cmp     ConvSaveSize,-1
        jz      rv59_9
        ;
        mov     ConvSavePara,0
        cmp     ConvSaveSize,0
        jz      rv59_200
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4800h     ;allocate memory block.
        mov     ax,ConvSaveSize
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_Flags[edi],1
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        jnz     rv59_9          ;if not enough for user buffer then don't allocate any more.
        mov     ConvSavePara,ax ;store para we saved.
rv59_200:
        mov     edi,offset ConvTempList
        mov     ecx,CONVITEMS
        xor     eax,eax
        push    ds
        pop     es
        cld
        rep     stosw
        ;
        mov     ecx,CONVITEMS
        mov     esi,offset ConvTempList
rv59_0:
        push    ecx
        push    esi
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4800h      ;allocate memory block.
        mov     es:RealRegsStruc.Real_BX[edi],-1         ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        pop     esi
        pop     ecx
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        cmp     bx,(4096*2)/16
        jc      rv59_2
        push    ebx
        push    ecx
        push    esi
        mov     es:RealRegsStruc.Real_AX[edi],4800h      ;allocate memory block.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        pop     esi
        pop     ecx
        pop     ebx
        test    es:RealRegsStruc.Real_Flags[edi],1
        jnz     rv59_2
        ;
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [esi],ax        ;store segment address.
        movzx   eax,ax
        shl     eax,4           ;linear address.
        movzx   ebx,bx
        shl     ebx,4
        add     ebx,eax         ;linear limit.
        add     eax,4095
        shr     eax,12          ;round up to next page.
        shr     ebx,12          ;round down to next page.
        sub     ebx,eax
        js      rv59_1
        add     [ConvTotal],ebx
rv59_1:
        add     esi,2
        dec     ecx
        jnz     rv59_0
        ;
rv59_2: ;Now release all memory blocks again.
        ;
        mov     ecx,CONVITEMS
        mov     esi,offset ConvTempList+((CONVITEMS-1)*2)
rv59_3:
        push    ecx
        push    esi
        cmp     w[esi],0
        jz      rv59_4
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4900h     ;release memory block.
        mov     ax,[esi]
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
rv59_4:
        pop     esi
        pop     ecx
        sub     esi,2
        dec     ecx
        jnz     rv59_3
        ;
        ;Ask for big block to try and improve cleanup process.
        ;
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4800h     ;allocate memory block.
        mov     es:RealRegsStruc.Real_BX[edi],-1        ;set new size desired.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        ;
        ;Now return pages found.
        ;

;       mov     edx,[ConvTotal]
        ;
rv59_9:
        pushfd
        mov     edx,[ConvTotal]        ; moved, MED, 11/15/99

        cmp     ConvSavePara,0  ;did we save any memory?
        jz      rv59_100
        pushad
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     es:RealRegsStruc.Real_AX[edi],4900h     ;release memory block.
        mov     ax,ConvSavePara
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
;        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt
        popad
        mov     ConvSavePara,0
rv59_100:
        popfd
        ;
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret

GetCONVPages    endp
endif

;-------------------------------------------------------------------------------
;
;Build a segment descriptor.
;
;On Entry:-
;
;ES:EDI - Descriptor entry to use.
;ESI    - Linear base to set.
;ECX    - limit in bytes.
;AL     - Code size bit.
;AH     - Present/PL/memory|system/type bits.
;
EMUMakeDesc     proc    near
        pushad
        and     edi,not 7               ;lose RPL & TI
        cmp     ecx,0100000h    ; see if we need to set g bit
        jc      rv60_0
        shr     ecx,12          ; div by 4096
        or      al,80h          ; set g bit
rv60_0: mov     es:[edi],cx             ;store low word of limit.
        shr     ecx,16
        or      cl,al
        mov     es:[edi+6],cl   ;store high bits of limit and gran/code size bits.
        mov     es:[edi+2],si   ;store low word of linear base.
        shr     esi,16
        mov     bx,si
        mov     es:[edi+4],bl   ;store mid byte of linear base.
        mov     es:[edi+7],bh   ;store high byte of linear base.
        mov     es:[edi+5],ah   ;store pp/dpl/dt/type bits.
        popad
        ret
EMUMakeDesc     endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Emulate INT 2Fh DPMI related functions.
;
Raw2FPatch      proc    near
        cmp     ax,1687h       ; that's actually a real-mode API only!
        jz      rv64_DoneC
        cmp     ax,1686h
        jnz     rv64_NotOurs
        xor     ax,ax
        jmp     rv64_Done
rv64_DoneC:
        stc
        ;
rv64_Done:
        ;Now update stacked flags.
        ;
        push    eax
        pushfd
        pop     eax                             ;get new flags.
        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv64_Use32Bit8
        push    ebp
        movzx   ebp,sp
        mov     b[ebp+2*4].IRET16._fl, al       ;update low(fl)
        pop     ebp
        pop     eax
        iret
rv64_Use32Bit8:
        mov     b[esp+4].IRET32._fl,al          ;update low(efl)
        pop     eax
        iretd
        ;
rv64_NotOurs:
        ;Not a function recognised by us so pass control to previous handler.
        ;
        jmp     cs:[OldInt2Fp]                  ;pass it onto previous handler.
;

_cwDPMIEMU$1 segment
	align 4
OldInt2Fp label fword
        dd offset IntNN386Catch+(2fh*INTNN386SIZE)
        dw DpmiEmuCS
_cwDPMIEMU$1 ends

Raw2FPatch      endp

;--- protected-mode debugger API int 41h
;--- just ensure that the interrupt isn't routed to real-mode

Raw41Patch      proc    near

        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      @F
        iret
@@:
        iretd

Raw41Patch      endp

