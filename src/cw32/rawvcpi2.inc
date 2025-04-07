
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;32-bit segment of RAW/VCPI stuff. This installs the basic DPMI emulation
;code: mode switches, physical memory. The rest of the DPMI emulation is:
; - interrup.inc  interrupt/exception handlers; get/set int/exc vectors
; - ldt.inc       LDT management
; - memory.inc    extended memory management; map/unmap physical memory
; - dosmem.inc    DOS memory management
;All of this gets moved into extended memory during initialisation to keep
;conventional memory foot print down.
;
;Known issues:
; 1. Int 15h memory management (used if VCPI/XMS isn't available) is very slow
;    unless CAUSEWAY=BIG1 is set.

USEPMFLAGS equ 0	;1=don't use rmcs flags for rm flags setup

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

	align 4
; byte table for 256 interrupts
; it's used to translate IRQs and INT 1C, 23h, 24h to an internal callback#
Int2CallCheck label byte
    db 256 dup (-1)
    org Int2CallCheck + 08h
    db 0,1, 2, 3, 4, 5, 6, 7 ;08-0F -> cb 0-7
    org Int2CallCheck + 70h
    db 8,9,10,11,12,13,14,15 ;70-77 -> cb 8-15
    org Int2CallCheck + 1Ch
    db 16                    ;   1C -> cb 16
    org Int2CallCheck + 23h
    db 17,18                 ;23-24 -> cb 17-18
    org Int2CallCheck + 100h
;
;-------------------------------------------------------------------------------
;
;Simulate real mode interrupt.
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
;Simulate real mode interrupt. int 31h, ax=300h
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;BL     - Interrupt number.
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
;Simulate real mode far call. Int 31h, ax=301h
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
;Simulate real mode far call with IRET stack frame. Int 31h, ax=302h
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

; field SS:SP in RMCS is NOT modified by this function.

; A CauseWay peculiarity is that the DPMI emulator works
; on the application stack. That's also true for the real-mode
; code support functions 0x300-0x302. To be safe, assume 128
; bytes below (E)SP to be used by the emulator.

;--- stack frame

RV29FR struct
    dd ?    ; RawStackPos
    dd ?    ; rv29_rmsp
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

        mov     ax,KernalDS     ;make our data addresable.
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     fs,eax
        push    d[rv29_rmsp]
        push    [RawStackPos]
;
;setup the real mode stack.
;
        mov     eax,es:[edi].RealRegsStruc.Real_SSSP
        shld    esi,eax,16
        or      eax, eax        ;rm stack supplied?
        jnz     rv29_GotStack
;
;Caller isn't supplying a stack so we will.
;
        mov     eax,RawStackPos
        sub     RawStackPos,RawStackDif ;update for re-entry.
        mov     si,RawStackReal
;
;Point to the real mode stack.
;
rv29_GotStack:
        movzx   eax,ax
        movzx   esi,si
        sub     eax,(4+4)+(4+4)		; make room for pm ss:esp and rmcs es:edi
        mov     [rv29_rmsp],ax
        mov     edx,esi             ; save rm SS in DX
        shl     esi,4
        add     esi,eax             ; ESI = linear address rm SS:SP

;--- struct of real-mode stack frame

;dd cs:ip     ;from IVT or rmcs.CS:IP
;dw flags     ;bh=1 only
;dw rv29_behindcall, GROUP16
;dw cx dup (?) 
;dq pm ss:esp
;dq rmcs      ;es:edi
;
;Store current stack pointer on v86 stack.
;
        mov     eax,esp
        test    BYTE PTR SystemFlags,SF_16BIT
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
        movzx   ecx,cx
        sub     esi,ecx
        sub     esi,ecx
        push    esi
        push    eax
@@:
        mov     ax,[ebp]
        mov     fs:[esi],ax
        add     ebp,2
        add     esi,2
        loop    @B
        pop     eax
        pop     esi
rv29_NoStacked:
        mov     ebp,eax
if USEPMFLAGS
        mov     eax,[ebp].RV29FR.wFlgs
else
        mov     ax,es:[edi].RealRegsStruc.Real_Flags
endif
        cmp     bh,1             ;call far proc with RETF frame?
        jz      rv29_noflagscopy
;                                ;for sim int and sim fcall with IRET frame...
        sub     esi,2
        mov     fs:[esi],ax      ;put flags onto the rm stack.
rv29_noflagscopy:
        sub     esi,4            ;put "caller" CS:IP onto the rm stack.
        mov     word ptr fs:[esi+0],offset rv29_behindcall
        mov     word ptr fs:[esi+2],GROUP16
        or      bh,bh            ;simulate int?
        jz      rv29_IsInt
        mov     ecx,es:[edi].RealRegsStruc.Real_CSIP
        jmp     rv29_NotInt
        ;
rv29_IsInt:
        and     ah,11111100b   ;clear TF and IF
        mov     es:[edi].RealRegsStruc.Real_Flags,ax
        ;See if this is a busy interrupt call back.
        ;
        movzx   ebx,bl
        mov     ecx,ebx
        mov     al,[Int2CallCheck][ebx]
        or      al,al
        js      rv29_c3
        mov     bl,al               ;ebx = callback#
        bt      [ICallBackBusy],ebx ;busy?
        jnc     rv29_c3
        mov     ecx,[ebx*4][ICallBackOldVec]
        jmp     rv29_c2
rv29_c3:
        mov     ecx,fs:[ecx*4]  ;Get IVT vector
rv29_c2:
rv29_NotInt:
        sub     esi,4
        mov     fs:[esi],ecx    ;put CS:IP onto the rm stack
;
;Copy register values onto real mode stack (es:edi -> fs:esi)
;8*4 = PUSHAD
;2   = Flags
;4*2 = seg regs DS,ES,FS,GS
;
        sub     esi,8*4+5*2
        push    esi
        push    ds

        xchg    esi,edi
        push    es
        push    fs
        pop     es
        pop     ds
        mov     ecx,(8*4+4*2)/4
        rep     movsd [edi],[esi]
        movsw   [edi],[esi]

        pop     ds
        pop     esi
;
;Get real-mode ss:sp values into DX:CX
;
        mov     ecx,edx
        mov     eax,ecx
        shl     eax,4
        mov     edx,esi       ;linear address rm SS:SP
        sub     edx,eax       ;subtract linear address (rm SS shl 4)
;
;Switch to real/v86 mode.
;
        push    offset rv29_contrm
        jmp     [fProtected2Real]

_cwRaw segment
rv29_contrm:
;
;Fetch registers off the stack and "return" to rm address.
;
        assume ds:nothing
        popad
        popf
        pop     es
        pop     ds
        pop     fs
        pop     gs
        retf
rv29_behindcall:
;
;Switch back to old stack.
;
        mov     sp,cs:[rv29_rmsp]
;
;Save all registers.
;
        push    gs
        push    fs
        push    ds
        push    es
        pushf
        pushad
;
;Make our data addressable again and store stack values.
;
        mov     ax,cs
        mov     ds,ax
        assume ds:GROUP16
        mov     bx,ss
        mov     bp,sp
;
;Retrieve protected mode stack address.
;8*4=pushad, 5*2=push segeregs + flags
;
        mov     edx,d[bp+8*4+5*2+0]
        mov     cx, w[bp+8*4+5*2+4]
;
;switch back to protected mode.
;
        cli
        push    DpmiEmuCS
        push    lowword offset rv29_contpm
        jmp     [Real2Protected]
;
_cwRaw ends

rv29_contpm:
        pop     [RawStackPos]
        pop     d[rv29_rmsp]

        mov     ax,KernalZero   ;/
        mov     ds,eax
;
;Retrieve v86 stack address.
;
        movzx   esi,bx
        shl     esi,4
        movzx   eax,bp
        add     esi,eax
;
;Retrieve table address.
;
        les     edi,[esi+8*4+5*2+2*4]
;
;Copy new register values into table.
;
        cld
        mov     ecx,(8*4+4*2)/4      ;copy just the 8 regs and fl,es,ds,fs
        rep     movsd
        movsw                        ;and finally gs
        ;
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        assume ds:nothing
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

STACKADJ equ 8*4+4*2+2 ;pushad, push segregs,pushf

RawCallBack     proc    near
        pushf
        push    cs
        db      0eah
        dw      @F,GROUP16
@@:
        ;
        ;Check if Windows enhanced mode has been started.
        ;
        cmp     cs:InWindows,0
        jz      rv30_Normal
        add     sp,2
        popf
        retf
        ;
rv30_Normal:
        cli
        pop     cs:[rv30_CallTab]
        ;
        push    gs
        push    fs
        push    ds
        push    es
        pushad
        mov     ax,cs
        mov     ds,ax           ;make our data addresable.
        assume  ds:GROUP16
        mov     [rv30_rmsp+0],sp
        mov     [rv30_rmsp+2],ss
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
        movzx   ebx,[rv30_CallTab]
        sub     bx,GROUP16
        imul    ebx,sizeof CallBackStruc
        add     ebx,offset CallBackTable;get callback

        movzx   eax,[rv30_rmsp+0]
        movzx   esi,[rv30_rmsp+2]       ;point to stacked registers.
        shl     esi,4
        add     esi,eax
        mov     ax,KernalZero   ;/
        mov     fs,eax
        ;
        les     edi,[ebx].CallBackStruc.CallBackRegs  ;get register structure.
        mov     edx,edi
        mov     ecx,8
        cld
        rep     movsd es:[edi],fs:[esi]
        mov     ax,fs:[esi+4*2]
        stosw                       ;flags
        movsd   es:[edi],fs:[esi]   ;ES,DS
        movsd   es:[edi],fs:[esi]   ;FS,GS

        add     esi,2               ;skip flags
        movsd                       ;CS:IP
        mov     eax,d[rv30_rmsp]
        add     ax,STACKADJ
        stosd                       ;SS:SP
        mov     edi,edx
        ;
        test    BYTE PTR SystemFlags,SF_16BIT
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
        ;Setup stack descriptor
        ;
        movzx   ebx,[ebx].CallBackStruc.CallBackStackSel
        push    ebx

        movzx   esi,es:RealRegsStruc.Real_SP[edi]
        movzx   eax,es:RealRegsStruc.Real_SS[edi]
        shl     eax,4
        and     ebx,not 7
        add     ebx,d GDTVal+2
        mov     fs:[ebx].Desc.Base_l,ax
        shr     eax,16
        mov     fs:[ebx].Desc.Base_m,al
        mov     fs:[ebx].Desc.Base_H,ah

        test    BYTE PTR SystemFlags,SF_16BIT
        pop     ds              ;load DS with stack sel
        assume ds:nothing
        pushfd                  ;push a dword to keep stack aligned even for 16-bit apps
        jz      rv30_Use32Bit13
        db 66h                  ;change "fword ptr" to "PF16 ptr"
rv30_Use32Bit13:
        call    fword ptr [ebp]
        cli
        ;
        ; copy the registers from RMCS in es:[edi]
        ; onto real-mode stack (then "restored" with POPAD, POP xx below)
        ;
        movzx   esi,es:RealRegsStruc.Real_SS[edi]       ;point to stacked registers.
        mov     ebp,esi            ;BP=realmode SS
        shl     esi,4
        movzx   eax,es:RealRegsStruc.Real_SP[edi]
        sub     eax,8*4+4*2+2+2*2  ;size PUSHAD, 4 seg regs, flags, CS:IP
        mov     edx,eax            ;DX=rmSP
        add     esi,eax
        xchg    esi,edi
        mov     eax,es
        mov     ds,eax             ;now src (DS:ESI) = callback's RMCS
        mov     ax,KernalZero
        mov     es,eax
        mov     ecx,8
        cld
        rep     movsd              ;8 GPRs
        lodsw                      ;flags
        movsd                      ;ES,DS
        movsd                      ;FS,GS
        movsd                      ;CS:IP
        stosw                      ;flags
        ;
        ;Switch back to real/v86 mode.
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        add     [RawStackPos],RawStackDif
        mov     ecx,ebp            ;CX=rmSS
        push    offset rv30_contrm
        jmp     [fProtected2Real]
_cwRaw segment
rv30_contrm:
        popad
        pop     es
        pop     ds
        pop     fs
        pop     gs
        iret
        ;
RawCallBack     endp

;-------------------------------------------------------------------------------
;--- internal callback for IRQs, INTs 1Ch/23h/24h;
;--- these callbacks have no associated RMCS.

RawIntCallBack    proc    near

        push    bp
        mov     bp,cs
        db      0eah
        dw      @F,GROUP16
@@:
        push    ax
        mov     ax,cs
        sub     bp,ax
        ;
        ;disable IRQ handling if Windows enhanced mode is running
        ;
        cmp     cs:InWindows,0
        jnz     rv31_ForceOld
        ;
        ;Check if this call back is busy.
        ;
        bts     w cs:[ICallBackBusy],bp  ;call back busy?
        jnc     rv31_NotBusy
rv31_ForceOld:
        ;
        ;This is a busy int call back so pass control to old real mode vector.
        ;The reason for this restriction is to avoid IRQs to enter a "routing"-loop.
        ;
        @dprintf16 DOPT_MODESW,<"RawIntCallBack: forced jmp to old vector",10>
        push    dx
        shl     bp,2
        mov     ax,w cs:[bp][ICallBackOldVec+0]
        mov     dx,w cs:[bp][ICallBackOldVec+2];fetch old real mode vector address.
        mov     bp,sp
        xchg    ax,[bp+2]
        xchg    dx,[bp+4]
        mov     bp,dx
        pop     dx
        retf                ;pass onto old handler.
        ;
        ;
rv31_NotBusy:
        mov     cs:[rv31_CallTab],bp   ;callback#
        mov     bp,sp
        mov     ax,[bp+2*2].IRET16._fl
;        and     ah,00001100b               ; clear TF,IF,IOPL,NT???
        and     ah,00111111b               ; clear NT
        mov     WORD PTR cs:[rv31_FlagsStore+0],ax
        pop     ax
        pop     bp
        ;
        ;Switch to new stack.
        ;
        mov     w cs:[rv31_rmsp+0],sp  ;store current stack.
        mov     w cs:[rv31_rmsp+2],ss
        mov     ss,cs:RawStackReal         ;use new stack.
        mov     esp,cs:RawStackPos
        sub     cs:RawStackPos,RawStackDif
        push    cs:[rv31_rmsp]         ;put old stack onto new one.
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

        mov     ax,cs
        mov     ds,ax
        assume ds:GROUP16
        mov     w[rv31_rmsp+0],sp
        mov     w[rv31_rmsp+2],ss
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
        movzx   ebx,[bx][ICallBackIntNo]   ;get int number.
        shl     ebx,1           ;*2
        lea     ebx,[ebx*2+ebx] ;*6
        add     ebx,offset InterruptTable
        mov     edx,cs:[ebx+0]             ;get offset.
        mov     cx,cs:[ebx+4]              ;get segment selector.
        test    BYTE PTR SystemFlags,SF_16BIT
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
        movzx   esi,w[rv31_rmsp+2]
        shl     esi,4
        movzx   eax,w[rv31_rmsp+0]
        add     esi,eax         ;Point to stacked registers.

        pushfd
        add     esp,2
        pop     w[rv31_FlagsStore+2]  ;save hiword(efl)

        mov     eax,es:[esi+32]
        mov     ebx,es:[esi+28]
        mov     ecx,es:[esi+24]
        mov     edx,es:[esi+20]
        mov     edi,es:[esi+12]
        mov     ebp,es:[esi+8]
        mov     esi,es:[esi+16]
        push    [rv31_rmsp]
        push    [rv31_CallTab]     ;that's a WORD only!!!!
        db 66h                     ;align the stack
        push    ds

        test    BYTE PTR SystemFlags,SF_16BIT
        jz      rv31_Use32Bit13

;--- todo: explain the "dummy" (d)words on the stack

        push    w[rv31_FlagsStore+0] ;fl dummy
        db 66h                     ;push CS as word!
        push    cs                 ;cs dummy
        pushw   0                  ;ip dummy
        push    w[rv31_FlagsStore+0]
        call    [rv31_CallB016]    ;far16 call - IRET frame
        lea     esp,[esp+(3*2)]    ;skip the extra words
        jmp     rv31_Use16Bit13
rv31_Use32Bit13:
        push    [rv31_FlagsStore]  ;efl dummy
        push    cs                 ; cs dummy
        pushd   0                  ;eip dummy
        push    [rv31_FlagsStore]
        call    [rv31_CallB032]    ;far32 call - IRETD frame
        lea     esp,[esp+(3*4)]    ;skip the extra dwords
rv31_Use16Bit13:
        db 66h                     ;needed for stack alignment
        pop     ds

        pushfd
        cli
        @dprintf DOPT_MODESW,<"RawIntCallBack: back from pm irq proc",10>
        pop     [rv31_FlagsStore]
        pop     [rv31_CallTab]
        pop     [rv31_rmsp]

        push    esi
        push    eax
        mov     ax,KernalZero
        mov     es,eax
        movzx   esi,w[rv31_rmsp+2]
        shl     esi,4
        movzx   eax,w[rv31_rmsp+0]
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
        ;todo: explain why some flags are modified here
        ;
        movzx   eax,WORD PTR es:[esi+4*2+7*4+0]  ;old real-mode SP
        movzx   esi,WORD PTR es:[esi+4*2+7*4+2]  ;old real-mode SS
        shl     esi,4
        add     esi,eax
        mov     ax,w[rv31_FlagsStore+0]
        and     ah,11111000b           ;clear TF, IF & DF
        and     es:[esi].IRET16._fl,0000011100000000b
        or      es:[esi].IRET16._fl,ax
        ;
        mov     bx,[rv31_CallTab]      ;restore call back structure.
        btr     w [ICallBackBusy],bx   ;clear busy flag.
        ;
        ;Switch back to real/v86 mode.
        ;
        mov     dx,w[rv31_rmsp+0]
        mov     cx,w[rv31_rmsp+2]
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
RawIntCallBack    endp

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
;--- create an alias descriptor of selector in BX; return new descriptor in AX
        push    ebx
        push    eax
        push    edi
        push    es
        mov     edi,offset RawSelBuffer
        mov     ax,KernalDS
        mov     es,eax
        call    RawBGetDescriptor       ;get src descriptor
        jc      rv46_000A_err
;
        push    ecx
        mov     cx,1
        call    RawGetDescriptors
        pop     ecx
        jc      rv46_000A_err
        mov     BYTE PTR es:[edi].Desc.Access,DescPresent+DescPL3+DescMemory+DescRWData
        mov     ebx,eax
        call    RawBPutDescriptor       ;set new descriptor details.
rv46_000A_err:
        pop     es
        pop     edi
        jc      rv46_000A_err2
        pop     ebx
        mov     bx,ax
        mov     eax,ebx
        pop     ebx
        ret
rv46_000A_err2:
        pop     eax
        pop     ebx
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
        call    RawReleaseDOSMemory
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
;--- ESP: (4*4) = ebp, edi, eax, offset rv46_done
;--- SS:EBP -> stack params
        lea     ebp,[esp+(4*4)+sizeof IRET32]
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      rv46_030x_0a
        movzx   edi,di
        movzx   ebp,bp
        sub     ebp,sizeof IRET16
rv46_030x_0a:
        call    eax
if 0 ;v5.1: removed
        jc      rv46_030x_9
;
;Mask real mode register structure flags.
;
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
endif
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
;rv46_DPMI_0701:
;rv46_DPMI_0702:
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
rv46_DPMI_0900:                 ;enable/disable/get virtual interrupts.
        cmp     al,3
        jae     rv46_NotOurs
        push    ebp
        lea     ebp,[esp+2*4+2*4]
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      rv46_1
        movzx   ebp,bp
        sub     ebp,2*2
rv46_1:
        cmp     al,01h
        ja      rv46_0902
        jz      rv46_0901
rv46_0900:
        btr     d[ebp],9        ;disable virtual IF
        jmp     rv46_3
rv46_0901:
        bts     d[ebp],9        ;enable virtual IF
        jmp     rv46_3
rv46_0902:
        bt      d[ebp],9        ;get virtual IF
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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
maxBlock      dd ?  ;0	max. free virtual memory block (bytes)
freeUnlocked  dd ?  ;4	pages (max unlocked allocation)
maxLockable   dd ?  ;8	pages (max locked allocation)
totalAdrSpace dd ?  ;12 pages
unlocked      dd ?  ;16 pages
freePhys      dd ?  ;20 pages
totalPhys     dd ?  ;24 pages
freeAdrSpace  dd ?  ;28 pages
swapFile      dd ?  ;32
rsvd          dd ?,?,? ;36-47
DPMI_MINFO ends

        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi

        call    RawGetMemoryMax   ;get max virtual memory in EBX

        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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
        add     edx,UnLockedPages       ;include currently un-locked pages.
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

        mov     eax,UnLockedPages
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
        pop     eax             ; save edx value off of stack
        pop     edi             ; restore original edi value
        push    eax             ; restore edx value to stack
        test    ebx,ebx         ; see if any virtual memory
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
        sub     eax,AllocedPages

; MED 02/15/96
        mov     edx,MaxMemLin
        shr     edx,12
        sub     edx,AllocedPages
        cmp     eax,edx                         ; see if greater than MAXMEM choke-off point
        jbe     med3
        mov     eax,edx

med3:
        mov     es:[edi].DPMI_MINFO.freeAdrSpace,eax
        add     es:[edi].DPMI_MINFO.unlocked,edx; MED 01/25/96

;--- set total physical pages; not quite correct, since memory used for page tables isn't included.
        mov     eax,TotalPages                  ; pages used as backup for virt. memory
        add     eax,TotalPhysical               ; free phys. pages returned by VCPI/XMS/INT15/DOS
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

if (RELXMSINRM eq 0) or (VCPIPMCALL eq 0)
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
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     MaxMemPhys,0
        stc
        jz      rv50_9
        ;
        call    GetVCPIPage  ;modifies EDX, ECX
        jnc     rv50_8
        call    GetXMSPage   ;modifies EDX, ECX
        jnc     rv50_8
        call    GetINT15Page ;modifies EDX, ECX
        jnc     rv50_8
if DEFLOWMEM LT 640
        call    GetCONVPage  ;modifies EDX, ECX
        jnc     rv50_8
endif
        jmp     rv50_9
        ;
rv50_8:
;        and     edx,not 4095
        and     dx,0f000h
        dec     MaxMemPhys
        dec     TotalPhysical
        jns     rv50_nowrap
        mov     TotalPhysical,0
rv50_nowrap:
        clc
        ;
rv50_9:
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebx
        pop     eax
        ret
PhysicalGetPage endp


;-------------------------------------------------------------------------------
;
;Get number of (free) memory pages suppliable by all current memory types.
;
;On Exit:-
;
;EDX    - Pages supported.
;
;updates [TotalPhysical]

PhysicalGetPages proc near
        push    eax
        push    ebx
        push    ecx
        push    ds
        push    es
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:DPMIGRP
        ;
        call    GetVCPIPages
        mov     [rv51_Total],edx

        call    GetXMSPages
        ;new for v5.1: assume VCPI and XMS share memory, so don't add
        ;but instead use the larger value
        cmp     edx,[rv51_Total]
        jb      @F
        mov     [rv51_Total],edx
@@:
        call    GetINT15Pages           ; won't return anything if DPMI/VCPI/XMS is on
        add     [rv51_Total],edx
if DEFLOWMEM LT 640
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
rv51_0:
        mov     TotalPhysical,edx
        clc                     ;exit with success.
        ;
        pop     es
        pop     ds
        assume ds:nothing
        pop     ecx
        pop     ebx
        pop     eax
        ret
        ;

PhysicalGetPages endp

;-------------------------------------------------------------------------------
;
;Attempt to allocate a page of VCPI memory.
;out: NC: 
;     EDX=phys. page
;     ECX=1 (flag, actually CL only needed)
;
GetVCPIPage     proc    near
        push    eax
        push    edi
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     ProtectedType,PTYP_VCPI
        jnz     rv52_9      ;works because PTYP_RAWXMS < PTYP_VCPI, so C is set
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
        cmp     ProtectedType,PTYP_VCPI
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
;uses XMS v2 functions only.
;OUT: NC if success, EDX=phys. page, ECX=0
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
;uses XMS v2 functions only.
;out: EDX=pages
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
;       cmp     ProtectedType,PTYP_VCPI
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
        jnz     rv55_0          ;---->
        ;
rv55_2: ;Now release all memory blocks again.
        ;
        jmp     rv55_4
rv55_3:                            ;<---- next block
        cmp     d[esi],0
        jz      rv55_4
        mov     edi,offset MemIntBuffer
        mov     dx,[esi+0]
        mov     ah,0dh                  ;unlock EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        call    RawSimulateFCall
        mov     ah,0ah                  ;free EMB
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
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
;--- out: EDX = phys. page, ECX=0

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

Big1Check2:                     ; <----
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
        call    RawSimulateInt

        test    RealRegsStruc.Real_FlagsL[edi],1 ; see if carry returned
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
        jmp     Big1Check2      ; and retry ---->

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
;--- get int 15h, ah=88/e801 pages
;--- out: EDX=pages

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
        cmp     ProtectedType,PTYP_VCPI  ;skip for DPMI/VCPI
        jnc     rv57_9
        cmp     XMSPresent,0             ;and XMS
        jnz     rv57_9
        ;
        ;Setup initial simulated int 15 values.
        ;

Big1Check1:                     ; <----
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
        call    RawSimulateInt

        test    RealRegsStruc.Real_FlagsL[edi],1 ; see if carry returned
        je      GIProcess1      ; nope
        cmp     Big1Flag,0
        je      GIProcess1      ; not using alternate extended memory, process anyway
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check1      ; and retry ---->

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
        jmp     Big1Check1      ; and retry ---->

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


if DEFLOWMEM LT 640
;-------------------------------------------------------------------------------
;
;Attempt to allocate another page of conventional memory.
;out: EDX=page, ECX=0
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
        mov     bl,21h
        call    RawSimulateInt
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_FlagsL[edi],1
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
        mov     bl,21h
        call    RawSimulateInt
        test    es:RealRegsStruc.Real_FlagsL[edi],1
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
        mov     bl,21h
        call    RawSimulateInt
        pop     esi
        test    es:RealRegsStruc.Real_FlagsL[edi],1
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
;out: NC, pages in EDX
;      C if error
;
GetCONVPages    proc    near
        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        push    ds
        pop     es
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
        mov     edi,offset MemIntBuffer
        mov     [edi].RealRegsStruc.Real_AX,4800h     ;allocate memory block.
        mov     ax,ConvSaveSize
        mov     [edi].RealRegsStruc.Real_EBX,eax      ;set size desired.
        mov     bl,21h
        call    RawSimulateInt
        mov     eax,[edi].RealRegsStruc.Real_EAX      ;get segment address.
        test    [edi].RealRegsStruc.Real_FlagsL,1     ;call failed?
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
        cld
        rep     stosw
        ;
        mov     ecx,CONVITEMS
        mov     esi,offset ConvTempList
rv59_0:                         ;<----
        push    ecx
        mov     edi,offset MemIntBuffer
        mov     [edi].RealRegsStruc.Real_AX,4800h     ;allocate memory block.
        mov     [edi].RealRegsStruc.Real_BX,-1        ;set new size desired.
        mov     bl,21h
        call    RawSimulateInt
        pop     ecx
        mov     ebx,[edi].RealRegsStruc.Real_EBX
        cmp     bx,(4096*2)/16
        jc      rv59_2
        push    ebx
        push    ecx
        mov     [edi].RealRegsStruc.Real_AX,4800h     ;allocate memory block.
        mov     bl,21h
        call    RawSimulateInt
        pop     ecx
        pop     ebx
        test    [edi].RealRegsStruc.Real_FlagsL,1     ;call failed?
        jnz     rv59_2
        ;
        mov     eax,[edi].RealRegsStruc.Real_EAX
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
        loop    rv59_0          ;---->
        ;
rv59_2: ;Now release all memory blocks again.
        ;
        mov     ecx,CONVITEMS
        mov     esi,offset ConvTempList+((CONVITEMS-1)*2)
rv59_3:                         ;<----
        push    ecx
        cmp     w[esi],0
        jz      rv59_4
        mov     edi,offset MemIntBuffer
        mov     [edi].RealRegsStruc.Real_AX,4900h     ;release memory block.
        mov     ax,[esi]
        mov     [edi].RealRegsStruc.Real_ES,ax        ;set block to release.
        mov     bl,21h
        call    RawSimulateInt
rv59_4:
        pop     ecx
        sub     esi,2
        loop    rv59_3          ;---->
        ;
        ;Ask for big block to try and improve cleanup process.
        ;
        mov     edi,offset MemIntBuffer
        mov     [edi].RealRegsStruc.Real_AX,4800h     ;allocate memory block.
        mov     [edi].RealRegsStruc.Real_BX,-1        ;set new size desired.
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

        cmp     ConvSavePara,0         ;did we save any memory?
        jz      rv59_100
        pushad
        mov     edi,offset MemIntBuffer
        mov     [edi].RealRegsStruc.Real_AX,4900h     ;release memory block.
        mov     ax,ConvSavePara
        mov     [edi].RealRegsStruc.Real_ES,ax        ;set block to release.
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
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret

GetCONVPages    endp
endif

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
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
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

        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT
        jz      @F
        iret
@@:
        iretd

Raw41Patch      endp

