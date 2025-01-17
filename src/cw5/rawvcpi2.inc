
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Second level section for RAW/VCPI stuff. This holds the higher level management
;code like interrupt/exception handlers, memory management and DPMI emulation.
;This gets moved into extended memory during initialisation to keep conventional
;memory foot print down.
;
_cwDPMIEMU      segment para public 'DPMI emulator code' use32
        assume ds:_cwDPMIEMU
cwDPMIEMUStart  label byte

if 0
;--- vector to jump to previous int 31h handler
;--- the problem is: this is raw/vcpi mode, there is no "previous" 
;--- handler for int 31h, it's routed to real-mode!

DefaultInt31 label fword
        dd offset IntNN386Catch+(31h*INTNN386SIZE)
        dw DpmiEmuCS
endif
;-------------------------------------------------------------------------------
;
;Call _cwRaw SimulateINT
;
EmuRawSimulateInt proc near
        db 66h, 9ah
        dw offset _fRawSimulateInt,KernalCS
        ret
EmuRawSimulateInt endp

;-------------------------------------------------------------------------------
;
;Call _cwRaw SimulateINT
;
EmuRawSimulateInt2 proc near
        db 66h, 9ah
        dw offset _fRawSimulateInt2,KernalCS
        ret
EmuRawSimulateInt2 endp

;-------------------------------------------------------------------------------
;
;Call _cwRaw SimulateFCALL
;
EmuRawSimulateFCall proc near
        db 66h, 9ah
        dw offset _fRawSimulateFCall,KernalCS
        ret
EmuRawSimulateFCall endp

;-------------------------------------------------------------------------------
;
;Call _cwRaw SimulateFCALL
;
EmuRawSimulateFCall2 proc near
        db 66h, 9ah
        dw offset _fRawSimulateFCall2,KernalCS
        ret
EmuRawSimulateFCall2 endp

;-------------------------------------------------------------------------------
;
;Call _cwRaw SimulateFCALLI
;
EmuRawSimulateFCallI proc near
        db 66h, 9ah
        dw offset _fRawSimulateFCallI,KernalCS
        ret
EmuRawSimulateFCallI endp

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
        jz      rv46_DPMI_0500
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
        call    RawResDOSMemory
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
        mov     eax, offset EmuRawSimulateInt2
        jmp     rv46_DPMI_030x

rv46_DPMI_0301:
        push    eax
        mov     eax, offset EmuRawSimulateFCall2
        jmp     rv46_DPMI_030x

rv46_DPMI_0302:
        push    eax
        mov     eax, offset EmuRawSimulateFCallI
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
        push    es:RealRegsStruc.Real_CSIP[edi]   ; do not modify CS:IP and SS:SP members
        push    es:RealRegsStruc.Real_SSSP[edi]
        call    eax
        pop     es:RealRegsStruc.Real_SSSP[edi]
        pop     es:RealRegsStruc.Real_CSIP[edi]
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
        call    RAWGetCallBack
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
        call    RAWRelCallBack
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
        mov     al,90                                   ; changed from 90h to 90 decimal, MED 01/24/96

; MED 01/25/96
;       mov     bx,00000111b
        mov     bx,00000011b
; see if should turn on virtual memory supported bit 2
        push    ds
        mov     dx,KernalDS
        mov     ds,edx
        assume ds:GROUP16
        mov     dx,[RawSystemFlags]
        pop     ds
        assume ds:nothing
        and     dx,1 shl 1              ; isolate VMM bit
        shl     dx,1                    ; shift to proper position
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
rv46_DPMI_0500:
        cmp     al,00h          ;get memory information?
        jnz     rv46_DPMI_0501

        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    ebp
        push    edi
        push    es
        call    RAWGetMemoryMax
        pop     es
        pop     edi

        test    BYTE PTR cs:DpmiEmuSystemFlags,1
        jz      rv46_0
        movzx   edi,di
rv46_0:
        mov     DWORD PTR es:[edi+0],ebx
        shr     ebx,12
        mov     DWORD PTR es:[edi+04h],ebx

        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16

        call PhysicalGetPages   ;get number of un-claimed pages.
        add     edx,NoneLockedPages     ;include currently un-locked pages.
        mov eax,edx
        shr eax,10                              ;work out page tables needed for this.
        shl eax,1                               ;2 pages per table.
        sub edx,eax                             ;lose page tables.
        cmp edx,ebx
        jc rv46_500_0
        mov edx,ebx
rv46_500_0:
        mov     DWORD PTR es:[edi+08h],edx

; MED 01/25/96
;       mov     eax,LinearLimit
;       sub     eax,LinearBase
;       shr     eax,12
;       mov     es:d[edi+0ch],eax

        mov     eax,NoneLockedPages
        mov     DWORD PTR es:[edi+10h],eax

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
        mov     dl,VMMName              ;get drive letter for this media.
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
        mov     DWORD PTR es:[edi+0ch],ebx

        mov     eax,edx
        pop     edx
        add     edx,eax
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        mov     DWORD PTR es:[edi+14h],edx

; MED 01/25/96
;       mov     es:d[edi+1ch],edx
        mov     eax,DWORD PTR es:[edi+0ch]
        sub     eax,medAllocPages

; MED 02/15/96
        mov     edx,MaxMemLin
        shr     edx,12
        sub     edx,medAllocPages
        cmp     eax,edx                         ; see if greater than MAXMEM choke-off point
        jbe     med3
        mov     eax,edx

med3:
        mov     DWORD PTR es:[edi+1ch],eax

        add     DWORD PTR es:[edi+10h],edx      ; MED 01/25/96

        mov     eax,TotalPages
        add     eax,TotalPhysical       ; MED 01/25/96
        mov     DWORD PTR es:[edi+18h],eax

        mov     eax,SwapFileLength
        shr     eax,12
        mov     DWORD PTR es:[edi+20h],eax

        mov     DWORD PTR es:[edi+24h],-1
        mov     DWORD PTR es:[edi+28h],-1
        mov     DWORD PTR es:[edi+2ch],-1

        pop     ds
        assume ds:nothing

        pop     ebp
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax

        clc
        ret
        ;
rv46_DPMI_0501:
        cmp     al,01h          ;get memory block?
        jnz     rv46_DPMI_0502
        push    eax
        push    esi
        push    edi
        push    ebx
        push    ecx
        call    RAWGetMemory
        pop     eax
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
        ;
rv46_DPMI_0502:
        cmp     al,02h          ;free memory block?
        jnz     rv46_DPMI_0503
        call    RAWRelMemory
        ret
        ;
rv46_DPMI_0503:
        cmp     al,03h          ;re-size memory block?
        jnz     rv46_NotOurs
        push    eax
        push    esi
        push    edi
        push    ebx
        push    ecx
        call    RAWResMemory
        pop     eax
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
        ;
rv46_DPMI_0600:
        cmp     al,00h          ;lock memory?
        jnz     rv46_DPMI_0601
        call    RAWLockMemory
        ret
        ;
rv46_DPMI_0601:
        cmp     al,01h          ;un-lock memory?
        jnz     rv46_DPMI_0602
        call    RAWUnLockMemory
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
        test    DOS4GFlag,-1
        pop     ds
        assume ds:nothing
        je      DPMI_0A00_NotDOS4G

        mov     edi,OFFSET RationalCopyright

DPMI_0A00_loop:
        mov     al,[esi]
        cmp     al,cs:[edi]
        jne     DPMI_0A00_NotDOS4G
        test    al,al
        je      DPMI_0A00_match
        inc     esi
        inc     edi
        jmp     DPMI_0A00_loop

DPMI_0A00_match:
        pop     esi
        pop     edi
        push    cs
        pop     es
        mov     edi,OFFSET DPMI_0A00_APIEntryPoint      ; es:edi -> dummy extension entry point
        clc                                     ; flag success
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
RationalCopyright       DB      "RATIONAL DOS/4G",0
;
;Set hardware break point.
;
rv46_DPMI_0B00:
        cmp     al,00h          ;set debug watch point?
        jnz     rv46_DPMI_0B01
        push    eax
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
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
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        shl     ebx,16
        mov     bx,cx
        ;
        ;Find a free table entry.
        ;
        mov     esi,offset DbgTable
        mov     ecx,4
        xor     edi,edi
rv46_0B00_0:
        test    Dbg.Dbg_Flags[esi],128  ;free?
        jz      rv46_0B00_1
        inc     edi
        add     esi,size Dbg
        dec     ecx
        jnz     rv46_0B00_0
        jmp     rv46_0B00_9
rv46_0B00_1:
        mov     Dbg.Dbg_Flags[esi],128
        mov     Dbg.Dbg_Address[esi],ebx
        mov     Dbg.Dbg_Size[esi],dl
        mov     Dbg.Dbg_Type[esi],dh
        mov     ebx,edi
        ;
        ;Program the hardware.
        ;
        push    ds   
        mov     eax, offset rv46_0B00_pl0
        call    EmuCallPL0Proc
        pop     ds
        ;
        mov     eax,ebx
        pop     ebx
        mov     bx,ax
        clc
        jmp     rv46_0B00_10
rv46_0B00_9:
        pop     ebx
        stc
rv46_0B00_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        ret

rv46_0B00_pl0:        
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        mov     eax,Dbg.Dbg_Address[esi]
        or      ebx,ebx
        jz      rv46_0B00_d0
        cmp     ebx,1
        jz      rv46_0B00_d1
        cmp     ebx,2
        jz      rv46_0B00_d2
        ;
rv46_0B00_d3:
        mov     dr3,eax
        mov     eax,dr6
        and     eax,not 8
        mov     dr6,eax
        jmp     rv46_0B00_2
rv46_0B00_d2:
        mov     dr2,eax
        mov     eax,dr6
        and     eax,not 4
        mov     dr6,eax
        jmp     rv46_0B00_2
rv46_0B00_d1:
        mov     dr1,eax
        mov     eax,dr6
        and     eax,not 2
        mov     dr6,eax
        jmp     rv46_0B00_2
rv46_0B00_d0:
        mov     dr0,eax
        mov     eax,dr6
        and     eax,not 1
        mov     dr6,eax
        ;
rv46_0B00_2:
        ;Set length/type/enable
        ;
        xor     edx,edx
        mov     dl,Dbg.Dbg_Size[esi]
        mov     dl,b[edx+DbgSize2Dr]
        shl     dl,2
        xor     eax,eax
        mov     al,Dbg.Dbg_Type[esi]
        mov     al,b[eax+DbgType2Dr]
        or      dl,al
        mov     ecx,ebx
        shl     ecx,2
        add     ecx,16
        shl     edx,cl
        mov     ecx,ebx
        shl     ecx,1
        mov     eax,3
        shl     eax,cl
        or      edx,eax
        mov     eax,15
        mov     ecx,ebx
        shl     ecx,2
        add     ecx,16
        shl     eax,cl
        push    eax
        mov     ecx,ebx
        shl     ecx,1
        mov     eax,3
        shl     eax,cl
        mov     ecx,eax
        pop     eax
        or      ecx,eax
        not     ecx
        ;
        mov     eax,dr7
        and     eax,ecx
        or      eax,edx
        mov     dr7,eax
        retd
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
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        ;Check handle range.
        ;
        movzx   ebx,bx
        cmp     ebx,3+1
        jnc     rv46_0B01_9
        ;
        ;Point to Dbg entry.
        ;
        mov     eax,size Dbg
        mul     ebx
        add     eax,offset DbgTable
        mov     esi,eax
        test    Dbg.Dbg_Flags[esi],128
        jz      rv46_0B01_9
        ;
        ;Free Dbg entry.
        ;
        mov     Dbg.Dbg_Flags[esi],0
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
        pop     es
        pop     ds
        popad
        assume ds:nothing
        ret

rv46_0B01_pl0:
        mov     ecx,ebx
        shl     ecx,1
        mov     eax,3
        shl     eax,cl
        not     eax
        mov     ecx,dr7
        and     ecx,eax
        mov     dr7,ecx
        retd
;
;Get state of break point.
;
rv46_DPMI_0B02:
        cmp     al,02h          ;get debug watch point state?
        jnz     rv46_DPMI_0B03
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    eax
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        ;Check handle range.
        ;
        movzx   ebx,bx
        cmp     ebx,3+1
        jnc     rv46_0B02_9
        ;
        mov     eax,size Dbg
        mul     ebx
        add     eax,offset DbgTable
        ;
        ;Active?
        ;
        test    Dbg.Dbg_Flags[eax],128
        jz      rv46_0B02_9
        ;
        ;Check state in hardware.
        ;
        mov eax, offset rv46_0B02_pl0
        call    EmuCallPL0Proc
        ;
        ;Isolate bit we want.
        ;
        mov     eax,esi
        mov     ecx,ebx
        mov     edx,1
        shl     edx,cl
        and     eax,edx
        shr     eax,cl
        mov     ecx,eax
        ;
        pop     eax
        mov     ax,cx
        clc
        jmp     rv46_0B02_10
        ;
rv46_0B02_9:
        pop     eax
        stc
rv46_0B02_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        ret

rv46_0B02_pl0:
        mov     eax,dr6
        retd
;
;Reset hardware breakpoint.
;
rv46_DPMI_0B03:
        cmp     al,03h          ;reset debug watch point?
        jnz     rv46_NotOurs
        pushad
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        ;Check handle range.
        ;
        movzx   ebx,bx
        cmp     ebx,3+1
        jnc     rv46_0B03_9
        ;
        mov     eax,size Dbg
        mul     ebx
        add     eax,offset DbgTable
        ;
        ;Active?
        ;
        test    Dbg.Dbg_Flags[eax],128
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
        pop     es
        pop     ds
        assume ds:nothing
        popad
        ret

rv46_0B03_pl0:        
        mov     ecx,ebx
        mov     edx,1
        shl     edx,cl
        not     edx
        mov     eax,dr6
        and     eax,edx
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
RawDPMIPatch    endp


;-------------------------------------------------------------------------------
_ffPhysicalGetPage proc far
        call    PhysicalGetPage
        ret
_ffPhysicalGetPage endp


;-------------------------------------------------------------------------------
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
        assume ds:_cwDPMIEMU
        stc
        jz      rv50_9
        ;
        call    GetVCPIPage
        jnc     rv50_8
        call    GetXMSPage
        jnc     rv50_8
        call    GetINT15Page
        jnc     rv50_8
        call    GetCONVPage
        jnc     rv50_8
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
        assume ds:_cwDPMIEMU
        clc
        ;
rv50_9:
        pop     gs
        pop     fs
        pop     es
        pop     ds
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
        ;
        mov     [rv51_Total],0
        call    GetVCPIPages
        add     [rv51_Total],edx

; MED, 11/11/99
        test    edx,edx
        jne     pgp2                    ; VCPI available memory exists
        push    ds
        mov     ax,MainDS
        mov     ds,eax
        assume ds:GROUP16
        mov     VCPIHasNoMem,1  ; flag no memory, so XMS gets a crack at it
        pop     ds
        assume ds:_cwDPMIEMU
pgp2:

        call    GetXMSPages
        add     [rv51_Total],edx

; MED, 11/11/99
        push    ds
        mov     ax,MainDS
        mov     ds,eax
        assume ds:GROUP16
        mov     VCPIHasNoMem,0  ; reset flag
        pop     ds
        assume ds:_cwDPMIEMU

        call    GetINT15Pages
        add     [rv51_Total],edx
        call    GetCONVPages
        jc      pgp3                    ; error allocating pages, MED, 11/15/99
        add     [rv51_Total],edx

pgp3:
        mov     edx,[rv51_Total]
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     edx,MaxMemPhys
        jc      rv51_0
        mov     edx,MaxMemPhys
rv51_0: mov     TotalPhysical,edx
        assume ds:_cwDPMIEMU
        clc                     ;exit with success.
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
        ;
	align 4
rv51_Total  dd ?

PhysicalGetPages endp

;-------------------------------------------------------------------------------
;
;Attempt to allocate a page of VCPI memory.
;
GetVCPIPage     proc    near
        push    eax
        push    edi
        push    ds
        mov     ax,MainDS
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
        call    EmuRawSimulateFCall
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
        assume ds:_cwDPMIEMU
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
        mov     ax,MainDS
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
        call    EmuRawSimulateFCall
        pop     ecx
        pop     ebx
        pop     es
        mov     eax,RealRegsStruc.Real_EAX[edi]
        mov     edx,RealRegsStruc.Real_EDX[edi]
        pop     edi
endif
        or      ah,ah           ;get anything?
        jnz     rv53_9
        clc
        jmp     rv53_10
        ;
rv53_9: xor     edx,edx
        stc
        ;
rv53_10:
        pop     ds
        pop     eax
        ret
if VCPIPMCALL
getfreevcpipages:
        assume ds:GROUP16
        mov ax, 0DE03h
        jmp [VCPI_Entry]
endif
        assume ds:_cwDPMIEMU

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
        push    fs
        push    gs
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        cmp     XMSPresent,0
        jz      rv54_9
        ;
        mov     edi,offset MemIntBuffer
        ;
rv54_3: mov     esi,offset XMSList
        xor     edi,edi
        mov     ecx,XMSITEMS
rv54_0:
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
rv54_1: or      edi,edi         ;already got a free entry?
        jnz     rv54_2
        mov     edi,esi
rv54_2:
        add     esi,sizeof XMSMEMITEM          ;next entry.
        dec     ecx
        jnz     rv54_0
        ;
        ;Need to try and allocate a new block.
        ;
        mov     esi,edi         ;get free entry number.
        or      esi,esi
        jz      rv54_9
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        push    esi
        mov     ah,08h
        mov     RealRegsStruc.Real_EAX[edi],eax
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;get size of largest block.
        pop     esi
        mov     eax,RealRegsStruc.Real_EAX[edi]
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
        push    esi
        push    ax
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EDX[edi],eax
        mov     ah,9
        mov     RealRegsStruc.Real_EAX[edi],eax
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;allocate memory.
        pop     bp
        pop     esi
        mov     eax,RealRegsStruc.Real_EAX[edi]
        cmp     ax,1
        jnz     rv54_9
        mov     edx,RealRegsStruc.Real_EDX[edi] ;get the handle.
        mov     [esi].XMSMEMITEM.wHdl,dx
        push    esi
        push    bp
        push    dx
        mov     ah,0ch
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;lock block & get address.
        pop     cx
        pop     bp
        pop     esi
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
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
        assume ds:_cwDPMIEMU
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
        push    fs
        push    gs
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
        mov     ecx,XMSITEMS
        xor     eax,eax
        push    ds
        pop     es
        cld
        rep     stosd
        ;
        mov     edi,offset MemIntBuffer
        ;
        mov     ecx,XMSITEMS
        mov     esi,offset XMSTempList
rv55_0:
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        ;
        push    ecx
        push    esi
        mov     ah,08h
        mov     RealRegsStruc.Real_EAX[edi],eax
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;get size of largest block.
        pop     esi
        pop     ecx
        mov     eax,RealRegsStruc.Real_EAX[edi]
        or      ax,ax
        jz      rv55_2
        ;
        ;Limit to pre-defined size.
        ;
        cmp     ax,XMSBlockSize
        jc      rv55_SizeOK
        mov     ax,XMSBlockSize
rv55_SizeOK:
        push    ecx
        push    esi
        push    ax
        mov     RealRegsStruc.Real_EDX[edi],eax
        mov     ah,9
        mov     RealRegsStruc.Real_EAX[edi],eax
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;allocate memory.
        pop     bp
        pop     esi
        pop     ecx
        mov     eax,RealRegsStruc.Real_EAX[edi]
        or      ax,ax
        jz      rv55_2
        mov     edx,RealRegsStruc.Real_EDX[edi] ;get the handle.
        mov     [esi+0],dx
        mov     w[esi+2],1
        push    ecx
        push    esi
        push    bp
        push    dx
        mov     ah,0ch
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;lock block & get address.
        pop     cx
        pop     bp
        pop     esi
        pop     ecx
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
        add     edi,4095                ;round up to next page.
        shr     edi,12
        shr     ebx,12
        sub     ebx,edi
        js      rv55_1
        dec     ebx
        js      rv55_1
        add     [XMSTotal],ebx
rv55_1:
        add     esi,4
        dec     ecx
        jnz     rv55_0
        ;
rv55_2: ;Now release all memory blocks again.
        ;
        mov     ecx,XMSITEMS
        mov     esi,offset XMSTempList+((XMSITEMS-1)*4)
rv55_3:
        push    ecx
        push    esi
        cmp     d[esi],0
        jz      rv55_4
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     dx,[esi]
        mov     ah,0dh
        push    dx
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall     ;un-lock it first.
        pop     dx
        mov     ah,0ah
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EDX[edi],edx
        push    ax
        mov     eax,[XMSControl]
        mov     RealRegsStruc.Real_CSIP[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        pop     ax
        call    EmuRawSimulateFCall
rv55_4:
        pop     esi
        pop     ecx
        sub     esi,4
        dec     ecx
        jnz     rv55_3
        ;
        ;Now find out how much of existing blocks is free.
        ;
        mov     esi,offset XMSList
        xor     edi,edi
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
        dec     ecx
        jnz     rv55_5
        ;
rv55_9: ;Now return pages found.
        ;
        mov     edx,[XMSTotal]
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        assume ds:_cwDPMIEMU
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
        push    fs
        push    gs
        ;
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        cmp     XMSPresent,0
        pop     ds
        assume ds:_cwDPMIEMU
        jnz     rv56_9
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
rv56_3: mov     esi,offset Int15Table
        mov     ecx,INT15ITEMS
        xor     edi,edi
rv56_0:
        cmp     d[esi],0
        jz      rv56_1
        mov     eax,[esi]
        add     eax,4096
        cmp     eax,4[esi]
        jc      rv56_GotOne
        jmp     rv56_2
rv56_1: or      edi,edi
        jnz     rv56_2
        mov     edi,esi
rv56_2: add     esi,4+4
        dec     ecx
        jnz     rv56_0
        ;
        ;Need to get another block of memory.
        ;
        or      edi,edi
        jz      rv56_9
        cmp     ILevel,8
        jnc     rv56_9          ;can't cope with any more.
        push    edi
        push    ax
        mov     ax,KernalZero
        mov     es,eax
        pop     ax
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
        cmp     Big1Flag,0      ; see if using alternate 0e801h means to get memory
        je      use88h2
        mov     ax,0e801h
        jmp     GIGetMem2

use88h2:
        mov     ah,88h          ; get top of extended memory

GIGetMem2:
        mov     bl,15h
        mov     edi,offset MemIntBuffer
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    EmuRawSimulateInt

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
        push    ax
        or      ax,bx           ; see if ax=bx=0, if so use cx,dx return instead
        pop     ax
        jne     GIComputeBig1Size2
        movzx   eax,WORD PTR RealRegsStruc.Real_ECX[edi]
        movzx   ebx,WORD PTR RealRegsStruc.Real_EDX[edi]
        push    ax
        or      ax,bx           ; see if cx=dx=0, if so use regular 88h function
        pop     ax
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
        ;EBX - base.
        ;ECX - size.
        ;
        mov     0[edi],ebx              ;store base address.
        mov     4[edi],ebx
        add     4[edi],ecx              ;set end address.
        ;
        movzx   esi,ILevel
        shl     esi,3
        add     esi,offset ITable
;       dec     ebx             ;move back to previous byte.
        sub     ebx,100000h             ;remove starting point.
        shr     ebx,10          ;convert to K.
        mov     [esi+4],ebx             ;set new base value.
        mov     bl,15h*4
        movzx   ebx,bl
        push    es
        push    eax
        mov     ax,KernalZero
        mov     es,eax
        mov     eax,es:[ebx]
        mov     [esi+0],eax             ;store old vector.
        pop     eax
        movzx   edx,ILevel
        mov     dx,[Int15PatchTable+edx*2]
        mov     cx,GROUP16               ;segment to use.
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
        mov     edx,0[esi]              ;get base address.
        add     d[esi],4096             ;move pointer along.
        xor     ecx,ecx
        clc
        jmp     rv56_10
        ;
rv56_9: stc
        ;
rv56_10:
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
        assume ds:_cwDPMIEMU
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
        push    fs
        push    gs
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
        cmp     Big1Flag,0      ; see if using alternate 0e801h means to get memory
        je      use88h1
        mov     ax,0e801h
        jmp     GIGetMem1

use88h1:
        mov     ah,88h          ; get top of extended memory

GIGetMem1:
        mov     bl,15h
        mov     edi,offset MemIntBuffer
        push    ds
        pop     es
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    EmuRawSimulateInt

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
        movzx   eax,WORD PTR RealRegsStruc.Real_EAX[edi]
        movzx   ebx,WORD PTR RealRegsStruc.Real_EBX[edi]

        push    ax
        or      ax,bx           ; see if ax=bx=0, if so use cx,dx return instead
        pop     ax
        jne     GIComputeBig1Size1
        movzx   eax,WORD PTR RealRegsStruc.Real_ECX[edi]
        movzx   ebx,WORD PTR RealRegsStruc.Real_EDX[edi]
        push    ax
        or      ax,bx           ; see if cx=dx=0, if so use regular 88h function
        pop     ax
        jne     GIComputeBig1Size1
        mov     Big1Flag,0      ; turn off alternate
        jmp     Big1Check1      ; and retry

GIComputeBig1Size1:
        shl     ebx,6           ; 64K chunks to 1K
        add     eax,ebx         ; add to 1K chunks below 64M
        jmp     GIComputeBytes1

use88hResult1:
        movzx   eax,WORD PTR RealRegsStruc.Real_EAX[edi]

GIComputeBytes1:

;       mov     w[Int15hValue],ax
        mov     [Int15hValue],eax

        mov     ax,ILevel
        mov     [INT15hLevel2],ax
        ;
rv57_0: ;Need to get another block of memory.
        ;
        cmp     [INT15hLevel2],8
        jnc     rv57_1          ;can't cope with any more.
        push    ax
        mov     ax,KernalZero
        mov     es,eax
        pop     ax
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
rv57_2: cmp     d[esi],0
        jz      rv57_3
        mov     eax,[esi]
        add     eax,4096
        cmp     eax,4[esi]
        jnc     rv57_3
        mov     eax,4[esi]
        sub     eax,0[esi]
        shr     eax,12          ;free pages remaining
        dec     eax
        add     [INT15hTotal],eax
rv57_3: add     esi,4+4
        dec     ecx
        jnz     rv57_2
        ;
rv57_9: mov     edx,[INT15hTotal]
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
        assume ds:_cwDPMIEMU
GetINT15Pages   endp


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
        push    fs
        push    gs
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        cmp     CONVSaveSize,-1
        jz      rv58_9
        ;
        mov     CONVSavePara,0
        cmp     CONVSaveSize,0
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     ax,CONVSaveSize
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_Flags[edi],1
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        jnz     rv58_9          ;if not enough for user buffer then don't allocate any more.
        mov     CONVSavePara,ax ;store para we saved.
rv58_200:
rv58_3: mov     esi,offset ConventionalList
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4a00h    ;resize memory block.
        mov     ax,[esi].DOSMEMITEM.wAddr               ;get real mode segment
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     ax,[esi].DOSMEMITEM.wSize               ;get current size.
        add     ax,4096/16
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4a00h    ;resize memory block.
        mov     ax,[esi].DOSMEMITEM.wAddr               ;get real mode segment
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     ax,[esi].DOSMEMITEM.wSize               ;get current size.
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        pop     edi
        pop     esi
        pop     ecx
        jmp     rv58_2
        ;
rv58_1: or      edi,edi         ;already got a free entry?
        jnz     rv58_2
        mov     edi,esi
rv58_2: add     esi,sizeof DOSMEMITEM    ;next entry.
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     es:RealRegsStruc.Real_EBX[edi],1        ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
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
        cmp     CONVSavePara,0  ;did we save any memory?
        jz      rv58_100
        pushad
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     w es:RealRegsStruc.Real_EAX[edi],4900h    ;release memory block.
        mov     ax,CONVSavePara
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        popad
        mov     CONVSavePara,0
rv58_100:
        popfd
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        assume ds:_cwDPMIEMU
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
        push    fs
        push    gs
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        mov     [CONVTotal],0
        ;
        cmp     CONVSaveSize,-1
        jz      rv59_9
        ;
        mov     CONVSavePara,0
        cmp     CONVSaveSize,0
        jz      rv59_200
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     ax,CONVSaveSize
        mov     es:RealRegsStruc.Real_EBX[edi],eax      ;set size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_Flags[edi],1
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        jnz     rv59_9          ;if not enough for user buffer then don't allocate any more.
        mov     CONVSavePara,ax ;store para we saved.
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     w es:RealRegsStruc.Real_EBX[edi],-1       ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        pop     esi
        pop     ecx
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        cmp     bx,(4096*2)/16
        jc      rv59_2
        push    ebx
        push    ecx
        push    esi
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
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
        add     [CONVTotal],ebx
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4900h    ;release memory block.
        mov     ax,[esi]
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
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
        mov     w es:RealRegsStruc.Real_EAX[edi],4800h    ;allocate memory block.
        mov     es:RealRegsStruc.Real_EBX[edi],-1       ;set new size desired.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        ;
        ;Now return pages found.
        ;

;       mov     edx,[CONVTotal]
        ;
rv59_9:
        pushfd
        mov     edx,[CONVTotal]        ; moved, MED, 11/15/99

        cmp     CONVSavePara,0  ;did we save any memory?
        jz      rv59_100
        pushad
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
        mov     w es:RealRegsStruc.Real_EAX[edi],4900h    ;release memory block.
        mov     ax,CONVSavePara
        mov     es:RealRegsStruc.Real_ES[edi],ax        ;set block to release.
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    EmuRawSimulateInt
        popad
        mov     CONVSavePara,0
rv59_100:
        popfd
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax
        ret
GetCONVPages    endp


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

;-------------------------------------------------------------------------------
;
;Convert number into ASCII Hex version.
;
;On Entry:-
;
;EAX    - Number to convert.
;ECX    - Digits to do.
;DS:EDI - Buffer to put string in.
;
Bin2HexE        proc    near
        push    edi
        push    ecx
        push    ebx
        mov     ebx,offset HexTableE
        add     edi,ecx
        dec     edi
        mov     edx,eax
rv62_0: mov     al,dl
        shr     edx,4
        and     al,15
        xlat
        mov     [edi],al
        dec     edi
        dec     ecx
        jnz     rv62_0
        pop     ebx
        pop     ecx
        pop     edi
        add     edi,ecx
        ret
Bin2HexE        endp

        assume ds:_cwDPMIEMU

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
        mov     [ebp+(4+4)+(2+2)], al           ;update lobyte(fl) in IRET16 frame
        pop     ebp
        pop     eax
        iret
rv64_Use32Bit8:
        mov     [esp+(4)+(4+4)],al              ;update lobyte(efl) in IRET32 frame
        pop     eax
        iretd
        ;
rv64_NotOurs:
        ;Not a function recognised by us so pass control to previous handler.
        ;
        jmp     cs:[OldInt2Fp]                  ;pass it onto previous handler.
;
OldInt2Fp label fword
        dd offset IntNN386Catch+(2fh*INTNN386SIZE)
        dw DpmiEmuCS

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

        assume ds:_cwDPMIEMU

        include interrup.inc
        include ldt.inc
        include memory.inc


HexTableE       db '0123456789ABCDEF'
	align 4
cwDPMIEMUEnd    label byte
_cwDPMIEMU      ends

