
;--- emulation of DPMI ax=01xxh functions

;--- 1. CauseWay originally allocated 1 paragraph more than requested and
;---    stored the size of the block in the first word of this para;
;---    this has been removed, the block's size is determined by
;---    the selector limit; old variant can still be activated with ADD1PARA equ 1.
;--- 2. selector tiling: originally active for both 16-bit and 32-bit clients;
;---    to restore old behavior, set TILE16ONLY equ 0.
;--- 3. enlarging a block with ax=102h isn't implemented. Should be, at
;---    least for 32-bit clients, once selector tiling has been fixed.

ADD1PARA equ 0  ;1=alloc 1 para header to store size (in para) in first word
TILE16ONLY equ 1;1=selector tiling for 16-bit only; 0=tiling for both 16- and 32-bit.
EXPANDIF32 equ 1;1=expand DOS mem block if app is 32-bit (AND selector tiling is off)
;-------------------------------------------------------------------------------
;On Entry:-
;
;ES:EDI - Descriptor entry to use.
;ESI    - Linear base to set.
;ECX    - limit in bytes.
;AL     - desc size bit.
;AH     - Present/PL/memory|system/type bits.
;
RawMakeDesc     proc    near
        pushad
        mov     es:[edi].Desc.Limit,cx
        mov     es:[edi].Desc.Base_l,si
        shr     ecx,16
        shr     esi,16
        mov     bx,si
        or      cl,al
        mov     es:[edi].Desc.Base_m,bl
        mov     es:[edi].Desc.Access,ah
        mov     es:[edi].Desc.Gran,cl
        mov     es:[edi].Desc.Base_H,bh
        popad
        ret
RawMakeDesc     endp


;-------------------------------------------------------------------------------
;
;DPMI ax=0100h: Allocate a block of DOS memory & provide a selector to access it with.
;caller (RawDPMIPatch) ensures hiword(eax), hiword(edx) and ebx are preserved.
;
RawGetDOSMemory proc near
        @dprintf DOPT_MEM,<"RawGetDOSMemory enter",10>
        push    ecx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
if ADD1PARA        
        cmp     bx,-1           ;maximum?
        jz      mem19_0
        inc     ebx             ;para extra for us.
mem19_0:
endif
        push    ebx
        mov     edi,offset MemIntBuffer ;RawGetDOSMemory
        push    ds
        pop     es
        mov     RealRegsStruc.Real_AX[edi],4800h;get memory.
        mov     RealRegsStruc.Real_EBX[edi],ebx ;memory size.
        mov     bl,21h
        call    RawSimulateInt  ;allocate it.
        pop     ecx
        mov     eax,RealRegsStruc.Real_EAX[edi] ;get result.
        mov     ebx,RealRegsStruc.Real_EBX[edi]
        test    RealRegsStruc.Real_FlagsL[edi],1
        jz      mem19_1
if ADD1PARA
        or      bx,bx           ;nothing available?
        jz      mem19_err
        dec     ebx             ;leave space for us!
endif
        jmp     mem19_err
        ;
mem19_1:
        ;Now try and allocate enough selectors.
        ;Actually, selector tiling should be done for 16-bit clients only
        ;
        push    eax
        push    ecx
if TILE16ONLY
        test    b SystemFlags,SF_16BIT
        jnz     @F
        mov     cx,1
        jmp     mem19_1a
@@:
endif
if ADD1PARA
        dec     ecx             ;lose our para.
endif
        movzx   ecx,cx
        add     ecx,4096-1
        shr     ecx,12          ;get number of 64k chunks.
mem19_1a:
        call    RawGetDescriptors
        pop     ecx
        pop     ebx
        jc      mem19_8
if ADD1PARA
        ;
        ;Store block size.
        ;
        push    ecx
        mov     dx,KernalZero
        mov     es,edx
        movzx   esi,bx          ;get segment address.
        shl     esi,4           ;get linear address.
        mov     WORD PTR es:[esi],cx            ;store block size.
        dec     WORD PTR es:[esi]               ;lose our para.
        pop     ecx
endif
        ;
        ;Setup selectors.
        ;
if ADD1PARA
        dec     cx              ;lose our para.
        inc     bx              ;/
endif
        push    eax
        push    ebx
        push    ds
        pop     es
        mov     edi,offset RawDescBuffer
        movzx   ecx,cx
        ;
mem19_2:                 ;<---- next descriptor
        push    eax
        push    ebx
        push    ecx
        movzx   esi,bx
        mov     ebx,eax
        shl     esi,4    ;esi=base
        shl     ecx,4
        dec     ecx      ;ecx=limit in bytes
        mov     al,0
        mov     ah,DescPresent+DescPL3+DescMemory+DescRWData
        call    RawMakeDesc
        call    RawBPutDescriptor
        pop     ecx
        pop     ebx
        pop     eax
if TILE16ONLY
        test    b SystemFlags,SF_16BIT
        jz      mem19_2a
endif
        add     eax,8           ;next selector.
        add     ebx,1000h       ;update segment base.
        sub     ecx,1000h       ;reduce segment size.
        ja      mem19_2         ;---->
mem19_2a:
        ;
        pop     eax             ;Get base segment again.
        pop     edx             ;Get base selector again.
        clc
        jmp     mem19_done
        ;
mem19_8:
        ;Release memory we managed to allocate.
        ;
        mov     edi,offset MemIntBuffer ;RawGetDOSMemory
;        mov     ax,KernalDS
;        mov     es,eax
        mov     RealRegsStruc.Real_AX[edi],4900h    ;release memory.
        mov     RealRegsStruc.Real_ES[edi],bx       ;segment.
        mov     bl,21h
        call    RawSimulateInt   ;release it.
        ;
mem19_err:
        stc
mem19_done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     ecx
        ret
RawGetDOSMemory endp


;-------------------------------------------------------------------------------
;
;DPMI ax=0102h: Re-size a block of DOS memory.
;
;IN: DX=selector, BX=new size in paragraphs
;
;failure: DOS error (error code returned in AX 7/8/9, max size in BX)
;         DPMI error (out of descriptors ax=8022, invalid block ax=8022)
;caller (RawDPMIPatch) ensures hiword(eax) and ebx are preserved.
;
RawResizeDOSMemory proc near
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        ;
        push    ebx
        push    edx
        mov     ebx,edx
        call    RawGetSelBase
        push    cx
        push    dx
        pop     esi             ;esi=base

        pop     edx
        pop     ebx
        mov     ax,8022h
        jc      mem20_err       ;selector invalid
        cmp     esi,0ffff0h+65536
        cmc
        jc      mem20_err       ;can't be DOS memory
if ADD1PARA
        sub     esi,16          ;back to our stuff.
        mov     ax,KernalZero
        mov     es,eax
        cmp     bx,WORD PTR es:[esi] ;shrinking or expanding?
else
        lsl     eax,edx
        inc     eax
        shr     eax,4
        cmp     bx,ax
endif
        jz      mem20_8         ;size unchanged
        jnc     mem20_Expand_err
        ;
mem20_Shrink:
        ;Attempt to shrink the memory block.
        ;
        push    ebx
        push    edx
if ADD1PARA
        inc     ebx             ;include our para.
endif
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer ;RawResizeDOSMemory
        mov     eax,esi
        shr     eax,4           ;get real mode segment.
        mov     RealRegsStruc.Real_AX[edi],4a00h
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_ES[edi],ax
        mov     bl,21h
        call    RawSimulateInt
        mov     eax,RealRegsStruc.Real_EAX[edi]
        mov     ebx,RealRegsStruc.Real_EBX[edi]
        test    RealRegsStruc.Real_FlagsL[edi],1
        pop     edx
        pop     ecx
        jnz     mem20_err        ;DOS failed it!
        ;
        ;Lose any selectors that are no longer needed.
        ;
if ADD1PARA        
        push    ecx
        push    edx

        mov     ebx,edx
        call    RawGetSelBase
        push    cx
        push    dx
        pop     esi             ;esi=base
        sub     esi,16          ;back to our stuff.

        pop     edx
        pop     ecx
        push    ecx
        push    edx
        mov     ax,KernalZero
        mov     es,eax
        mov     bx,WORD PTR es:[esi]  ;get old size.
        mov     WORD PTR es:[esi],cx  ;store new size.
        pop     edx
        pop     ecx
else
        lsl     ebx,edx
        inc     ebx
        shr     ebx,4
endif        
;--- here: dx=first sel;bx=old size;cx=new size
        mov     eax,edx
        movzx   ecx,cx
mem20_2:                        ;<---- next descriptor
        push    eax
        push    ebx
        mov     ebx,eax

        cmp     ecx,0
        jnz     @F
        call    RawRelDescriptor
        jmp     desc_done
@@:
        push    ecx
        shl     ecx,4
        dec     ecx             ;ecx=limit in bytes
        push    ecx
        pop     dx
        pop     cx
        call    RawSetSelLimit
        pop     ecx
        sub     ecx,1000h       ;reduce segment size.
        jnc     desc_done
        xor     ecx,ecx
desc_done:
        pop     ebx
        pop     eax
if TILE16ONLY
        test    b SystemFlags,SF_16BIT
        jz      mem20_8
endif
        add     eax,8           ;next selector.
        sub     ebx,1000h
        ja      mem20_2         ;keep going till all done.
;
mem20_8:
        clc
        jmp     mem20_done
        ;
mem20_Expand_err:
        ;
        ;Attempt to expand the memory block.
        ;
if TILE16ONLY and EXPANDIF32
;--- for 32-bit, expanding is ok if selector tiling is off
        test    b SystemFlags,SF_16BIT
        jz      mem20_Shrink
endif
if ADD1PARA
        mov     bx,WORD PTR es:[esi]            ;return current length as maximum.
else
        mov     bx,ax
endif
        mov     ax,8   ;out of memory
mem20_err:
        stc
mem20_done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        ret
RawResizeDOSMemory endp


;-------------------------------------------------------------------------------
;
;DPMI ax=0101h: Release a block of DOS memory.
;
;IN:  DX=selector
;
;caller (RawDPMIPatch) ensures hiword(eax) is preserved
;if DOS call fails with C, return error code in EAX
;(error code 7 apparently is never returned by DOS, meaning a call to
;free a block twice won't return an error if the MCB still exists).
;for invalid selectors, return AX=8022h
;
RawReleaseDOSMemory proc near

        push    ebx
        push    ecx
        push    edx
if ADD1PARA
        push    esi
endif
        push    edi
        ;
        ;Get segment base address.
        ;
        mov     ax,8022h
        mov     ebx,edx
        call    RawGetSelBase
        jc      mem21_err
        shl     ecx,16
        mov     cx,dx
        cmp     ecx,0ffff0h+65536
        cmc
        jc      mem21_err       ;can't be DOS memory
if ADD1PARA
        mov     esi,ecx
        sub     esi,16
        mov     eax,esi
        shr     eax,4           ;real mode paragraph address.
        push    ds
        mov     dx,KernalZero
        mov     ds,edx
        movzx   ecx,WORD PTR [esi]  ;get block size.
        pop     ds
else
        mov     eax,ecx
        shr     eax,4
        lsl     ecx,ebx
        inc     ecx
        shr     ecx,4           ;convert to paragraphs
endif
        ;
        ;Release DOS memory block.
        ;
        push    es
        push    ebx
        mov     dx,KernalDS
        mov     es,edx
        assume es:GROUP16
        mov     edi,offset MemIntBuffer ;RawReleaseDOSMemory
        mov     es:[edi].RealRegsStruc.Real_AX,4900h;release block.
        mov     es:[edi].RealRegsStruc.Real_ES,ax   ;block to release.
        mov     bl,21h
        call    RawSimulateInt
        mov     edx,es:[edi].RealRegsStruc.Real_EAX
        test    b es:[edi].RealRegsStruc.Real_Flags,1
        pop     ebx
        pop     es
        assume es:nothing
        jz      mem21_0
        mov     eax,edx
        stc
        jmp     mem21_err

;--- Release selectors.
;--- RawRelDescriptor will set any seg regs to NULL if they contain the selector to be freed.
;--- todo: selector tiling should be done for 16-bit only.
mem21_0:
        call    RawRelDescriptor
if TILE16ONLY
        test    b DpmiEmuSystemFlags,SF_16BIT
        jz      mem21_1
endif
        add     ebx,8           ;next descriptor.
        sub     ecx,1000h
        ja      mem21_0         ;release all selectors.
mem21_1:
        clc
mem21_err:
        pop     edi
if ADD1PARA
        pop     esi
endif
        pop     edx
        pop     ecx
        pop     ebx
        ret
RawReleaseDOSMemory endp
