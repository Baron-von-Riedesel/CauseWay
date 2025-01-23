
;--- for RAW/VCPI mode

MAXREAL2PROT equ 128

	align 2
Real2ProtList   dw MAXREAL2PROT dup (0)

;-------------------------------------------------------------------------------
;
;Allocate block of descriptor entries.
;
;On Entry:-
;
;CX     - Number of descriptors to allocate.
;
;On Exit:-
;
;AX     - Base selector.
;
RawGetDescriptors proc near
        ;
        push    ecx
        push    edx
        push    edi
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax
        mov     edi,MDTLinear+4 ;point to descriptor map.
        movzx   edx,cx
        mov     ecx,8192
        mov     al,0
        cld
        dec     edx
        jnz     ldt1_0
        ;
        ;Search for single selector.
        ;
        repne   scasb           ;find a zero.
        jnz     ldt1_err
        dec     edi
        mov     al,1
        mov     es:[edi],al
        jmp     ldt1_1
        ;
        ;search for multiple selectors.
        ;
ldt1_0:
        repne   scasb           ;find a zero.
        jnz     ldt1_err
        cmp     edx,ecx
        jnc     ldt1_err
        push    ecx
        push    edi
        mov     ecx,edx
        repe    scasb           ;check for string of zero's
        pop     edi
        pop     ecx
        jnz     ldt1_0
        dec     edi
        push    edi
        mov     ecx,edx
        inc     ecx
        mov     al,1
        rep     stosb           ;mark all entries as used.
        pop     edi
ldt1_1:
        sub     edi,MDTLinear+4
        shl     edi,3
        push    eax
        pop     ax
        push    di
        add     edi,MDTLinear
        mov     ecx,edx
        inc     ecx
ldt1_2:
        xor     eax,eax
        stosd   es:[edi]
        mov     eax,(DescPresent+DescMemory+DescRWData+DescPL3) shl 8
        stosd   es:[edi]
        loop    ldt1_2
        pop     eax
        or      ax,111b         ;RPL=3, TI=1
        jmp     ldt1_9
ldt1_err:
        stc
ldt1_9:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     edx
        pop     ecx
        ret
RawGetDescriptors endp


;-------------------------------------------------------------------------------
;
;Release a previously allocated descriptor entry.
;
;On Entry:-
;
;BX     - Selector for descriptor to release.
;
RawRelDescriptor proc near
        ;
        push    eax
        push    ecx
        push    esi
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax

        test    bl,4            ;LDT selector?
        jz      error
        movzx   esi,bx          ;Get selector to use.
        shr     esi,3           ;/8 for descriptor number.
        cmp     esi,GDT_Entries
        jc      error
        add     esi,MDTLinear+4
        cmp     BYTE PTR es:[esi],0  ;entry allocated?
        jz      error
        mov     BYTE PTR es:[esi],0  ;mark this entry as free.
        movzx   esi,bx
        and     esi,not 7
        add     esi,MDTLinear
        xor     eax,eax
        mov     es:[esi+0],eax
        mov     es:[esi+4],eax
        xor     ecx,ecx
        mov     esi,gs
        lar     eax,esi
        jz      @F
        mov     gs,ecx
@@:
        mov     esi,fs
        lar     eax,esi
        jz      @F
        mov     fs,ecx
@@:
        pop     esi
        lar     eax,esi
        jz      @F
        mov     esi,ecx
@@:
        mov     es,esi
        pop     esi
        lar     eax,esi
        jz      @F
        mov     esi,ecx
@@:
        mov     ds,esi
        ;
        clc
        jmp     rrd_9
error:
        pop     es
        pop     ds
        stc
rrd_9: 
        assume ds:nothing
        pop     esi
        pop     ecx
        pop     eax
        ret
RawRelDescriptor endp


;-------------------------------------------------------------------------------
;
;Create/return a protected mode selector that maps a real mode segment.
;
;On Entry:
;
;BX     - Real mode segment address to map.
;
;On Exit:
;
;AX     - Protected mode selector.
;
;Note: Multiple calls with the same value will nearly always return the same
;selector depending on how many selectors have been allocated with this routine.
;

RawReal2ProtSel proc    near
        ;
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,eax
        assume ds:_cwDPMIEMU
        movzx   ebx,bx
        shl     ebx,4           ;convert segment to linear.
        mov     esi,offset Real2ProtList        ;point to list of selectors.
        mov     ecx,MAXREAL2PROT
ldt3_0:
        cmp     w[esi],0        ;free entry?
        jz      ldt3_New
        push    ebx
        push    ecx
        mov     bx,[esi]
        call    RawGetSelBase
        shl     ecx,16
        mov     cx,dx
        mov     edx,ecx
        pop     ecx
        pop     ebx
        cmp     edx,ebx         ;same base?
        jz      ldt3_GotOne
        add     esi,2
        loop    ldt3_0
;
;Have to allocate a new descriptor.
;
ldt3_New:
        mov     cx,1
        call    RawGetDescriptors       ;get a descriptor.
        jc      ldt3_9
        push    ebx
        mov     dx,bx
        shr     ebx,16
        mov     cx,bx
        mov     ebx,eax
        call    RawSetSelBase
        mov     dx,-1
        xor     cx,cx
        call    RawSetSelLimit
        pop     ebx
        cmp     esi,offset Real2ProtList + sizeof Real2ProtList
        jae     ldt3_Done
        mov     [esi],ax
        jmp     ldt3_Done
;
;Found a previous selector with right base so use that.
;
ldt3_GotOne:
        mov     ax,[esi]
;
;Now exit with succesful value.
;
ldt3_Done:
        clc
ldt3_9:
        pop     ds
        assume ds:nothing
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        ret
RawReal2ProtSel endp

;--- generic check of selector in BX

checkdesc proc
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax
        test    bl,4
        stc
        jz      done
        cmp     bx, GDT_Entries*8
        jb      done
        movzx   esi,bx          ;Get the selector.
        and     esi,not 7       ;lose RPL & TI.
        add     esi,MDTLinear   ;offset into descriptor table.
        cmp     byte ptr es:[esi+5],1 ;allocated descriptor?
done:
        ret
checkdesc endp

        assume ds:nothing

;-------------------------------------------------------------------------------
;
;Set selector type.
;
;On Entry:-
;
;BX     - Selector.
;CL     - Type.
;CH     - Extended type.
;
RawSetSelType   proc    near
        ;
        push    eax
        push    esi
        push    ds
        push    es
        call    checkdesc
        jc      done
        mov     es:[esi+5],cl
        and     ch,11110000b
        and     BYTE PTR es:[esi+6],00001111b
        or      es:[esi+6],ch
        ;
        clc
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     esi
        pop     eax
        ret
RawSetSelType   endp


;-------------------------------------------------------------------------------
;
;Set selector limit.
;
;On Entry:-
;
;BX     - Selector.
;CX:DX  - Limit.
;
RawSetSelLimit  proc    near
        ;
        push    eax
        push    ecx
        push    esi
        push    ds
        push    es
        call    checkdesc
        jc      done
        shl     ecx,16
        mov     cx,dx
        xor     al,al
        cmp     ecx,100000h     ; see if we need to set g bit
        jc      ldt5_2
        shr     ecx,12          ; div by 4096
        or      al,80h          ; set g bit
ldt5_2:
        mov     es:[esi+0],cx   ;store low word of limit.
        shr     ecx,16
        or      cl,al
        and     BYTE PTR es:[esi+6],01110000b   ;lose limit & G bit.
        or      es:[esi+6],cl           ;store high bits of limit and G bit.
        ;
        clc
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     esi
        pop     ecx
        pop     eax
        ret
RawSetSelLimit  endp


;-------------------------------------------------------------------------------
;
;Set selector linear base address.
;
;On Entry:-
;
;BX     - Selector.
;CX:DX  - Linear base.
;
RawSetSelBase   proc    near
        ;
        push    eax
        push    esi
        push    ds
        push    es
        call    checkdesc
        jc      done
        mov     es:[esi+2],dx   ;base low.
        mov     es:[esi+4],cl   ;base mid.
        mov     es:[esi+7],ch   ;base high.
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     esi
        pop     eax
        ret
RawSetSelBase   endp


;-------------------------------------------------------------------------------
;
;Get selector linear base address.
;
;On Entry:-
;
;BX     - Selector.
;
;On Exit:-
;
;CX:DX  - Linear base.
;
RawGetSelBase   proc    near
        ;
        push    eax
        push    esi
        push    ds
        push    es
        call    checkdesc
        jc      done
        mov     dx,es:[esi+2]   ;base low.
        mov     cl,es:[esi+4]   ;base mid.
        mov     ch,es:[esi+7]   ;base high.
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     esi
        pop     eax
        ret
RawGetSelBase   endp


;-------------------------------------------------------------------------------
;
;Copy buffer into descriptor.
;
;On Entry:-
;
;BX     - Selector.
;ES:[E]DI       - Buffer.
;
RawBPutDescriptor proc near
        ;
        push    eax
        push    esi
        push    edi
        push    ds
        push    es
        call    checkdesc
        jc      done
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        jz      ldt8_Use32
        movzx   edi,di
ldt8_Use32:
        xchg    esi,edi
        pop     ds      ;source in DS:[ESI] now
        push    ds
        cld
        movsd
        movsd
        ;
        clc
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     eax
        ret
RawBPutDescriptor endp


;-------------------------------------------------------------------------------
;
;Copy descriptor into buffer.
;
;On Entry:-
;
;BX     - Selector.
;ES:[E]DI       - Buffer.
;
RawBGetDescriptor proc near
        ;
        push    eax
        push    esi
        push    edi
        push    ds
        push    es
        call    checkdesc
        jc      done
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        jz      ldt9_Use32
        movzx   edi,di
ldt9_Use32:
        pop     es
        push    es
        mov     ax,KernalZero
        mov     ds,eax
        cld
        movsd
        movsd
        ;
        clc
done:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     eax
        ret
RawBGetDescriptor endp
