;
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;

_DATA32 segment
	align 4

MouseEventTab   RealRegsStruc <0>
        db 1024 dup (0)
MouseEventStack label byte
;
OldInt33h       df 0
;
Int33hCallBack  dd 0
Int33hUserCode  df 0
Int33hUserMask  dw 0
Int33hUserOK    dw 0

_DATA32 ends
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int33hOpen      proc    far
;
;Setup int 33h patch.
;
        assume ds:GROUP32
        assume es:GROUP16
        test    BYTE PTR es:SystemFlags,1
        jz      int331_32bit
        mov     eax,offset Int33hDummy
        mov     w[Int33hUserCode+0],ax
        mov     w[Int33hUserCode+2],cs
        jmp     int331_0bit
int331_32bit:
        mov     d[Int33hUserCode+0],offset Int33hDummy
        mov     w[Int33hUserCode+4],cs
int331_0bit:
        ;
        ;Check mouse driver present.
        ;
;       mov     ax,0
;       int     33h
;       cmp     ax,0            ;mouse driver present?
;       jz      @@9
        ;
        ;Get call back.
        ;
        push    ds
        push    es
        push    ds
        pop     es
        mov     esi,offset MouseEvent
        mov     edi,offset MouseEventTab
        push    cs
        pop     ds
        Sys     GetCallBack
        pop     es
        pop     ds
        jc      int331_9
        mov     w[Int33hCallBack+0],dx
        mov     w[Int33hCallBack+2],cx
        ;
        ;Set patch vector.
        ;
        mov     bl,33h
        Sys     GetVect
        test    BYTE PTR es:SystemFlags,1
        jz      int331_Use32
        movzx edx, dx
int331_Use32:
        mov     d[OldInt33h+0],edx
        mov     w[OldInt33h+4],cx
        mov     edx,offset Int33h
        mov     ecx,cs
        mov     bl,33h
        Sys     SetVect
        ;
        assume es:nothing
        assume ds:nothing
int331_9:
        clc
        ;
        ret
Int33hOpen      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int33hClose     proc    far
;
;Remove int 33h patch.
;
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        ;Release CallBack.
        ;
        cmp     d[Int33hCallBack],0
        jz      int332_8
;       mov     ax,0
;       int     33h
        mov     dx,w[Int33hCallBack+0]
        mov     cx,w[Int33hCallBack+2]
        Sys     RelCallBack
        ;
int332_8:
        ;Release interupt vector.
        ;
        cmp     w[OldInt33h+4],0
        jz      int332_9
        mov     edx,d[OldInt33h+0]
        mov     cx,w[OldInt33h+4]
        mov     bl,33h
        Sys     SetVect
int332_9:
        pop     ds
        assume ds:nothing
        ;
        ret
Int33hClose     endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int33h  proc    far
        cmp     ax,0            ;Reset?
        jz      int333_Reset
        cmp     ax,9            ;Set pointer shape?
        jz      int333_SetCursor
        cmp     ax,0ch          ;Set event handler?
        jz      int333_SetEvent
        cmp     ax,14h          ;Swap event handlers?
        jz      int333_SwapEvent
        cmp     ax,15h          ;Get state buffer size?
        jz      int333_GetStateSize
        cmp     ax,16h          ;Save state?
        jz      int333_SaveState
        cmp     ax,17h          ;Restore state?
        jz      int333_ResState
        jmp     int333_NotOurs
        ;
int333_SwapEvent:
        ;Swap event handlers.
        ;
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cmp     Int33hUserOK,0  ;handler installed?
        jnz     int333_se0
        ;
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:GROUP32
;        pop     ds
        jz      int333_se1
        xchg    dx,w[Int33hUserCode+0]
        mov     ax,w[Int33hUserCode+2]
        mov     w[Int33hUserCode+2],es
        mov     es,eax
        jmp     int333_se2
int333_se1:
        xchg    edx,d[Int33hUserCode+0]
        mov     ax,w[Int33hUserCode+4]
        mov     w[Int33hUserCode+4],es
        mov     es,eax
int333_se2:
        xchg    cx,w[Int33hUserMask]
        push    ecx
        push    edx
        push    es
        mov     cx,w[Int33hUserMask]
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_EAX[edi],0ch
        mov     es:RealRegsStruc.Real_ECX[edi],ecx
        mov     dx,w[Int33hCallBack]
        mov     es:RealRegsStruc.Real_EDX[edi],edx
        mov     cx,w[Int33hCallBack+2]
        mov     es:RealRegsStruc.Real_ES[edi],cx
        mov     bl,33h
        Sys     IntXX
        mov     Int33hUserOK,1  ;enable user routine.
        assume es:nothing
        pop     es
        pop     edx
        pop     ecx
        jmp     int333_se9
        ;
int333_se0:
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:GROUP32
;        pop     ds
        jz      int333_se3
        xchg    dx,w[Int33hUserCode+0]
        mov     ax,w[Int33hUserCode+2]
        mov     w[Int33hUserCode+2],es
        mov     ebx,es
        mov     es,eax
        mov     eax,cs
        cmp     ax,bx
        jnz     int333_se4
        mov     Int33hUserOK,0
        push    eax
        mov     eax,offset Int33hDummy
        mov     w[Int33hUserCode+0],ax
        pop     eax
        mov     w[Int33hUserCode+2],cs
        jmp     int333_se5
int333_se3:
        xchg    edx,d[Int33hUserCode+0]
        mov     ax,w[Int33hUserCode+4]
        mov     w[Int33hUserCode+4],es
        mov     ebx,es
        mov     es,eax
        mov     eax,cs
        cmp     ax,bx
        jnz     int333_se4
        mov     Int33hUserOK,0
        mov     d[Int33hUserCode+0],offset Int33hDummy
        mov     w[Int33hUserCode+4],cs
int333_se5:
        push    ecx
        push    edx
        push    es
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     edi,offset Int33Buffer
        mov     w es:RealRegsStruc.Real_EAX[edi],0
        mov     bl,33h
        Sys     IntXX
        assume es:nothing
        pop     es
        pop     edx
        pop     ecx
        ;
int333_se4:
        xchg    cx,w[Int33hUserMask]
        ;
int333_se9:
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        assume ds:nothing
        jmp     int333_Done
        ;
int333_Reset:
        ;Remove's event handler among other things.
        ;
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     Int33hUserOK,0
        mov     d[Int33hUserCode+0],offset Int33hDummy
;        push    ds
;        mov     ds,apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:GROUP32
;        pop     ds
        jz      int333_r32
        mov     w[Int33hUserCode+2],cs
        jmp     int333_r0
int333_r32:
        mov     w[Int33hUserCode+4],cs
int333_r0:
        mov     w[Int33hUserMask],0
        assume ds:nothing
        pop     ds
        jmp     int333_NotOurs          ;carry onto real mode handler.
        ;
int333_SetCursor:
        ;Set graphics cursor shape.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        push    eax
        xor     eax,eax
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int333_Use32Bit50
        movzx   edx,dx
int333_Use32Bit50:
        pop     eax
        mov     esi,edx         ;source buffer.
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        pop     ds
        assume ds:nothing
        cld
        push    ecx
        mov     ecx,64
        rep     movsb           ;copy into transfer buffer.
        pop     ecx
        mov     edi,offset Int33Buffer
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        assume es:nothing
        mov     bl,33h
        Sys     IntXX
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_SetEvent:
        ;Set event handler.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     Int33hUserOK,0          ;disable old handler.
        jz      int333_Use32Bit51
        mov     w[Int33hUserCode+0],dx
        mov     w[Int33hUserCode+2],es
        jmp     int333_Use16Bit51
int333_Use32Bit51:
        mov     d[Int33hUserCode+0],edx
        mov     w[Int33hUserCode+4],es
int333_Use16Bit51:
        mov     w[Int33hUserMask],cx
        assume ds:nothing
        mov     ds,cs:apiDSeg32
        mov     es,cs:apiDSeg16
        assume ds:GROUP32
        assume es:GROUP16
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     es:RealRegsStruc.Real_ECX[edi],ecx
        mov     dx,w[Int33hCallBack]
        mov     es:RealRegsStruc.Real_EDX[edi],edx
        mov     cx,w[Int33hCallBack+2]
        mov     es:RealRegsStruc.Real_ES[edi],cx
        mov     bl,33h
        Sys     IntXX
        mov     Int33hUserOK,1  ;enable user routine.
        assume ds:nothing
        assume es:nothing
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_GetStateSize:
        ; Get state buffer size.
        ;
        push    eax
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        mov     es,cs:apiDSeg16
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     bl,33h
        Sys     IntXX
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        add     ebx,8+2         ;leave space for our stuff.
        jmp     int333_Done
        ;
int333_SaveState:
        ;Preserve mouse driver state.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        ;
        push    eax
        xor     eax,eax
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int333_Use32Bit58
        movzx   edx,dx
int333_Use32Bit58:
        pop     eax
        ;
        push    edx
        push    es
        mov     edi,offset Int33Buffer
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     fs,es:PSPSegment
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        assume es:nothing
        mov     bl,33h
        Sys     IntXX
        mov     es:RealRegsStruc.Real_EAX[edi],15h
        mov     bl,33h
        Sys     IntXX
        mov     ecx,es:RealRegsStruc.Real_EBX[edi]
        movzx   ecx,cx
        pop     es
        pop     edi
        ;
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cld
        mov     eax,d[Int33hUserCode+0]
        stosd   es:[edi]
        mov     ax,w[Int33hUserCode+4]
        stosw   es:[edi]
        mov     ax,[Int33hUserOK]
        stosw   es:[edi]
        mov     ax,[Int33hUserMask]
        stosw   es:[edi]
        ;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        rep     movsb
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_ResState:
        ;Restore mouse driver state.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        push    eax
        xor     eax,eax
        mov     ax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int333_Use32Bit59
        movzx   edx,dx
int333_Use32Bit59:
        pop     eax
        ;
        push    eax
        push    edx
        push    es
        push    eax
        push    edx
        push    es
        mov     es,cs:apiDSeg16
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_EAX[edi],15h
        mov     bl,33h
        Sys     IntXX
        mov     ecx,es:RealRegsStruc.Real_EBX[edi]
        movzx   ecx,cx
        pop     ds
        pop     esi
        pop     eax
        ;
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     fs,es:PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        assume es:nothing
        ;
        push    ds
        push    es
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        cld
        lodsd   [esi]
        mov     DWORD PTR es:[Int33hUserCode+0],eax
        lodsw   [esi]
        mov     WORD PTR es:[Int33hUserCode+4],ax
        lodsw   [esi]
        mov     es:[Int33hUserOK],ax
        lodsw   [esi]
        mov     es:[Int33hUserMask],ax
        assume es:nothing
        pop     es
        pop     ds
        rep     movsb
        ;
        pop     es
        pop     edx
        pop     eax
        mov     edi,offset Int33Buffer
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        assume es:nothing
        mov     bl,33h
        Sys     IntXX
        ;
        pop     fs
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_Done:
        ;Now update stacked flags.
        ;
        push    eax
        push    ebx
        pushf
        pop     ax                      ;get new flags.
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:nothing
;        pop     ds
        jz      int333_Use32Bit8
        mov     bx,sp
        mov     bx,ss:[bx+(4+4)+(2+2)]          ;get original flags.
        jmp     int333_Use16Bit8
int333_Use32Bit8:
        mov     bx,ss:[esp+(4+4)+(4+4)]         ;get original flags.
int333_Use16Bit8:
        and     bx,0000011000000000b            ;retain IF & DF.
        and     ah,11111001b                    ;lose IF & DF.
        or      ax,bx                           ;get old IF.
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:nothing
;        pop     ds
        jz      int333_Use32Bit9
        mov     bx,sp
        mov     ss:[bx+(4+4)+(2+2)],ax          ;modify stack flags.
        jmp     int333_Use16Bit9
int333_Use32Bit9:
        mov     ss:[esp+(4+4)+(4+4)],ax         ;modify stack flags.
int333_Use16Bit9:
        pop     ebx
        pop     eax
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:nothing
;        pop     ds
        jz      int333_Use32Bit10
        iret
int333_Use32Bit10:
        iretd
        ;
int333_NotOurs:
        ;Not a function recognised by us so pass control to previous handler.
        ;
        jmp     FWORD PTR cs:[OldInt33h]                ;pass it onto previous handler.
Int33h  endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
;--- real-mode callback entry
;--- es:(e)di -> real-mode call struct
;--- ds:(e)si -> real-mode stack
;--- no need to preserve std registers except ES:EDI

MouseEvent      proc    far

;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:nothing
;        pop     ds
        jz      int334_start32
        movzx esi, si
        movzx edi, di
int334_start32:
        cld
        lodsw   [esi]
        mov     es:RealRegsStruc.Real_IP[edi],ax
        lodsw   [esi]
        mov     es:RealRegsStruc.Real_CS[edi],ax
        add     es:RealRegsStruc.Real_SP[edi],4
        ;
        push    ds
        push    es
        push    edi
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        cmp     w[Int33hUserOK],0
        jz      int334_Exit
        mov     eax, ss
        mov     ecx, ds
        mov     edx, esp
        cmp     ax, cx
        jz      @F
        mov     ss,ecx
        mov     esp,offset MouseEventStack
@@:
        push    eax
        push    edx
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        movsx   eax,ax
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        movsx   ebx,bx
        mov     ecx,es:RealRegsStruc.Real_ECX[edi]
        movsx   ecx,cx
        mov     edx,es:RealRegsStruc.Real_EDX[edi]
        movsx   edx,dx
        mov     esi,es:RealRegsStruc.Real_ESI[edi]
        movsx   esi,si
        mov     edi,es:RealRegsStruc.Real_EDI[edi]
        movsx   edi,di
        ;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        jz      int334_Use32Bit
        db 66h
int334_Use32Bit:
        call    FWORD PTR cs:[Int33hUserCode]
        lss     esp,[esp]
int334_Exit:
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,1
        pop     edi
        pop     es
        pop     ds
        assume ds:nothing
        jz      int334_Use32Bit2
        iret
int334_Use32Bit2:
        iretd
MouseEvent      endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
Int33hDummy     proc    near
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
;        test    BYTE PTR SystemFlags,1
        test    BYTE PTR apiSystemFlags,1
;        assume ds:nothing
;        pop     ds
        jz      int335_32Bit
        retw
int335_32Bit:
        retf
Int33hDummy     endp


if 0
;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
Bord33  proc    near
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
Bord33  endp
endif

