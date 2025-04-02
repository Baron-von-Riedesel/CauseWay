;
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;

INT33STAT struc
CallBack  dd ?
UserCode  df ?  ;don't change order of UserCode,UserOK,UserMask
UserOK    dw ?
UserMask  dw ?
INT33STAT ends

_DATA32 segment
DbgOut label byte
	align 4
	db 1024 dup (0)
MouseEventStack label byte
Int33Buffer     RealRegsStruc <>
	align 4
MouseEventTab   RealRegsStruc <>
	align 4
int33h INT33STAT <0,0,0,0>
;
OldInt33h       df 0
;
	align 4

_DATA32 ends
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int33hOpen      proc    far
;
;Setup int 33h patch.
;
        assume ds:GROUP32
        assume es:GROUP16
        test    BYTE PTR es:SystemFlags,SF_16BIT
        jz      int331_32bit
        mov     eax,offset Int33hDummy
        mov     w[int33h.UserCode+0],ax
        mov     w[int33h.UserCode+2],cs
        jmp     int331_0bit
int331_32bit:
        mov     d[int33h.UserCode+0],offset Int33hDummy
        mov     w[int33h.UserCode+4],cs
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
        mov     w[int33h.CallBack+0],dx
        mov     w[int33h.CallBack+2],cx
        ;
        ;Set patch vector.
        ;
        mov     bl,33h
        Sys     GetVect
        test    BYTE PTR es:SystemFlags,SF_16BIT
        jz      int331_Use32
        movzx   edx, dx
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
        cmp     d[int33h.CallBack],0
        jz      int332_8
;       mov     ax,0
;       int     33h
        mov     dx,w[int33h.CallBack+0]
        mov     cx,w[int33h.CallBack+2]
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
int333_SwapEvent: ; AX=0014h
        ;Swap event handlers.
        ;
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cmp     int33h.UserOK,0  ;handler installed?
        jnz     int333_se0
        ;
        test    BYTE PTR apiSystemFlags,SF_16BIT
        jz      int333_se1
        xchg    dx,w[int33h.UserCode+0]
        mov     ax,w[int33h.UserCode+2]
        mov     w[int33h.UserCode+2],es
        mov     es,eax
        jmp     int333_se2
int333_se1:
        xchg    edx,d[int33h.UserCode+0]
        mov     ax,w[int33h.UserCode+4]
        mov     w[int33h.UserCode+4],es
        mov     es,eax
int333_se2:
        xchg    cx,[int33h.UserMask]
        push    ecx
        push    edx
        push    es
        mov     cx,[int33h.UserMask]
        push    ds
        pop     es
        assume es:GROUP32
        mov     edi,offset Int33Buffer
        mov     RealRegsStruc.Real_AX[edi],000ch
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     dx,w[int33h.CallBack+0]
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     cx,w[int33h.CallBack+2]
        mov     RealRegsStruc.Real_ES[edi],cx
        mov     bl,33h
        Sys     IntXX
        mov     int33h.UserOK,1  ;enable user routine.
        pop     es
        assume es:nothing
        pop     edx
        pop     ecx
        jmp     int333_se9
        ;
int333_se0:
        test    BYTE PTR apiSystemFlags,SF_16BIT
        jz      int333_se3
        xchg    dx,w[int33h.UserCode+0]
        mov     ax,w[int33h.UserCode+2]
        mov     w[int33h.UserCode+2],es
        mov     ebx,es
        mov     es,eax
        mov     eax,cs
        cmp     ax,bx
        jnz     int333_se4
        mov     int33h.UserOK,0
        mov     w[int33h.UserCode+0],lowword offset Int33hDummy
        mov     w[int33h.UserCode+2],cs
        jmp     int333_se5
int333_se3:
        xchg    edx,d[int33h.UserCode+0]
        mov     ax,w[int33h.UserCode+4]
        mov     w[int33h.UserCode+4],es
        mov     ebx,es
        mov     es,eax
        mov     eax,cs
        cmp     ax,bx
        jnz     int333_se4
        mov     int33h.UserOK,0
        mov     d[int33h.UserCode+0],offset Int33hDummy
        mov     w[int33h.UserCode+4],cs
int333_se5:
        push    ecx
        push    edx
        push    es
        push    ds
        pop     es
        assume es:GROUP32
        mov     edi,offset Int33Buffer
        mov     RealRegsStruc.Real_AX[edi],0000
        mov     bl,33h
        Sys     IntXX
        pop     es
        assume es:nothing
        pop     edx
        pop     ecx
        ;
int333_se4:
        xchg    cx,[int33h.UserMask]
        ;
int333_se9:
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_Reset:
        ;Remove's event handler among other things.
        ;
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     int33h.UserOK,0
        mov     d[int33h.UserCode+0],offset Int33hDummy
        test    BYTE PTR apiSystemFlags,SF_16BIT
        jz      int333_r32
        mov     w[int33h.UserCode+2],cs
        jmp     int333_r0
int333_r32:
        mov     w[int33h.UserCode+4],cs
int333_r0:
        mov     w[int33h.UserMask],0
        pop     ds
        assume ds:nothing
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
        push    ds
        push    es
        push    fs
        push    eax
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
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     bl,33h
        Sys     IntXX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_SetEvent: ; AX=000Ch
        ;Set event handler.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,SF_16BIT
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     int33h.UserOK,0          ;disable old handler.
        jz      int333_Use32Bit51
        mov     w[int33h.UserCode+0],dx
        mov     w[int33h.UserCode+2],es
        jmp     int333_Use16Bit51
int333_Use32Bit51:
        mov     d[int33h.UserCode+0],edx
        mov     w[int33h.UserCode+4],es
int333_Use16Bit51:
        mov     w[int33h.UserMask],cx
        push    ds
        pop     es
        assume es:GROUP32
        mov     edi,offset Int33Buffer
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     dx,w[int33h.CallBack+0]
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     cx,w[int33h.CallBack+2]
        mov     RealRegsStruc.Real_ES[edi],cx
        mov     bl,33h
        Sys     IntXX
        mov     int33h.UserOK,1  ;enable user routine.
        pop     es
        assume es:nothing
        pop     ds
        assume ds:nothing
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
        push    ds
        push    es
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     bl,33h
        Sys     IntXX
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        pop     es
        assume es:nothing
        pop     ds
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        add     ebx,6+2+2        ;leave space for our stuff.
        jmp     int333_Done
        ;
int333_SaveState: ;ax=0016h
        ;Preserve mouse driver state.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        push    fs
        ;
        push    eax
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
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,33h
        Sys     IntXX
        mov     es:RealRegsStruc.Real_AX[edi],0015h   ;get driver storage requirement
        mov     bl,33h
        Sys     IntXX
        movzx   ecx,es:RealRegsStruc.Real_BX[edi]

        pop     es
        assume es:nothing
        pop     edi
        ;
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cld
        mov     esi,offset int33h.UserCode
        movsd   [edi],[esi]   ;UserCode
        movsw   [edi],[esi]
        movsw   [edi],[esi]   ;UserOK
        movsw   [edi],[esi]   ;UserMask
        ;
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        rep     movsb
        pop     fs
        pop     es
        pop     ds
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int333_Done
        ;
int333_ResState: ;ax=0017h
        assume ds:nothing
        ;Restore mouse driver state ES:EDX
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        push    fs
        push    eax
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int333_Use32Bit59
        movzx   edx,dx
int333_Use32Bit59:
        pop     eax
        ;
        push    eax
        push    edx

        push    eax
        mov     esi,edx
        push    es
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     edi,offset Int33Buffer
        mov     es:RealRegsStruc.Real_AX[edi],0015h   ;get driver storage requirement
        mov     bl,33h
        Sys     IntXX
        movzx   ecx,es:RealRegsStruc.Real_BX[edi]
        pop     ds
        pop     eax

        cld
        mov     edi,offset int33h.UserCode
        movsd   [edi],[esi]   ;UserCode
        movsw   [edi],[esi]
        movsw   [edi],[esi]   ;UserOK
        movsw   [edi],[esi]   ;UserMask

        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     fs,PSPSegment
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        assume es:nothing
        xor     edi,edi
        rep     movsb
        ;
        pop     edx
        pop     eax

        mov     edi,offset Int33Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,33h
        Sys     IntXX
        ;
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
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
        pushfd
        pop     eax                             ;get new flags.
        and     ah,11111001b                    ;lose IF & DF.
        test    BYTE PTR apiSystemFlags,SF_16BIT
        jz      int333_Use32Bit8
        push    ebp
        movzx   ebp,sp
        and     [ebp+2*4].IRET16._fl,000011000000000b ;retain IF & DF.
        or      [ebp+2*4].IRET16._fl,ax               ;modify stack flags.
        pop     ebp
        pop     eax
        iret
int333_Use32Bit8:
        and     w [esp+1*4].IRET32._fl,000011000000000b ;retain IF & DF.
        or      w [esp+1*4].IRET32._fl,ax               ;modify stack flags.
        pop     eax
        iretd
        ;
int333_NotOurs:
        ;Not a function recognised by us so pass control to previous handler.
        ;
        jmp     cs:[OldInt33h]                ;pass it onto previous handler.
Int33h  endp

		assume ds:nothing

;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
;--- real-mode callback entry
;--- es:(e)di -> real-mode call struct
;--- ds:(e)si -> real-mode stack
;--- no need to preserve std registers except ES:EDI

MouseEvent      proc    far

        test    BYTE PTR apiSystemFlags,SF_16BIT
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
        cmp     w[int33h.UserOK],0
        jz      int334_Exit
        mov     eax, ss
        mov     ecx, ds
        mov     edx, esp
        cmp     ax, cx
        jz      @F
        mov     ss,ecx
        mov     esp,offset MouseEventStack
@@:
        push    eax       ;save old SS:ESP
        push    edx
        movsx   eax,es:RealRegsStruc.Real_AX[edi]
        movsx   ebx,es:RealRegsStruc.Real_BX[edi]
        movsx   ecx,es:RealRegsStruc.Real_CX[edi]
        movsx   edx,es:RealRegsStruc.Real_DX[edi]
        mov     esi,es:RealRegsStruc.Real_ESI[edi]
        movsx   esi,si
        mov     edi,es:RealRegsStruc.Real_EDI[edi]
        movsx   edi,di
        ;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,SF_16BIT
        jz      int334_Use32Bit
        db 66h
int334_Use32Bit:
        call    FWORD PTR cs:[int33h.UserCode]
        lss     esp,[esp]
int334_Exit:
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,SF_16BIT
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
        test    BYTE PTR apiSystemFlags,SF_16BIT
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

