;
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;

_DATA32 segment
	align 4

OldInt10h       df 0
Int10hStaticSel dw ?
Int10hUltraFont dw ?
	align 4
Int10Buffer     RealRegsStruc <>
_DATA32 ends
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

Int10hOpen      proc    far
;
;Setup int 10h patch.
;
        assume ds:GROUP32
        assume es:GROUP16
;        mov     Int10hDSeg,es   ;store GROUP16 data selector.
;        mov     Int10hDDSeg,ds
        ;
        Sys     GetSel          ;Get a selector
        jc      int101_9
        mov     Int10hStaticSel,bx
        Sys     GetSel
        jc      int101_9
        mov     Int10hUltraFont,bx
        ;
        mov     bl,10h
        Sys     GetVect
        test    BYTE PTR es:SystemFlags,1
        jz      @F
        movzx   edx, dx
@@:
        mov     d[OldInt10h+0],edx
        mov     w[OldInt10h+4],cx
        mov     edx,offset Int10h
        mov     ecx,cs
        mov     bl,10h
        Sys     SetVect
        assume es:nothing
        assume ds:nothing
        clc
int101_9:
        ret
Int10hOpen      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int10hClose     proc    far
;
;Remove int 10h patch.
;
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cmp     w[OldInt10h+4],0
        jz      int102_9
        mov     edx,d[OldInt10h+0]
        mov     cx,w[OldInt10h+4]
        mov     bl,10h
        Sys     SetVect
int102_9:
        pop     ds
        assume ds:nothing
        ;
        ret
Int10hClose     endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Int10h  proc    far
        cmp     ax,1002h                ;set pen numbers?
        jz      int103_SetPens
        cmp     ax,1009h                ;get pen numbers?
        jz      int103_GetPens
        cmp     ax,1012h                ;set colour values?
        jz      int103_SetColours
        cmp     ax,1017h                ;get colour values?
        jz      int103_GetColours
        cmp     ax,1100h                ;Set font?
        jz      int103_SetFont1
        cmp     ax,1110h                ;Set font and program CRTC?
        jz      int103_SetFont1
        cmp     ax,1130h                ;Get font details?
        jz      int103_GetFont
        cmp     ah,13h                  ;write teletype string?
        jz      int103_WriteString
        cmp     ah,1bh                  ;state info?
        jz      int103_VideoState
        cmp     ax,1c00h
        jz      int103_StateSize
        cmp     ax,1c01h                ;state save?
        jz      int103_StateSave
        cmp     ax,1c02h                ;state restore?
        jz      int103_StateRestore
        cmp     ax,0cd00h               ;Ultra set palette?
        jz      int103_UltraSetPal
        cmp     ax,0cd02h               ;Ultra get palette?
        jz      int103_UltraGetPal
        cmp     ax,0cd10h               ;Ultra set font?
        jz      int103_UltraSetFont
int103_NotOurs:
        ;Not a function recognised by us so pass control to previous handler.
        ;
        jmp     FWORD PTR cs:[OldInt10h]                ;pass it onto previous handler.
        ;
int103_GetFont:
        ;Get font details? returns ptr in es:ebp
        ;
        push    eax
        push    ebx
        push    edi
        push    ds
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     bl,10h
        Sys     IntXX
        mov     bx,es:[edi].RealRegsStruc.Real_ES
        mov     ax,0002h
        int     31h
        jc      int103_9
        movzx   ebp,es:[edi].RealRegsStruc.Real_BP
        mov     dl,b es:[edi].RealRegsStruc.Real_DX
        mov     cx,es:[edi].RealRegsStruc.Real_CX
        mov     es,eax
        assume es:nothing
int103_9:
        pop     ds
        pop     edi
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_UltraSetFont:
        ;UltraVision set font. DS:ESI->8-byte font name
        ;
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit503
        movzx   ebp,bp
int103_Use32Bit503:
        mov     eax,ds
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit504
        movzx   esi,si
int103_Use32Bit504:
        pop     eax

        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        pop     ds
        assume ds:nothing

        xor     edi,edi
        cld
        movsd                   ;copy 8 bytes into transfer buffer.
        movsd

        push    eax
        push    ecx
        push    edx
        movzx   ax,bh
        mul     cx
        movzx   ecx,ax
        mov     esi,ebp         ;source buffer.
        push    es
        pop     ds
        mov     edi,8           ;offset 8 in transfer buffer
        cld
        rep     movsb           ;copy into transfer buffer.
        pop     edx
        pop     ecx
        pop     eax

        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     es:[edi].RealRegsStruc.Real_SI,0
        mov     es:[edi].RealRegsStruc.Real_BP,8
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_DS,ax
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        mov     eax,es:[edi].RealRegsStruc.Real_EAX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        jmp     int103_Done
        ;
int103_UltraGetPal:
        ;UltraVision get palette.
        ;
        push    eax
        push    ebx
        push    ecx
        push    edi
        push    ebp
        push    es
        push    fs
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
;        mov     fs,PSPSegment
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     bl,10h
        Sys     IntXX           ;read pen values.
;        mov     ds,cs:apiDSeg32
;        assume ds:GROUP32
        mov     bx,Int10hUltraFont
        movzx   edx,es:[edi].RealRegsStruc.Real_DS
        shl     edx,4
        mov     ecx,65535
        Sys     SetSelDet32
        mov     ds,ebx
        assume ds:nothing
        movzx   edx,es:[edi].RealRegsStruc.Real_DX
        movzx   esi,w es:[edi].RealRegsStruc.Real_ESI
        pop     fs
        pop     es
        assume es:nothing
        pop     ebp
        pop     edi
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_UltraSetPal:
        ;UltraVision set palette.
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
        mov     eax,ds
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit502
        movzx   edx,dx
int103_Use32Bit502:
        pop     eax
        mov     esi,edx         ;source buffer.
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        pop     ds
        assume ds:nothing
        cld
        mov     ecx,16
        rep     movsb           ;copy into transfer buffer.
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_DS,ax
        mov     bl,10h
        Sys     IntXX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_VideoState:
        ;Some sort of video state table function.
        ;
        or      bx,bx
        jnz     int103_NotOurs
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit501
        movzx   edi,di
int103_Use32Bit501:
        pop     eax
        push    edi
        push    es
        ;
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX           ;read pen values.
        mov     eax,es:[edi].RealRegsStruc.Real_EAX
        pop     es
        assume es:nothing
        pop     edi
        cmp     al,1bh
        jz      int103_ok100
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
        xor     eax,eax
        jmp     int103_Done
int103_ok100:
        push    eax
        push    edi
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        mov     ecx,64
        cld
        rep     movsb           ;copy the results.
        pop     edi
        movzx   edx,WORD PTR es:[edi+2]
        shl     edx,4
        mov     ecx,65535
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     bx,Int10hStaticSel
        mov     WORD PTR es:[edi+2],bx
        Sys     SetSelDet32
        pop     eax
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
        jmp     int103_Done
        ;
int103_SetFont1:
        ;Load user font into character generator ram. ES:EBP
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit500
        movzx   ebp,bp
int103_Use32Bit500:
        pop     eax

        push    eax
        push    ecx
        push    edx
        movzx   ax,bh
        mul     cx
        movzx   ecx,ax
        mov     esi,ebp         ;source buffer.
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        pop     ds
        assume ds:nothing
        cld
        rep     movsb           ;copy into transfer buffer.
        pop     edx
        pop     ecx
        pop     eax

        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     w es:[edi].RealRegsStruc.Real_EBP,0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_SetPens:
        ;ES:DX - List of 17 bytes to send to video.
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit50
        movzx   edx,dx
int103_Use32Bit50:
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
        mov     ecx,17
        rep     movsb           ;copy into transfer buffer.
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_AX,1002h
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_GetPens:
        ;ES:DX - Space for list of 17 pen numbers.
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit51
        movzx   edx,dx
int103_Use32Bit51:
        pop     eax
        push    edx
        push    es
        ;
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_AX,1009h
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX           ;read pen values.
        pop     es
        assume es:nothing
        pop     edi
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        mov     ecx,17
        cld
        rep     movsb           ;copy the results.
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
        jmp     int103_Done
        ;
int103_SetColours:
        ;ES:DX - list of RGB values.
        ;CX    - number of values.
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
        jnc     int103_Use32Bit52
        movzx   edx,dx
int103_Use32Bit52:
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
        movzx   ecx,cx
        mov     eax,ecx
        add     ecx,ecx
        add     ecx,eax         ;3 bytes per entry.
        rep     movsb           ;copy into transfer buffer.
        pop     ecx
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_AX,1012h
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
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
        jmp     int103_Done
        ;
int103_GetColours:
        ;ES:EDX - Buffer for list of RGB values.
        ;CX    - Number of values.
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
        jnc     int103_Use32Bit53
        movzx   edx,dx
int103_Use32Bit53:
        pop     eax
        push    ecx
        push    edx
        push    es
        ;
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edx,edx
        mov     es:[edi].RealRegsStruc.Real_AX,1017h
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX           ;read pen values.
        pop     es
        assume es:nothing
        pop     edi
        pop     ecx
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        movzx   ecx,cx
        mov     eax,ecx
        add     ecx,ecx
        add     ecx,eax         ;3 bytes per entry.
        cld
        rep     movsb           ;copy the results.
        pop     fs
        pop     es
        pop     ds
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_WriteString:
        ;ES:BP - String to write.
        ;CX    - Number of characters to write.
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
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int103_Use32Bit54
        movzx   ebp,bp
int103_Use32Bit54:
        pop     eax
        mov     esi,ebp         ;source buffer.
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        pop     ds
        assume ds:nothing
        cld
        push    cx
        cmp     al,2
        jc      int103_noatts
        add     cx,cx
int103_noatts:
        movzx   ecx,cx
        rep     movsb           ;copy into transfer buffer.
        pop     cx
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     ebp,ebp
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
;       mov     es:[edi].RealRegsStruc.Real_EBP,edx
        mov     es:[edi].RealRegsStruc.Real_EBP,ebp

        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_EDX,edx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        pop     fs
        pop     es
        assume es:nothing
        pop     ds
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        ;
int103_StateSize:
        ;Return state size.
        ;
        push    eax
        push    ecx
        push    edi
        push    es
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     bl,10h
        Sys     IntXX
        mov     ebx,es:RealRegsStruc.Real_EBX[edi]
        cmp     bx,2048/64
        jc      int103_ss3
        ;
        xor     bx,bx
        test    cl,1
        jz      int103_ss0
        add     bx,0c0h
int103_ss0:
        test    cl,2
        jz      int103_ss1
        add     bx,0c0h
int103_ss1:
        test    cl,4
        jz      int103_ss2
        add     bx,0380h
int103_ss2:
        add     bx,63
        shr     bx,6
int103_ss3:
        pop     es
        assume es:nothing
        pop     edi
        pop     ecx
        pop     eax
        jmp     int103_Done
        ;
int103_StateSave:
        ;Save state function. ES:EBX
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
        jnc     int103_Use32Bit58
        movzx   ebx,bx
int103_Use32Bit58:
        pop     eax

        push    eax
        push    ebx
        push    es
        ;
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     ebx,ebx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        ;
        pop     es
        pop     ebx
        pop     eax
        assume es:nothing
        ;
        push    eax
        push    ebx
        mov     ax,1c00h
        int     10h             ;Get buffer size.
        movzx   ecx,bx          ;# of 64-byte blocks
        pop     ebx
        pop     eax

        shl     ecx,6           ;*64
        mov     edi,ebx         ;destination buffer.
        xor     esi,esi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        cld
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
        jmp     int103_Done
        ;
int103_StateRestore:
        ;Restore state function. ES:EBX
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
        jnc     int103_Use32Bit59
        movzx   ebx,bx
int103_Use32Bit59:
        pop     eax
        ;
        push    eax
        push    ebx
        push    ecx
        push    es

        push    eax
        push    ebx
        mov     ax,1c00h
        int     10h             ;Get buffer size.
        movzx   ecx,bx          ;# of 64-byte blocks
        pop     ebx
        pop     eax

        shl     ecx,6           ;*64
        mov     esi,ebx         ;source buffer.
        push    es
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        xor     edi,edi
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        pop     ds
        cld
        rep     movsb
        pop     es
        pop     ecx
        pop     ebx
        pop     eax
        ;
        mov     edi,offset Int10Buffer
        mov     es,cs:apiDSeg32
        assume es:GROUP32
        xor     ebx,ebx
        mov     es:[edi].RealRegsStruc.Real_EAX,eax
        mov     es:[edi].RealRegsStruc.Real_EBX,ebx
        mov     es:[edi].RealRegsStruc.Real_ECX,ecx
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[edi].RealRegsStruc.Real_ES,ax
        mov     bl,10h
        Sys     IntXX
        ;
        pop     fs
        pop     es
        pop     ds
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     int103_Done
        assume es:nothing
        ;
int103_Done:
        ;Now update stacked flags.
        ;
        push    eax
        pushfd
        pop     eax
        and     ah,11111001b                    ;lose IF & DF.
        test    BYTE PTR apiSystemFlags,1
        jz      int103_Use32Bit8
        push    ebp
        movzx   ebp,sp
        and     [ebp+2*4].IRET16._fl,0000011000000000b  ;retain IF & DF.
        or      [ebp+2*4].IRET16._fl,ax
        pop     ebp
        pop     eax
        iret
int103_Use32Bit8:
        and     w [esp+1*4].IRET32._fl,0000011000000000b;retain IF & DF.
        or      w [esp+1*4].IRET32._fl,ax
        pop     eax
        iretd
        ;
Int10h  endp
;

