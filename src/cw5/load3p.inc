;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
LOAD3P struct 4
_Name        df ?
_Environment dw ?
_SystemFlags dd ?
_AutoOffset  dd ?
_Command     df ?
_Handle      dw ?            ;file handle.
_Flags       dd ?
_EntryEIP    dd ?
_EntryESP    dd ?
_SegCS       dw ?
_SegSS       dw ?
_SegMem      dd ?
_RelocMem    dd ?
_ProgBase    dd ?
_BaseSeg     dw ?
_NumSegs     dw ?
_PSP         dd ?
_EntryDS     dd ?
_ExpMem      dd ?
_ModLink     dd ?
_ImpMem      dd ?
_ImpCnt      dd ?
_ImpFlags    dd ?
_ImpType     dd ?
_3PStart     dd ?
_SL_RelocPnt dd ?
_SL_RelocLeft dd ?
_SL_LoadPnt  dd ?
_SL_LoadLeft dd ?
LOAD3P ends

;Load, relocate and execute the application code. 3P format loader.
;
;On Entry:
;
;EBX    - Mode.
;       0 - Normal EXEC.
;       1 - Load for debug.
;       2 - Load for overlay.
;DS:EDX - File name.
;ES:ESI - Command line.
;CX     - Environment selector, 0 to use existing copy.
;
;On Exit:
;
;Carry set on error and AX = error code else,
;
;If Mode=0
;
;AL = ErrorLevel (returned by child's terminate)
;
;If Mode=1
;
;CX:EDX - Entry CS:EIP
;BX:EAX - Entry SS:ESP
;SI     - PSP.
;DI     - Auto DS.
;EBP    - Segment definition memory.
;
;If Mode=2
;
;CX:EDX - Entry CS:EIP
;BX:EAX - Entry SS:ESP
;SI     - PSP.
;
;Error codes:
;
;1      - DOS file access error.
;2      - Not a 3P file.
;3      - Not enough memory.
;
Load3P  proc    near uses ds es fs gs

local   _ebp:dword
local   realseg:dword
local   _dx:word
local   load0:LOAD3P

;--- for Win3x/9x, clear hiword(ebp) if client is 16-bit
        test    b cs:apiSystemFlags,1
        jz      @F
        movzx   ebp,bp
@@:
        mov     d[load0._Name+0],edx
        mov     w[load0._Name+4],ds
        mov     [load0._Flags],ebx
        mov     d[load0._Command+0],esi
        mov     w[load0._Command+4],es
        mov     [load0._Environment],cx
        ;
        xor     eax, eax
        mov     [load0._PSP],eax
        mov     [load0._Handle],ax
        mov     [load0._SegMem],eax
        mov     [load0._RelocMem],eax
        mov     [load0._AutoOffset],eax
        mov     [load0._ExpMem],eax
        mov     [load0._ModLink],eax
        mov     [load0._ImpMem],eax

        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        movzx   eax,RealSegment
        mov     realseg,eax
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
;Try and open the file.
;
        push    ds
        lds     edx,[load0._Name]
        mov     ax,3d00h                            ;open, read only.
        int     21h
        pop     ds
        jc      api89_no_file_error
        mov     [load0._Handle],ax                  ;store the handle.
;
;Check for just a 3P file with no extender.
;
api89_Look3P:
        mov     bx,[load0._Handle]
        mov     edx,offset apiMZHeader              ;somewhere to put the info.
        mov     ecx,size MZHeader
        mov     ah,3fh
        int     21h
        jc      api89_file_error
        cmp     ax,cx
        jnz     api89_file_error
        ;
        cmp     w[apiMZHeader.Signature],'ZM'
        jnz     api89_CheckNew
        ;
        ;Move back to EXE details.
        ;
        mov     ax,[apiMZHeader.NumPages]           ;get length in 512 byte blocks
        cmp     [apiMZHeader._Length],0
        je      medexe3                             ;not rounded if no modulo
        dec     ax                                  ;lose 1 cos its rounded up
medexe3:
        add     ax,ax                               ;mult by 2
        mov     dh,0
        mov     dl,ah
        mov     ah,al
        mov     al,dh                               ;mult by 256=*512
        add     ax,[apiMZHeader._Length]            ;add length mod 512
        adc     dx,0                                ;add any carry to dx
        mov     cx,ax
        xchg    cx,dx                               ;swap round for DOS.
        mov     ax,4200h                            ;set absolute position.
        int     21h
        jmp     api89_Look3P
        ;
api89_CheckNew:
        cmp     w[apiNewHeader],'P3'                ;ID ok?
        jnz     api89_file_error
;
;Check if this is the right module.
;
        cmp     ExecMCount,0
        jz      api89_emc0
        dec     ExecMCount
        mov     edx,d[apiNewHeader.NewSize]
        sub     edx,size NewHeaderStruc
        mov     cx,dx
        shr     edx,16
        xchg    cx,dx
        mov     ax,4201h
        int     21h
        jmp     api89_Look3P
;
;Get file offset and store it.
;
api89_emc0:
        mov     bx,[load0._Handle]
        xor     cx,cx
        xor     dx,dx
        mov     ax,4201h
        int     21h
        shl     edx,16
        mov     dx,ax
        mov     [load0._3PStart],edx
;
;Close the file again.
;
        mov     bx,[load0._Handle]
        mov     ah,3eh
        int     21h
        mov     [load0._Handle],0
;
;Create a new PSP.
;
        push    ds
        mov     ebx,[load0._Flags]
        mov     cx,[load0._Environment]
        les     esi,[load0._Command]
        lds     edx,[load0._Name]
        call    CreatePSP
        pop     ds
        mov     [load0._PSP],ebx
        jc      api89_mem_error
;
;Open the input file again.
;
        push    ds
        lds     edx,[load0._Name]
        mov     ax,3d00h                            ;open, read only.
        int     21h
        pop     ds
        jc      api89_file_error
        mov     [load0._Handle],ax                  ;store the handle.
;
;Move past 3P header again.
;
        mov     edx,[load0._3PStart]
        mov     cx,dx
        shr     edx,16
        xchg    dx,cx
        mov     ax,4200h
        mov     bx,[load0._Handle]
        int     21h
;
;Grab flags.
;
        mov     esi,offset apiNewHeader
        mov     eax,NewHeaderStruc.NewFlags[esi]
        mov     [load0._SystemFlags],eax
        test    b[load0._SystemFlags+3],128         ;compressed?
        jz      api89_noret
        cmp     [load0._Flags],1                    ;debug load?
        jnz     api89_noret
        jmp     api89_file_error
;
;Setup auto stack stuff if its needed.
;
api89_noret:
        mov     esi,offset apiNewHeader
        cmp     NewHeaderStruc.NewEntryESP[esi],0   ;Need an automatic stack?
        jnz     api89_NotAutoESP
        mov     eax,NewHeaderStruc.NewAutoStack[esi] ;Get auto stack size.
        or      eax,eax
        jnz     api89_GotAutoSize
        mov     eax,1024
api89_GotAutoSize:
        mov     NewHeaderStruc.NewEntryESP[esi],eax ;Setup ESP value.
        mov     ebx,NewHeaderStruc.NewAlloc[esi]
        add     NewHeaderStruc.NewAlloc[esi],eax    ;update memory size needed.
        mov     [load0._AutoOffset],ebx             ;store it for later.
;
;Get EXPORT memory.
;
api89_NotAutoESP:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewExports[esi]
        or      ecx,ecx
        jz      api89_NoExports0
        Sys     GetMemLinear32
        jc      api89_mem_error
        mov     [load0._ExpMem],esi
;
;Get IMPORT module link table memory.
;
api89_NoExports0:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewImportModCnt[esi]
        or      ecx,ecx
        jz      api89_NoImports0
        shl     ecx,2                               ;dword per entry.
        add     ecx,4                               ;allow for count dword.
        Sys     GetMemLinear32
        jc      api89_mem_error
        mov     [load0._ModLink],esi
        push    es
        mov     es,realseg
        mov     DWORD PTR es:[esi],0                ;clear entry count for now.
        mov     es,[load0._PSP]
        mov     es:[EPSP_Struc.EPSP_Imports],esi
        pop     es
;
;Get program main memory.
;
api89_NoImports0:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewAlloc[esi]    ;get memory size required.
        Sys     GetMemLinear32                      ;Get segment/selector.
        jc      api89_mem_error                     ;Not enough memory.
        mov     [load0._ProgBase],esi
;
;Get segment definition memory, selectors and details.
;
        mov     esi,offset apiNewHeader
        movzx   ecx,NewHeaderStruc.NewSegments[esi] ;get number of segments.
        cmp     [load0._AutoOffset],0               ;need auto SS?
        jz      api89_NoAutoSeg
        inc     ecx                                 ;one more for the stack.
api89_NoAutoSeg:
        push    ecx
        shl     ecx,3                               ;8 bytes per entry.
        Sys     GetMemLinear32
        pop     ecx
        jc      api89_mem_error
        mov     [load0._SegMem],esi
        Sys     GetSels
        jc      api89_mem_error
        mov     [load0._BaseSeg],bx                 ;store base selector.
        mov     [load0._NumSegs],cx                 ;store number of selectors.
;
;Update programs memory and selector details in PSP and variables.
;
        push    es
        mov     es,[load0._PSP]
        mov     ax,[load0._BaseSeg]                 ;get base selector.
        mov     es:[EPSP_Struc.EPSP_SegBase],ax
        mov     ax,[load0._NumSegs]                 ;get number of selectors.
        shl     ax,3                                ;8 bytes per selector.
        mov     es:[EPSP_Struc.EPSP_SegSize],ax
        mov     eax,[load0._ProgBase]               ;get memory address.
        mov     es:[EPSP_Struc.EPSP_MemBase],eax
        mov     es:[EPSP_Struc.EPSP_NearBase],eax
        mov     edi,offset apiNewHeader
        mov     eax,NewHeaderStruc.NewAlloc[edi]    ;get memory size.
        mov     es:[EPSP_Struc.EPSP_MemSize],eax
        pop     es
;
;Read segment definitions.
;
        test    b[load0._SystemFlags+3],128         ;compressed?
        jz      api89_ncp0
        push    es
        mov     es,realseg
        mov     bx,[load0._Handle]
        mov     edi,[load0._SegMem]
        Sys     cwcLoad                             ;bx=file handle, es:edi=memory to load
        pop     es
        or      ax,ax
        jnz     api89_file_error
        jmp     api89_ncp1
        ;
api89_ncp0:
        mov     esi,offset apiNewHeader
        movzx   ecx,NewHeaderStruc.NewSegments[esi] ;get number of segments.
        shl     ecx,3                               ;8 bytes per entry.
        mov     bx,[load0._Handle]
        mov     edx,[load0._SegMem]
        push    ecx
        push    ds
        mov     ds,realseg
        call    ReadFile
        pop     ds
        pop     ecx
        jc      api89_file_error
        cmp     eax,ecx
        jnz     api89_file_error
;
;Setup a segment definition for auto-stack if needed.
;
api89_ncp1:
        cmp     [load0._AutoOffset],0
        jz      api89_NoAutoMake
        mov     esi,offset apiNewHeader
        movzx   edi,NewHeaderStruc.NewSegments[esi] ;get number of segments.
        mov     NewHeaderStruc.NewEntrySS[esi],di   ;store SS number.
        shl     edi,3                               ;8 bytes per entry.
        add     edi,[load0._SegMem]                 ;offset to space for new entry.
        inc     NewHeaderStruc.NewSegments[esi]     ;update number of segments.
        mov     eax,[load0._AutoOffset]
        mov     es,realseg
        mov     es:[edi+0],eax                      ;setup base.
        mov     eax,NewHeaderStruc.NewEntryESP[esi] ;get limit.
        cmp     eax,100000h
        jc      api89_NoAutoGBit
        shr     eax,12
        or      eax,1 shl 20                        ;set G bit.
api89_NoAutoGBit:
        or      eax,2 shl 21                        ;set class 'stack'
        mov     es:[edi+4],eax                      ;setup limit entry.
;
;Get relocation table memory.
;
api89_NoAutoMake:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewRelocs[esi]   ;get number of relocation entries.
        or      ecx,ecx
        jz      api89_NoRelocsMem
        shl     ecx,2                               ;4 bytes per entry.
        Sys     GetMemLinear32
        jc      api89_mem_error
        mov     [load0._RelocMem],esi
;
;Read relocation entries.
;
        test    b[load0._SystemFlags+3],128         ;compressed?
        jz      api89_ncp2
        push    es
        mov     es,realseg
        mov     bx,[load0._Handle]
        mov     edi,[load0._RelocMem]
        Sys     cwcLoad
        pop     es
        or      ax,ax
        jnz     api89_file_error
        jmp     api89_NoRelocsMem
        ;
api89_ncp2:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewRelocs[esi]   ;get number of relocation entries.
        shl     ecx,2                               ;4 bytes per entry.
        mov     bx,[load0._Handle]
        mov     edx,[load0._RelocMem]
        push    ecx
        push    ds
        mov     ds,realseg
        call    ReadFile
        pop     ds
        pop     ecx
        jc      api89_file_error
        cmp     eax,ecx                             ;did we get enough?
        jnz     api89_file_error
;
;Read export table.
;
api89_NoRelocsMem:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewExports[esi]  ;get length of export table.
        or      ecx,ecx
        jz      api89_NoExpMem
        ;
        test    b[load0._SystemFlags+3],128         ;compressed?
        jz      api89_ncp3
        push    es
        mov     es,realseg
        mov     bx,[load0._Handle]
        mov     edi,[load0._ExpMem]
        Sys     cwcLoad
        pop     es
        or      ax,ax
        jnz     api89_file_error
        jmp     api89_NoExpMem
        ;
api89_ncp3:
        mov     bx,[load0._Handle]
        mov     edx,[load0._ExpMem]
        push    ecx
        push    ds
        mov     ds,realseg
        call    ReadFile
        pop     ds
        pop     ecx
        jc      api89_file_error
        cmp     eax,ecx                             ;did we get enough?
        jnz     api89_file_error
;
;Get IMPORT memory.
;
api89_NoExpMem:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewImports[esi]  ;get size of IMPORT table.
        or      ecx,ecx
        jz      api89_NoImpMem1
        shl     ecx,2                               ;4 bytes per entry.
        Sys     GetMemLinear32
        jc      api89_mem_error
        mov     [load0._ImpMem],esi
;
;Read IMPORT entries.
;
        test    b[load0._SystemFlags+3],128         ;compressed?
        jz      api89_ncp20
        push    es
        mov     es,realseg
        mov     bx,[load0._Handle]
        mov     edi,[load0._ImpMem]
        Sys     cwcLoad
        pop     es
        or      ax,ax
        jnz     api89_file_error
        jmp     api89_NoImpMem1
        ;
api89_ncp20:
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewImports[esi]  ;get size of IMPORT table.
        mov     bx,[load0._Handle]
        mov     edx,[load0._ImpMem]
        push    ecx
        push    ds
        mov     ds,realseg
        call    ReadFile
        pop     ds
        pop     ecx
        jc      api89_file_error
        cmp     eax,ecx                             ;did we get enough?
        jnz     api89_file_error
;
;Read exe image.
;
api89_NoImpMem1:
        mov     esi,offset apiNewHeader
        test    NewHeaderStruc.NewFlags[esi],1 shl 31 ;compressed?
        jz      api89_NotComp
        mov     bx,[load0._Handle]
        push    es
        mov     es,realseg
        mov     edi,[load0._ProgBase]
        Sys     cwcLoad
        pop     es
        or      ax,ax
        jnz     api89_file_error
        jmp     api89_ImageLoaded
        ;
api89_NotComp:
        test    NewHeaderStruc.NewFlags[esi],1 shl 30
        jz      api89_LoadAll
        jmp     api89_LoadVMM
api89_LoadAll:
        mov     edx,[load0._ProgBase]
        mov     ecx,NewHeaderStruc.NewLength[esi]   ;get image length.
        mov     bx,[load0._Handle]
        push    ecx
        push    ds
        mov     ds,realseg
        call    ReadFile                            ;read the file.
        pop     ds
        pop     ecx
        jc      api89_file_error                    ;problems problems.
        cmp     eax,ecx                             ;did we get right amount?
        jnz     api89_file_error
        jmp     api89_ImageLoaded
;
;Break the file read up into 8K chunks that suit the relocations.
;
api89_LoadVMM:
        mov     eax,offset apiNewHeader
        mov     eax,NewHeaderStruc.NewRelocs[eax]   ;get number of relocation entries.
        mov     [load0._SL_RelocLeft],eax
        mov     eax,[load0._RelocMem]
        mov     [load0._SL_RelocPnt],eax
        mov     eax,offset apiNewHeader
        mov     eax,NewHeaderStruc.NewLength[eax]
        mov     [load0._SL_LoadLeft],eax
        mov     eax,[load0._ProgBase]
        mov     [load0._SL_LoadPnt],eax
        mov     [load0._EntryEIP],0
        ;
        mov     es,realseg
api89_sl0:                                          ;<----
        ;Anything left to load?
        ;
        cmp     [load0._SL_LoadLeft],0
        jz      api89_ImageLoaded
        ;
        ;Work out how much we should load.
        ;
        mov     ecx,[load0._SL_LoadLeft]
        cmp     [load0._SL_RelocLeft],0
        jz      api89_sl3
        mov     esi,[load0._SL_RelocPnt]
        mov     eax,es:[esi]                        ;get fixup address.
        and     eax,0FFFFFFFh
        add     eax,4                               ;allow for cross page fixup.
        add     eax,8191
        and     eax,not 8191                        ;get terminal offset.
        mov     ebx,[load0._SL_LoadPnt]
        sub     ebx,[load0._ProgBase]
        sub     eax,ebx                             ;get relative offset.
        cmp     eax,ecx
        jnc     api89_sl1
        mov     ecx,eax
        ;
        ;Load next program chunk.
        ;
api89_sl1:
        or      ecx,ecx
        jnz     api89_sl3

        mov     eax,[load0._EntryEIP]
        or      ebx,-1
        mov     es,ebx

api89_sl3:

        mov     edx,[load0._SL_LoadPnt]
        mov     bx,[load0._Handle]
        push    ecx
        push    ds
        push    es
        pop     ds
        call    ReadFile                            ;read the file.
        pop     ds
        pop     ecx
        jc      api89_file_error                    ;problems problems.
        cmp     eax,ecx                             ;did we get right amount?
        jnz     api89_file_error
        ;
        ;Update variables.
        ;
        add     [load0._SL_LoadPnt],ecx
        sub     [load0._SL_LoadLeft],ecx
        ;
        ;Process all fixups in range of what we loaded.
        ;
        mov     esi,[load0._SL_RelocPnt]
        mov     ecx,[load0._SL_RelocLeft]
        mov     fs,[load0._PSP]
        ;
        mov     bx,[load0._BaseSeg]
        mov     edi,fs:[EPSP_Struc.EPSP_MemBase]
        add     edi,fs:[EPSP_Struc.EPSP_MemSize]
        mov     dx,fs:[EPSP_Struc.EPSP_SegSize]
        shr     dx,3
        mov     _dx,dx
        or      ecx,ecx
        jz      api89_SL_DoneRelocs
api89_SL_RelocLoop:                                 ;<----
        mov     edx,es:[esi]                        ;get linear offset.
        mov     eax,edx
        and     edx,0FFFFFFFh                       ;Lose relocation type.
        shr     eax,28                              ;Get relocation type.
        add     edx,[load0._ProgBase]               ;get linear address.
        cmp     edx,edi
        jnc     api89_file_error2
        or      al,al
        jz      api89_SL_Seg16
        dec     al
        jz      api89_SL_Offset32
        jmp     api89_file_error2
        ;
api89_SL_Offset32:
        mov     eax,edx
        add     eax,4-1
        cmp     eax,[load0._SL_LoadPnt]             ;beyond what we loaded?
        jnc     api89_SL_DoneRelocs
        mov     eax,es:[edx]                        ;Get offset.
        add     eax,[load0._ProgBase]
        cmp     eax,edi                             ;Beyond program?
        jnc     api89_file_error2
        mov     eax,[load0._ProgBase]
        add     DWORD PTR es:[edx],eax              ;Do the relocation.
        jmp     api89_SL_NextReloc
        ;
api89_SL_Seg16:
        mov     eax,edx
        add     eax,2-1
        cmp     eax,[load0._SL_LoadPnt]             ;beyond what we loaded?
        jnc     api89_SL_DoneRelocs
        mov     ax,es:[edx]                         ;get selector number.
        cmp     ax,_dx
        jnc     api89_file_error2
        shl     ax,3                                ;*8 for selector value.
        add     ax,bx                               ;add base selector.
        mov     es:[edx],ax                         ;set segment selector.
        ;
api89_SL_NextReloc:
        add     esi,4                               ;next relocation entry.
        inc     [load0._EntryEIP]
        dec     ecx
        jnz     api89_SL_RelocLoop                  ;---->
        ;
api89_SL_DoneRelocs:
        mov     [load0._SL_RelocPnt],esi
        mov     [load0._SL_RelocLeft],ecx
        mov     esi,offset apiNewHeader
        mov     NewHeaderStruc.NewRelocs[esi],ecx
        jmp     api89_sl0                           ;---->
;
;Close the file now we're done with it.
;
api89_ImageLoaded:
        push    ds
        push    ds
        pop     fs
        pop     es
        ;
        mov     bx,[load0._Handle]                  ;grab handle again,
        mov     [load0._Handle],0
        mov     ah,3eh                              ;and close it.
        int     21h
;
;Process relocations.
;
        mov     esi,offset apiNewHeader
        mov     ecx,NewHeaderStruc.NewRelocs[esi]   ;get number of relocation entries.
        or      ecx,ecx
        jz      api89_DoneReloc
        mov     esi,[load0._RelocMem]
        mov     es,realseg
        mov     fs,[load0._PSP]
        ;
        mov     [load0._EntryEIP],0
        mov     bx,[load0._BaseSeg]
        mov     edi,fs:[EPSP_Struc.EPSP_MemBase]
        add     edi,fs:[EPSP_Struc.EPSP_MemSize]
        mov     dx,fs:[EPSP_Struc.EPSP_SegSize]
        shr     dx,3
        mov     _dx,dx
api89_RelocLoop:                                    ;<----
        mov     edx,es:[esi]                        ;get linear offset.
        mov     eax,edx
        and     edx,0FFFFFFFh                       ;Lose relocation type.
        shr     eax,28                              ;Get relocation type.
        add     edx,[load0._ProgBase]               ;get linear address.
        cmp     edx,edi
        jnc     api89_file_error2
        or      al,al
        jz      api89_Seg16
        dec     al
        jz      api89_Offset32
        jmp     api89_file_error2
        ;
api89_Offset32:
        mov     eax,es:[edx]                        ;Get offset.
        add     eax,[load0._ProgBase]
        cmp     eax,edi                             ;Beyond program?
        jnc     api89_file_error2
        mov     eax,[load0._ProgBase]
        add     DWORD PTR es:[edx],eax              ;Do the relocation.
        jmp     api89_NextReloc
        ;
api89_Seg16:
        mov     ax,es:[edx]                         ;get selector number.
        cmp     ax,_dx
        jnc     api89_file_error2
        shl     ax,3                                ;*8 for selector value.
        add     ax,bx                               ;add base selector.
        mov     es:[edx],ax                         ;set segment selector.
        ;
api89_NextReloc:
        add     esi,4                               ;next relocation entry.
        inc     [load0._EntryEIP]
        dec     ecx
        jnz     api89_RelocLoop                     ;---->

        push    ds
        push    ds
        pop     fs
        pop     es
;
;Set selector details.
;
api89_DoneReloc:
        mov     esi,offset apiNewHeader
        movzx   ecx,NewHeaderStruc.NewSegments[esi] ;get number of segments.
        mov     esi,[load0._SegMem]
        mov     es,realseg
        mov     bx,[load0._BaseSeg]                 ;base selector.
api89_SegLoop:
        push    ebx
        push    ecx
        push    esi
        ;
        mov     eax,es:[esi+4]                      ;Get limit.
        mov     ecx,eax
        and     ecx,0fffffh                         ;mask to 20 bits.
        test    eax,1 shl 20                        ;G bit set?
        jz      api89_NoGBit
        shl     ecx,12
        or      ecx,4095
api89_NoGBit:
        or      ecx,ecx
        jz      api89_NoDecLim
        cmp     ecx,-1
        jz      api89_NoDecLim
        dec     ecx
api89_NoDecLim:
        mov     edx,es:[esi]                        ;get base.
        ;
        test    eax,1 shl 27                        ;FLAT segment?
        jz      api89_NotFLATSeg
        ;
        push    fs
        mov     fs,[load0._PSP]
        mov     fs:[EPSP_Struc.EPSP_NearBase],0     ;Make sure NEAR functions work.
        pop     fs
        ;
        add     edx,[load0._ProgBase]
        or      ecx,-1                              ;Update the limit.
        xor     edx,edx
        jmp     api89_DoSegSet
        ;
api89_NotFLATSeg:
        add     edx,[load0._ProgBase]               ;offset within real memory.
        ;
api89_DoSegSet:
        Sys     SetSelDet32
        mov     cx,w[load0._SystemFlags]            ;use default setting.
        shr     cx,14
        xor     cl,1
        or      cl,b[load0._SystemFlags+2]
        and     cl,1
        call    _DSizeSelector
        ;
        mov     eax,es:[esi+4]                      ;Get class.
        shr     eax,21                              ;move type into useful place.
        and     eax,0fh                             ;isolate type.
        or      eax,eax
        jz      api89_CodeSeg
        jmp     api89_SegDone
        ;
api89_CodeSeg:
        mov     eax,es:[esi+4]                      ;Get type bits.
        mov     cx,0                                ;Set 16 bit seg.
        test    eax,1 shl 25
        jnz     api89_Default
        mov     cx,1
        test    eax,1 shl 26                        ;32 bit seg?
        jnz     api89_Default
        mov     cx,w[load0._SystemFlags]            ;use default setting.
        shr     cx,14
        and     cx,1                                ;get code size
        xor     cx,1                                ;flip it for selector function.
api89_Default:
        Sys     CodeSel
        ;
api89_SegDone:
        pop     esi
        pop     ecx
        pop     ebx
        add     esi,8                               ;next definition.
        add     ebx,8                               ;next selector.
        dec     ecx
        jnz     api89_SegLoop
;
;Update export table values.
;
        cmp     [load0._ExpMem],0
        jz      api89_exp9
        push    es
        mov     es,[load0._PSP]
        mov     esi,[load0._ExpMem]
        mov     edi,[load0._SegMem]
        mov     es:[EPSP_Struc.EPSP_Exports],esi
        mov     dx,es:[EPSP_Struc.EPSP_SegBase]
        mov     es,realseg
        mov     ecx,es:[esi]
        mov     _ebp,esi
        add     esi,4
        ;
        ;Do module name entry.
        ;
;       add     es:[esi],ebp                        ;turn offset into an address.
        mov     eax,_ebp
        add     es:[esi],eax                        ;turn offset into an address.
        add     esi,4
        ;
        ;Do all other entries.
        ;
api89_exp0:                                         ;<----
;       add     es:[esi],ebp                        ;turn offset into an address.
        mov     eax,_ebp
        add     es:[esi],eax                        ;turn offset into an address.
        mov     ebx,es:[esi]                        ;get address of export entry.
        movzx   eax,WORD PTR es:[ebx+4]             ;get segment def.
        add     WORD PTR es:[ebx+4],dx              ;update seg
        shl     eax,3
        add     eax,edi                             ;point into seg defs.
        test    DWORD PTR es:[eax+4],1 shl 27       ;FLAT seg?
        jz      api89_exp1
        mov     eax,[load0._ProgBase]
        add     DWORD PTR es:[ebx],eax              ;update to reflect FLAT
api89_exp1:
        add     esi,4
        dec     ecx
        jnz     api89_exp0                          ;---->
        pop     es
api89_exp9:
;
;Setup entry regs.
;
        mov     esi,offset apiNewHeader
        ;
        mov     ebx,NewHeaderStruc.NewEntryEIP[esi] ;get entry address.
        movzx   eax,NewHeaderStruc.NewEntryCS[esi]
        shl     eax,3
        add     ax,[load0._BaseSeg]
        mov     [load0._SegCS],ax
        sub     ax,[load0._BaseSeg]
        add     eax,[load0._SegMem]                 ;point to this segments entry.
        test    DWORD PTR es:[eax+4],1 shl 27       ;FLAT segment?
        jz      api89_NotFLATEIP
        add     ebx,es:[eax]                        ;include segments offset within image.
        add     ebx,[load0._ProgBase]
api89_NotFLATEIP:
        mov     [load0._EntryEIP],ebx
        ;
        mov     ebx,NewHeaderStruc.NewEntryESP[esi]
        movzx   eax,NewHeaderStruc.NewEntrySS[esi]
        shl     eax,3
        add     ax,[load0._BaseSeg]
        mov     [load0._SegSS],ax
        sub     ax,[load0._BaseSeg]
        add     eax,[load0._SegMem]                 ;point to this segments entry.
        test    DWORD PTR es:[eax+4],1 shl 27       ;FLAT segment?
        jz      api89_NotFLATESP
        add     ebx,es:[eax]                        ;include segments offset within image.
        add     ebx,[load0._ProgBase]
api89_NotFLATESP:
        mov     [load0._EntryESP],ebx
        ;
        mov     eax,[load0._PSP]
        cmp     NewHeaderStruc.NewAutoDS[esi],0
        jz      api89_NotAUTODS
        movzx   eax,NewHeaderStruc.NewAutoDS[esi]
        dec     eax
        shl     eax,3
        add     ax,[load0._BaseSeg]
api89_NotAUTODS:
        mov     [load0._EntryDS],eax
;
;Convert import module names to pointers to export tables.
;
        cmp     [load0._ImpMem],0                   ;any imports?
        jz      api89_imp9
        mov     [load0._ImpCnt],0                   ;Clear the counter.
        mov     es,realseg
api89_imp0:
        mov     esi,[load0._ImpMem]
        add     esi,DWORD PTR es:[esi]              ;move to module name list.
        mov     eax,[load0._ImpCnt]
        cmp     eax,DWORD PTR es:[esi]              ;Done all modules?
        jnc     api89_imp6
        add     esi,es:[esi+4+eax*4]                ;Point to this module name.
        ;
        ;Try and find the module.
        ;
        call    FindModule
        jc      api89_findmodule_error
        ;
        ;Update the module link table and usage count.
        ;
        mov     esi,[load0._ModLink]
        inc     DWORD PTR es:[esi]                  ;update count.
        mov     eax,[load0._ImpCnt]
        mov     es:[esi+4+eax*4],edi                ;store EPSP link pointer.
        ;
        ;Move onto next module name.
        ;
        inc     [load0._ImpCnt]
        jmp     api89_imp0
;
;Now it's time to work through the relocations performing the fixups.
;
api89_imp6:
        mov     esi,[load0._ImpMem]
        add     esi,DWORD PTR es:[esi+8]            ;move to fixup list.
        mov     eax,es:[esi]                        ;get number of entries.
        or      eax,eax
        jz      api89_imp9
        mov     [load0._ImpCnt],eax
        add     esi,4
api89_imp7:
        xor     eax,eax
        mov     al,es:[esi]
        mov     [load0._ImpType],eax
        mov     eax,es:[esi+1]
        mov     [load0._ImpFlags],eax
        add     esi,1+4
        ;
        ;Point to the right modules export list.
        ;
        mov     ecx,[load0._ImpFlags]                ;get flags
        shr     ecx,30
        xor     eax,eax
api89_imp8:
        shl     eax,8
        mov     al,es:[esi]
        inc     esi
        dec     ecx
        jnz     api89_imp8
        mov     edi,[load0._ModLink]
        mov     edi,es:[edi+4+eax*4]
        mov     edi,es:EPSP_Struc.EPSP_Exports[edi]
        ;
        ;Point to the import name string.
        ;
        mov     ecx,[load0._ImpFlags]
        shr     ecx,28
        and     ecx,3
        xor     eax,eax
api89_imp10:
        shl     eax,8
        mov     al,es:[esi]
        inc     esi
        dec     ecx
        jnz     api89_imp10
        ;
        ;Ordinal or string import?
        ;
        test    [load0._ImpType],128                ;ordinal?
        jz      api89_imp14
        cmp     es:[edi],eax                        ;check number of entries.
        jc      api89_file_error
        mov     edi,es:[edi+4+eax*4]
        jmp     api89_imp11
        ;
api89_imp14:
        mov     edx,[load0._ImpMem]
        add     edx,es:[edx+4]                      ;point to name list.
        add     edx,es:[edx+4+eax*4]                ;point to name string.
        ;
        ;Try and find name string in export list.
        ;
        push    ebp
        mov     ebp,edx
        call    FindFunction
        pop     ebp
        jc      api89_file_error
        ;
api89_imp11:
        ;Found the right entry, now do the fixup.
        ;
        mov     ebx,[load0._ImpFlags]
        and     ebx,0FFFFFFFh
        add     ebx,[load0._ProgBase]
        mov     edx,es:[edi]
        xor     ecx,ecx
        mov     cx,es:[edi+4]
        mov     eax,[load0._ImpType]
        ;
        ;Check if this is self relative.
        ;
        test    eax,64
        jz      api89_imp15
        and     eax,63
        mov     edx,ebx                             ;must be flat.
        cmp     eax,1
        jz      api89_imp18
        mov     edx,[load0._ImpFlags]
        and     edx,0FFFFFFFh
api89_imp18:
        add     edx,2
        test    eax,1
        jz      api89_imp16
        add     edx,2
api89_imp16:
        sub     edx,es:[edi]
        neg     edx
api89_imp15:
        and     eax,63
        or      eax,eax
        jz      api89_imp_offset16
        dec     eax
        jz      api89_imp_offset32
        dec     eax
        jz      api89_imp_pointer16
        dec     eax
        jz      api89_imp_pointer32
        dec     eax
        jz      api89_imp_base
        jmp     api89_file_error
        ;
        ;Do a 16-bit offset fixup.
        ;
api89_imp_offset16:
        mov es:[ebx],dx
        jmp     api89_imp17
        ;
        ;Do a 32-bit offset fixup.
        ;
api89_imp_offset32:
        mov es:[ebx],edx
        jmp     api89_imp17
        ;
        ;Do a 16-bit pointer fixup.
        ;
api89_imp_pointer16:
        mov        es:[ebx],dx
        mov     es:[ebx+2],cx
        jmp     api89_imp17
        ;
        ;Do a 32-bit pointer fixup.
        ;
api89_imp_pointer32:
        mov        es:[ebx],edx
        mov     es:[ebx+4],cx
        jmp     api89_imp17
        ;
        ;Do a base fixup.
        ;
api89_imp_base:
        mov     es:[ebx],cx
        ;
api89_imp17:
        dec     [load0._ImpCnt]
        jnz     api89_imp7
api89_imp9:
;
;Lose relocation memory.
;
        mov     esi,[load0._RelocMem]
        Sys     RelMemLinear32                      ;release relocation list memory.
        mov     [load0._RelocMem],0
;
;Lose import memory.
;
        mov     esi,[load0._ImpMem]                 ;release IMPORT memory.
        or      esi,esi
        jz      api89_NoRelImp9
        Sys     RelMemLinear32
        mov     [load0._ImpMem],0
;
;Lose segment definitions.
;
api89_NoRelImp9:
        cmp     [load0._Flags],1
        jz      api89_NoSegMemRel
        mov     esi,[load0._SegMem]
        Sys     RelMemLinear32                      ;release segment memory.
        mov     [load0._SegMem],0
;
;Check if this is an exec or just a load.
;
api89_NoSegMemRel:
        cmp     [load0._Flags],0
        jz      api89_Exec
;
;Switch back to parents PSP if this is a debug load.
;
        cmp     [load0._Flags],2
        jz      api89_NoPSwitch2
        push    fs
        mov     fs,[load0._PSP]
        mov     bx,fs:[EPSP_Struc.EPSP_Parent]
        pop     fs
        mov     ah,50h          ;set PSP
        int     21h
;        mov     ebp,[load0._SegMem]
;--- this is to be returned by the "real" EBP register        
        mov     eax,[load0._SegMem]
        mov     [ebp+0],eax
;
;Return program details to caller.
;
api89_NoPSwitch2:
        mov     edx,[load0._EntryEIP]
        mov     cx,[load0._SegCS]
        mov     eax,[load0._EntryESP]
        mov     bx,[load0._SegSS]
        mov     esi,[load0._PSP]
        mov     edi,[load0._EntryDS]
        clc
        jmp     api89_exit
;
;Run it.
;
api89_Exec:
        mov     eax,[load0._Flags]
        mov     ebx,[load0._EntryEIP]
        mov     cx,[load0._SegCS]
        mov     edx,[load0._EntryESP]
        mov     si,[load0._SegSS]
        mov     edi,[load0._PSP]
        push    ebp
        mov     ebp,[load0._EntryDS]
        call    ExecModule
        pop     ebp
        clc
;
;Shut down anything still hanging around.
;
api89_error:
        pushfd
        push    eax
;
;Make sure file is closed.
;
        pushfd
        xor     bx,bx
        xchg    bx,[load0._Handle]
        or      bx,bx
        jz      api89_NoClose
        mov     ah,3eh
        int     21h
;
;Make sure all work spaces are released.
;
api89_NoClose:
        mov     esi,[load0._RelocMem]
        or      esi,esi
        jz      api89_NoRelRel
        Sys     RelMemLinear32                      ;release relocation list memory.
        mov     [load0._RelocMem],0
api89_NoRelRel:
        mov     esi,[load0._SegMem]
        or      esi,esi
        jz      api89_NoSegRel
        Sys     RelMemLinear32                      ;release segment memory.
        mov     [load0._SegMem],0
api89_NoSegRel:
        mov     esi,[load0._ImpMem]
        or      esi,esi
        jz      api89_NoImpRel
        Sys     RelMemLinear32
;
;Restore previous state.
;
api89_NoImpRel:
        popfd
        jnc     api89_RelPSP
        cmp     [load0._PSP],0
        jz      api89_NoRelRes
;
;Restore vectors & DPMI state.
;
api89_RelPSP:
        push    ds
        pop     gs
        push    ds
        pop     fs
        push    ds
        pop     es
        mov     eax,[load0._Flags]
        mov     ebx,[load0._PSP]
        call    DeletePSP
;
;Return to caller.
;
api89_NoRelRes:
        pop     eax
        popfd
        ;
api89_exit:
        ;
        ret
;
;Not enough memory error.
;
api89_mem_error:
        mov     ax,3
        stc
        jmp     api89_error
;
;Couldn't find the file.
;
api89_no_file_error:
        mov        ax,1
        stc
        jmp     api89_error
;
;Not a 3P file.
;
api89_file_error:
        mov     ax,2
        stc
        jmp     api89_error
;
;Corrupt file.
;
api89_file_error2:
        mov     eax,[load0._EntryEIP]               ;Get the relocation number.
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     b[ErrorM11_0+0]," "
        mov     b[ErrorM11_0+1]," "
        mov     b[ErrorM11_0+2]," "
        mov     cl,8
        mov     edi,offset ErrorM11_1
        call    Bin2Hex
        pop     ds
        assume ds:GROUP32
        mov     ax,2
        stc
        jmp     api89_error

api89_findmodule_error:        
        ;
        ;Put module's name into command line error message.
        ;
        ;ES:ESI is the name string.
        ;
        push    ds
        push    eax
        mov     ds,apiDSeg16
        assume ds:GROUP16
        cmp     ax,1
        jnz     api89_egm1
        mov     edi,offset ErrorM10_T
        movzx   ecx,BYTE PTR es:[esi]
        inc     esi
        mov     b[edi],'"'
        inc     edi
api89_egm0:
        mov     al,es:[esi]
        mov     [edi],al
        inc     esi
        inc     edi
        loop    api89_egm0
        mov     b[edi+0],'"'
        mov     b[edi+1],13
        mov     b[edi+2],10
        mov     b[edi+3],"$"
api89_egm1:
        pop     eax
        pop     ds
        assume ds:GROUP32
        stc
        jmp     api89_error

Load3P  endp

        assume ds:nothing
