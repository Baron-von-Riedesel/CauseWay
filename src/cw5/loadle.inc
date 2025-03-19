;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;LE object format.

;
LE_OBJ          struc
LE_OBJ_Size             dd ?    ;virtual size in bytes.
LE_OBJ_Base             dd ?    ;relocation base address.
;
LE_OBJ_Flags            dd ?    ;object flags.
LE_OBJ_PageIndex        dd ?    ;page map index.
LE_OBJ_PageNum          dd ?    ;page map entries.
LE_OBJ_Reserved db 4 dup (0)    ;reserved.
LE_OBJ          ends

LOADLE struct 4
_Name        df ?
_EntryES     dw ?
_Command     df ?
_Environment dw ?
_Handle      dd ?
_Flags       dd ?
_LEOffset    dd ?
_ProgMem     dd ?,?
_BaseSeg     dw ?     ;_BaseSeg & _NumSegs must be consecutive
_NumSegs     dw ?
_ObjMem      dd ?
_FixupMem    dd ?
_ObjCount    dd ?
_ObjBase     dd ?
_PageCount   dd ?,?
_EntryEIP    dd ?
_EntryCS     dw ?
_PSP         dw ?
_EntryESP    dd ?
_EntrySS     dw ?
_EntryDS     dw ?
_ModLink     dd ?,?
LOADLE ends

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Somewhere to load the LE header.
;
_DATA32 segment
LEHeader        LE_Header <>   ;also used to read MZ header and "new offset" (at pos 3Ch)

LETemp  db 256 dup (?)

	align 4

;load1 LOADLE <>

_DATA32 ends

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Load, relocate and execute the application code. LE format loader.
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
;2      - Not an LE file.
;3      - Not enough memory.
;
LoadLE  proc    near uses ds es fs gs

local   load1:LOADLE
local   _ebp:dword

        @dprintf DOPT_EXEC,<"LoadLE enter",10>
        mov     d[load1._Name+0],edx
        mov     w[load1._Name+4],ds
        mov     [load1._Flags],ebx
        mov     d[load1._Command+0],esi
        mov     w[load1._Command+4],es
        mov     [load1._Environment],cx
        ;
        xor     eax,eax
        mov     [load1._Handle],eax
        mov     [load1._PSP],ax
        mov     [load1._ProgMem+0],eax
        mov     d[load1._BaseSeg],eax
        mov     [load1._ObjMem],eax
        mov     [load1._FixupMem],eax
        mov     [load1._ModLink+0],eax
        mov     [load1._ModLink+4],eax

        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
;Try and open the file.
;
        push    ds
        lds     edx,[load1._Name]
        mov     ax,3d00h                ;open, read only.
        int     21h
        pop     ds
        jc      load1_no_file_error
        mov     [load1._Handle],eax
;
;Check it's an MZ file.
;
        mov     ebx,[load1._Handle]
        mov     edx,offset LEHeader     ;somewhere to put the info.
        mov     ecx,2
        mov     ah,3fh
        int     21h
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
        cmp     w[LEHeader],'ZM'
        jnz     load1_file_error

;Look for an LE offset.
;
GetLEOff:
        mov     ebx,[load1._Handle]
        mov     dx,3ch
        xor     cx,cx
        mov     ax,4200h
        int     21h
        mov     edx,offset LEHeader
        mov     ecx,4
        mov     ah,3fh
        int     21h                     ;Fetch LE offset.
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
        cmp     d[LEHeader],0           ;any offset?
        jz      load1_file_error
        mov     eax,d[LEHeader]

SaveLEOff:
        mov     [load1._LEOffset],eax
;
;Load the LE header.
;
        mov     dx,w[LEHeader+0]
        mov     cx,w[LEHeader+2]
        mov     ax,4200h
        int     21h                     ;Move to LE section.
        mov     edx,offset LEHeader
        mov     ecx,size LE_Header
        mov     ah,3fh
        int     21h
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
;
;Check it really is an LE file.
;
        cmp     [LEHeader.LE_ID],"EL"
        jnz     load1_file_error
;
;Close the file again.
;
medclose:
        xor     ebx,ebx
        xchg    ebx,[load1._Handle]
        mov     ah,3eh
        int     21h
;
;Create a new PSP.
;
        push    ds
        mov     ebx,[load1._Flags]
        mov     cx,[load1._Environment]
        les     esi,[load1._Command]
        lds     edx,[load1._Name]
        call    CreatePSP
        pop     ds
        mov     [load1._PSP],bx
        jc      load1_mem_error
;
;Open the input file again.
;
        push    ds
        lds     edx,[load1._Name]
        mov     ax,3d00h                ;open, read only.
        int     21h
        pop     ds
        jc      load1_file_error
        mov     [load1._Handle],eax
;
;Process any EXPORT entries that need pulling in.
;
        cmp     d[LEHeader.LE_ResidentNames],0
        jz      load1_NoExports

IFDEF DEBUGLE
        jmp     load1_NoExports
ENDIF

        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        ;
        ;Set file pointer to resident names.
        ;
        mov     ecx,[LEHeader.LE_ResidentNames]
        add     ecx,[load1._LEOffset]
        mov     dx,cx
        shr     ecx,16
        mov     ebx,[load1._Handle]
        mov     ax,4200h
        int     21h
        ;
        ;Sit in a loop reading names.
        ;
;        xor     ebp,ebp                 ;reset entry count.
        mov     _ebp,0                  ;reset entry count.
        mov     edi,4                   ;reset bytes required.
load1_ge0:
        mov     edx,offset LETemp
        mov     ecx,1
        mov     ah,3fh
        int     21h
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
        xor     eax,eax
        mov     al,[edx]
        and     al,127
        jz      load1_ge1               ;end of the list?
        add     eax,1                   ;include count byte
        add     eax,2                   ;include ordinal/segment
        inc     _ebp                     ;update name count.
        add     edi,eax
        add     edi,4                   ;dword of value
        add     edi,4                   ;table entry memory required.
        mov     ecx,eax
        dec     ecx
        mov     ah,3fh
        int     21h                     ;read rest of the entry to skip it.
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
        jmp     load1_ge0               ;fetch all names.
        ;
        ;Allocate EXPORT table memory.
        ;
load1_ge1:
        mov     ecx,edi
        Sys     GetMemLinear32
        jc      load1_mem_error
        mov     DWORD PTR es:[esi],0    ;reset count.
        push    es
        mov     es,[load1._PSP]
        mov     es:[EPSP_Struc.EPSP_Exports],esi
        pop     es
        mov     edi,_ebp                 ;get number of entries.
        shl     edi,2                   ;dword per entry.
        add     edi,4                   ;allow for count dword.
        add     edi,esi                 ;point to target memory.
        mov     edx,esi
        add     edx,4                   ;point to table memory.
        ;
        ;Move back to start of names again.
        ;
        push    ecx
        push    edx
        mov     ecx,[LEHeader.LE_ResidentNames]
        add     ecx,[load1._LEOffset]
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h
        pop     edx
        pop     ecx
        ;
        ;Read all the names again.
        ;
load1_ge2:
;        or      ebp,ebp                 ;done all names?
        cmp     _ebp,0                  ;done all names?
        jz      load1_ge3
        push    edx
        mov     ecx,1
        mov     edx,offset LETemp
        mov     ah,3fh
        int     21h                     ;get name string length.
        pop     edx
        jc      load1_file_error
        cmp     ax,1
        jnz     load1_file_error
        movzx   ecx,b[LETemp]
        and     cl,127                  ;name length.
        add     ecx,2                   ;include ordinal.
        mov     ah,3fh
        push    edx
        mov     edx,offset LETemp+1
        int     21h                     ;read rest of this entry.
        pop     edx
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
        inc     DWORD PTR es:[esi]      ;update EXPORT count.
        mov     es:[edx],edi            ;set this entries address.
        add     edx,4
        mov     DWORD PTR es:[edi],0    ;clear offset.
        add     edi,4
        movzx   eax,b[LETemp]
        and     eax,127
        inc     eax
        mov     ecx,eax
        add     eax,offset LETemp
        movzx   eax,w[eax]
        mov     es:[edi],ax             ;set entry table ordinal.
        add     edi,2
        push    esi
        mov     esi,offset LETemp
        rep     movsb                   ;copy EXPORT name.
        pop     esi
        dec     _ebp
        jmp     load1_ge2
load1_ge3:
        dec     DWORD PTR es:[esi]      ;lose module name from the count.
;
;Get object definition memory.
;
load1_NoExports:
        mov     eax,size LE_OBJ         ;length of an object entry.
        mul     [LEHeader.LE_ObjNum]    ;number of objects.
        mov     ecx,eax
        Sys     GetMemLinear32          ;Get memory.
        jc      load1_mem_error         ;Not enough memory.
        mov     [load1._ObjMem],esi
;
;Read object definitions.
;
        push    ecx
        mov     ebx,[load1._Handle]
        mov     ecx,[LEHeader.LE_ObjOffset] ;Get object table offset.
        add     ecx,[load1._LEOffset]
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h
        pop     ecx
        mov     edx,[load1._ObjMem]
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     ds,RealSegment
        mov     ah,3fh
        int     21h                     ;read definitions.
        pop     ds
        assume ds:GROUP32
        jc      load1_file_error
        cmp     ax,cx
        jnz     load1_file_error
;
;Work out how much memory we need for the program.
;
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        mov     ecx,[LEHeader.LE_ObjNum] ;number of objects.
        mov     esi,[load1._ObjMem]
;        xor     ebp,ebp                 ;clear memory requirement.
        mov     _ebp,0                   ;clear memory requirement.
load1_objup0:
        mov     eax,es:LE_OBJ.LE_OBJ_Size[esi]
        add     eax,4095
;        and     eax,not 4095            ;page align objects
        and     ax,0f000h               ;page align objects
        mov     es:LE_OBJ.LE_OBJ_Size[esi],eax
        add     _ebp,eax                 ;update program memory length.
        add     esi,size LE_OBJ
        dec     ecx
        jnz     load1_objup0
;
;Get programs memory block.
;
        mov     ecx,_ebp
        Sys     GetMemLinear32          ;Get memory.
        jc      load1_mem_error         ;Not enough memory.
        mov     [load1._ProgMem+0],esi
        mov     [load1._ProgMem+4],ecx
;
;Run through objects setting up load addresses.
;
        mov     edx,[load1._ProgMem+0]    ;reset load offset.
        mov     ecx,[LEHeader.LE_ObjNum]  ;number of objects.
        mov     esi,[load1._ObjMem]
load1_objup1:
        mov     es:LE_OBJ.LE_OBJ_Base[esi],edx  ;set load address.
        add     edx,es:LE_OBJ.LE_OBJ_Size[esi]  ;update with object length.
        add     esi,size LE_OBJ
        dec     ecx
        jnz     load1_objup1
;
;Get selectors.
;
        mov     ecx,[LEHeader.LE_ObjNum]
        Sys     GetSels
        jc      load1_mem_error
        mov     [load1._BaseSeg],bx        ;store base selector.
        mov     [load1._NumSegs],cx        ;store number of selectors.
;
;Update programs memory and selector details in PSP and variables.
;
        push    es
        mov     es,[load1._PSP]
        mov     ax,[load1._BaseSeg]                      ;get base selector.
        mov     es:[EPSP_Struc.EPSP_SegBase],ax
        mov     ax,[load1._NumSegs]                      ;get number of selectors.
        shl     ax,3
        mov     es:[EPSP_Struc.EPSP_SegSize],ax
        mov     eax,[load1._ProgMem+0]                   ;get memory address.
        mov     es:[EPSP_Struc.EPSP_MemBase],eax
        mov     es:[EPSP_Struc.EPSP_NearBase],eax
        mov     eax,[load1._ProgMem+4]                   ;get memory size.
        mov     es:[EPSP_Struc.EPSP_MemSize],eax
        pop     es
;
;Fetch entry table and update EXPORT table values.
;
        push    es
        mov     es,[load1._PSP]
        cmp     es:[EPSP_Struc.EPSP_Exports],0 ;any exports?
        pop     es
        jz      load1_NoEntries
        push    es
        mov     es,[load1._PSP]
        mov     eax,es:[EPSP_Struc.EPSP_Exports]
        pop     es
        cmp     DWORD PTR es:[eax],0                    ;just a module name?
        jz      load1_NoEntries
        ;
        ;Move file pointer to start of entry table.
        ;
        mov     ecx,[LEHeader.LE_EntryTable]
        add     ecx,[load1._LEOffset]
        mov     ebx,[load1._Handle]
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h
        ;
        ;Work out how much work space we need.
        ;
        mov     ecx,[LEHeader.LE_EntryTable]
        cmp     [LEHeader.LE_Directives],0
        jz      load1_ge4
        sub     ecx,[LEHeader.LE_Directives]
        jmp     load1_ge5
load1_ge4:
        sub     ecx,[LEHeader.LE_Fixups]
load1_ge5:
        neg     ecx
        Sys     GetMemLinear32          ;get entry table memory.
        jc      load1_mem_error
        mov     edx,esi
        push    ds
        push    es
        pop     ds
        call    ReadFile                ;read the entry table.
        pop     ds
        jc      load1_file_error
        cmp     eax,ecx
        jnz     load1_file_error
        ;
        ;Work through all EXPORT's setting values.
        ;
        push    es
        mov     es,[load1._PSP]
        mov     esi,es:[EPSP_Struc.EPSP_Exports]
        pop     es
        mov     ecx,es:[esi]            ;get number of entries.
        add     DWORD PTR es:[esi+4],4+2 ;correct module name pointer.
        add     esi,4+4
load1_exp0:
        push    ecx
        push    esi
        push    edx
        mov     esi,es:[esi]            ;point to this entry.

;        xor     ebp,ebp
;        xchg    bp,WORD PTR es:[esi+4]  ;get & clear ordinal.
        xor     eax,eax
        xchg    ax,es:[esi+4]
        mov     _ebp,eax

        dec     _ebp
load1_exp1:
        mov     bh,es:[edx]             ;get bundle count.
        or      bh,bh
        jz      load1_bad_entry
        mov     bl,es:[edx+1]           ;get bundle type.
        add     edx,2
        mov     edi,edx                 ;point to object number incase we need it.
        xor     eax,eax
        mov     al,0
        cmp     bl,0
        jz      load1_exp2
        add     edx,2                   ;skip object number.
        mov     al,3
        cmp     bl,1
        jz      load1_exp2
        mov     al,5
        cmp     bl,2
        jz      load1_exp2
        mov     al,5
        cmp     bl,3
        jz      load1_exp2
        mov     al,7
        cmp     bl,4
        jz      load1_exp2
        jmp     load1_bad_entry
load1_exp2:
load1_exp3:
        or      bh,bh
        jz      load1_exp1              ;end of this bundle.
;        or      ebp,ebp                 ;our ordinal?
        cmp     _ebp,0                  ;our ordinal?
        jz      load1_exp4
        add     edx,eax                 ;next entry.
        dec     _ebp
        dec     bh
        jmp     load1_exp3
        ;
load1_exp4:
        or      bl,bl
        jz      load1_bad_entry
        dec     bl
        jz      load1_exp_16bit
        dec     bl

IFDEF DEBUGLE
        jz      load1_bad_fixup1
ENDIF

        jz      load1_bad_fixup
        dec     bl
        jz      load1_exp_32bit
        dec     bl
        jz      load1_bad_entry
        jmp     load1_bad_entry
        ;
load1_bad_entry:
        pop     edx
        pop     esi
        pop     ecx

IFDEF DEBUGLE
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    ds
        push    cs
        pop     ds
        cmp     bl,4
        jae     debugf2ae10
        mov     edx,OFFSET debugf2textl10
        jmp     debugf2loop2
debugf2ae10:
        mov     edx,OFFSET debugf2textae10
debugf2loop2:
        cmp     BYTE PTR ds:[edx],0
        je      debugf2b
        mov     ecx,1
        mov     bx,1
        mov     ah,40h
        int     21h
        inc     edx
        jmp     debugf2loop2
debugf2b:
        mov     edx,OFFSET debugf2textcrlf
        push    cs
        pop     ds
        mov     ecx,2
        mov     bx,1
        mov     ah,40h
        int     21h
        pop     ds
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        jmp     debugf2out

debugf2textl10  DB      '<10',0
debugf2textae10 DB      '>=10',0
debugf2textcrlf DB      13,10

debugf2out:
        jmp     load1_bad_fixup2
ENDIF

        jmp     load1_bad_fixup
        ;
load1_exp_16bit:
        movzx   eax,WORD PTR es:[edi]   ;get the object number.
        dec     eax
        shl     ax,3
        add     ax,[load1._BaseSeg]
        mov     es:[esi+4],ax
        movzx   eax,WORD PTR es:[edx+1] ;get the offset.
        mov     es:[esi],eax
        jmp     load1_exp8
        ;
load1_exp_32bit:
        movzx   eax,WORD PTR es:[edi]
        dec     eax
        push    eax
        shl     eax,2
        mov     ebx,eax
        add     ebx,ebx
        shl     eax,2
        add     ebx,eax
        add     ebx,[load1._ObjMem]
        mov     ebx,es:LE_OBJ.LE_OBJ_Base[ebx]
        pop     eax
        shl     ax,3
        add     ax,[load1._BaseSeg]
        mov     es:[esi+4],ax
        mov     eax,es:[edx+1]
        add     eax,ebx
        mov     es:[esi],eax
        ;
load1_exp8:
        pop     edx
        pop     esi
        pop     ecx
        add     esi,4
        dec     ecx
        jnz     load1_exp0
        ;
        mov     esi,edx
        Sys     RelMemLinear32
;
;Read program objects.
;
load1_NoEntries:
;        mov     ebp,[LEHeader.LE_ObjNum]        ;number of objects.
        mov     eax,[LEHeader.LE_ObjNum]        ;number of objects.
        mov     _ebp,eax
        mov     esi,[load1._ObjMem]

load1_load0:
        mov     eax,es:LE_OBJ.LE_OBJ_Flags[esi] ;get objects flags.
        and     eax,LE_OBJ_Flags_FillMsk        ;isolate fill type.
        cmp     eax,LE_OBJ_Flags_Zero           ;zero filled?
        jnz     load1_load1
        ;
        ;Zero this objects memory.
        ;
        mov     ecx,es:LE_OBJ.LE_OBJ_Size[esi]  ;get objects virtual length.
        mov     edi,es:LE_OBJ.LE_OBJ_Base[esi]
        xor     eax,eax
        push    ecx
        and     ecx,3
        rep     stosb
        pop     ecx
        shr     ecx,2
        rep     stosd
load1_load1:
        ;
        ;Set file offset for data.
        ;
        mov     eax,es:LE_OBJ.LE_OBJ_PageIndex[esi] ;get first page index.
        dec     eax
        mul     [LEHeader.LE_PageSize]              ;* page size.
        add     eax,d[LEHeader.LE_Data]             ;data offset.
        mov     dx,ax
        shr     eax,16
        mov     cx,ax
        mov     ax,4200h
        mov     ebx,[load1._Handle]
        int     21h                                 ;set the file pointer.
        ;
        ;Work out how much data we're going to load.
        ;
        mov     eax,es:LE_OBJ.LE_OBJ_PageNum[esi]   ;get number of pages.
        mov     ebx,eax
        mul     [LEHeader.LE_PageSize]              ;* page size.
        mov     edx,es:LE_OBJ.LE_OBJ_Base[esi]      ;get load address.
        xor     ecx,ecx
        or      eax,eax
        jz      load1_loadz
        mov     ecx,es:LE_OBJ.LE_OBJ_Size[esi]

        add     ebx,es:LE_OBJ.LE_OBJ_PageIndex[esi] ;get base page again.
        dec     ebx
        cmp     ebx,[LEHeader.LE_Pages]             ;we getting the last page?
        jnz     load1_load2
        mov     ebx,[LEHeader.LE_PageSize]
        sub     ebx,[LEHeader.LE_LastBytes]
        sub     eax,ebx

load1_load2:
        cmp     ecx,eax
        jc      load1_load3
        mov     ecx,eax
load1_load3:
        ;
        ;Load the data.
        ;
        mov     ebx,[load1._Handle]
        push    ds
        push    es
        pop     ds
        call    ReadFile
        pop     ds
        jc      load1_file_error
        cmp     eax,ecx
        jnz     load1_file_error
        ;
ZeroNotLoaded:
        ;Zero any memory we didn't just load to for Watcom's BSS benefit.
        ;
        cmp     ecx,es:LE_OBJ.LE_OBJ_Size[esi]
        jnc     load1_load4
        push    edi
        mov     edi,edx
        add     edi,ecx
        sub     ecx,es:LE_OBJ.LE_OBJ_Size[esi]
        neg     ecx
        xor     eax,eax
        push    ecx
        shr     ecx,2
        or      ecx,ecx
        jz      load1_load6
load1_load5:
        mov     es:[edi],eax
        add     edi,4
        dec     ecx
        jnz     load1_load5
load1_load6:
        pop     ecx
        and     ecx,3
        rep     stosb
        pop     edi
load1_load4:
        ;
        ;Next object.
        ;
load1_loadz:
        add     esi,size LE_OBJ
        dec     _ebp
        jnz     load1_load0
;
;Get fixup table memory & load fixups.
;
        mov     ecx,[LEHeader.LE_FixupSize]
        Sys     GetMemLinear32          ;Get memory.
        jc      load1_mem_error         ;Not enough memory.
        mov     [load1._FixupMem],esi
        push    ecx
        mov     ecx,[LEHeader.LE_Fixups]
        add     ecx,[load1._LEOffset]
        mov     dx,cx
        shr     ecx,16
        mov     ebx,[load1._Handle]
        mov     ax,4200h
        int     21h                     ;move to fixup data.
        pop     ecx
        mov     edx,esi
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     ds,RealSegment
        call    ReadFile
        pop     ds
        assume ds:GROUP32
        jc      load1_file_error
        cmp     eax,ecx
        jnz     load1_file_error
;
;Get IMPORT module name links.
;
        mov     ecx,[LEHeader.LE_ImportModNum]
        or      ecx,ecx
        jz      load1_GotImpMods
        shl     ecx,2
        add     ecx,4
        Sys     GetMemLinear32
        jc      load1_mem_error
        mov     DWORD PTR es:[esi],0    ;clear entry count for now.
        push    es
        mov     es,[load1._PSP]
        mov     es:[EPSP_Struc.EPSP_Imports],esi
        pop     es
        mov     [load1._ModLink+0],esi
        ;
        ;Work out offset in fixup data.
        ;
        mov     esi,[LEHeader.LE_ImportModNames]
        sub     esi,[LEHeader.LE_Fixups]
        add     esi,[load1._FixupMem]
        ;
        ;Work through each name getting the module link address.
        ;
load1_NextModLnk:
        cmp     [LEHeader.LE_ImportModNum],0
        jz      load1_GotImpMods
        ;
        ;Preserve current header state.
        ;

        push    esi
        mov     ecx,size LE_Header
        Sys     GetMemLinear32
        mov     _ebp,esi
        pop     esi
        jc      load1_mem_error
        mov     edi,_ebp
        push    esi
        mov     esi,offset LEHeader
        rep     movsb
        pop     esi

        ;
        ;Search for this name.
        ;
        @dprintf DOPT_EXEC,<"LoadLE: calling FindModule",10>
        call    FindModule
        jc      load1_error

        ;
        push    esi
        push    edi
        mov     esi,_ebp
        mov     edi,offset LEHeader
        mov     ecx,size LE_Header
        push    ds
        push    es
        pop     ds
        pop     es
        rep     movsb
        push    ds
        push    es
        pop     ds
        pop     es
        mov     esi,_ebp
        Sys     RelMemLinear32
        pop     edi
        pop     esi

        mov     edx,[load1._ModLink+0]
        mov     eax,es:[edx]            ;get current count.
        shl     eax,2
        add     eax,4
        mov     es:[edx+eax],edi        ;store link address.
        inc     DWORD PTR es:[edx]      ;update link count.
        ;
        movzx   ecx,BYTE PTR es:[esi]
        inc     ecx
        add     esi,ecx
        dec     [LEHeader.LE_ImportModNum]
        jmp     load1_NextModLnk

;
;Apply the fixups.
;
load1_GotImpMods:
        mov     eax,[LEHeader.LE_ObjNum]
        mov     [load1._ObjCount],eax
        mov     eax,[load1._ObjMem]
        mov     [load1._ObjBase],eax
        mov     [load1._EntryEIP],0
load1_fix0:
        mov     esi,[load1._ObjBase]
        mov     ecx,es:LE_OBJ.LE_OBJ_PageNum[esi]
        or      ecx,ecx
        jz      load1_fix400
        mov     [load1._PageCount+0],ecx
        mov     [load1._PageCount+4],0
        mov     edx,es:LE_OBJ.LE_OBJ_PageIndex[esi]
        dec     edx
        mov     _ebp,edx                 ;Set base page map entry.
load1_fix1:
        mov     edx,_ebp
        mov     esi,[load1._FixupMem]
        mov     ecx,es:[esi+4+edx*4]    ;Get next offset.
        mov     edx,es:[esi+edx*4]      ;Get start offset.
        sub     ecx,edx                 ;Get number of bytes
        jz      load1_fix4

        mov     esi,[load1._FixupMem]
        add     esi,[LEHeader.LE_FixupsRec] ;Point to fixup data.
        sub     esi,[LEHeader.LE_Fixups]
        add     esi,edx                 ;Move to start of this pages fixups.
load1_fix2:

        mov     al,es:[esi]             ;Get type byte.
        mov     bl,al
        shr     bl,4                    ;Get single/multiple flag.
        mov     bh,al
        and     bh,15                   ;Get type.
        inc     esi
        dec     ecx
        mov     al,es:[esi]             ;Get second type byte.
        mov     dl,al
        and     dl,3                    ;Get internal/external specifier.
        mov     dh,al
        shr     dh,2                    ;Get destination type.
        inc     esi
        dec     ecx
        ;
        push    ebx
        and     bl,not 1
        or      bl,bl                   ;Check it's a single entry.
        pop     ebx
        jz      CheckUnknown

IFDEF DEBUGLE
        jnz     load1_bad_fixup3
ENDIF

        jnz     load1_bad_fixup
        ;

IFDEF DEBUGLE
        test    dh,011010b              ;Check for un-known bits.
        jnz     load1_bad_fixup9
ENDIF


; added support for additive bit, MED 06/10/96
;       test    dh,011011b              ;Check for un-known bits.
CheckUnknown:
        test    dh,011010b              ;Check for un-known bits.
        jnz     load1_bad_fixup

        or      dl,dl                   ;Check it's an internal target.
        jnz     load1_fixup_import

        cmp     bh,0010b                ;Word segment?
        jz      load1_Seg16
        cmp     bh,0111b                ;32-bit offset?
        jz      load1_32BitOff
        cmp     bh,0110b                ;Seg:32-bit offset?
        jz      load1_Seg1632BitOff
        cmp     bh,1000b                ;32-bit self relative?
        jz      load1_Self32Off
        cmp     bh,0101b                ;16-bit offset?
        jz      load1_16BitOff

; MED 12/09/96
        cmp     bh,1                    ; ignore fixup ???
        je      load1_fix3

IFDEF DEBUGLE
        jmp     load1_bad_fixup4
        cmp     bh,1
        jne     around
;       add     esi,4
;       sub     ecx,4
        jmp     load1_fix3
around:
ENDIF

        jmp     load1_bad_fixup
;
;Fetch an external referance.
;
load1_fixup_import:
        ;
        ;Grab the page offset.
        ;
        movsx   edi,WORD PTR es:[esi]
        add     esi,2
        sub     ecx,2
        ;
        ;Check import type.
        ;
        cmp     dl,01b                  ;ordinal?
        jz      load1_fiximp0
        cmp     dl,10b                  ;name?

IFDEF DEBUGLE
        jnz     load1_bad_fixup5
ENDIF

        jnz     load1_bad_fixup
        ;
        ;Importing by name so find the name.
        ;
        push    edi
;--- FundFunction expects register EBP to hold the name offset!
;--- this register isn't free to use here, so use EDI instead
        mov     edi,[LEHeader.LE_ImportNames]
        sub     edi,[LEHeader.LE_Fixups]
        movzx   eax,WORD PTR es:[esi+1]
        add     edi,eax                  ;point to function name.
        add     edi,[load1._FixupMem]    ;point to function name.

        movzx   eax,BYTE PTR es:[esi]
        shl     eax,2
        add     eax,[load1._ModLink+0]
        push    ebp
;--- now EBP can be used
        mov     ebp,edi
        mov     edi,es:[eax]            ;point to module.
        mov     edi,es:EPSP_Struc.EPSP_Exports[edi] ;point to export table.
        call    FindFunction
        mov     eax,edi
        pop     ebp
        pop     edi
        jc      load1_file_error
        add     esi,1+2
        sub     ecx,1+2
        jmp     load1_fiximp2
        ;
        ;Importing by ordinal so go straight to the export.
        ;
load1_fiximp0:
        push    edi
        movzx   edi,BYTE PTR es:[esi]
        shl     edi,2
        add     edi,[load1._ModLink+0]
        mov     edi,es:[edi]
        mov     edi,es:EPSP_Struc.EPSP_Exports[edi] ;point to export table.
        movzx   eax,WORD PTR es:[esi+1]
        add     esi,2
        sub     ecx,2
        test    dh,100000b
        jz      load1_fiximp1
        sub     esi,2
        add     ecx,2
        movzx   eax,BYTE PTR es:[esi+1]
        add     esi,1
        sub     ecx,1
load1_fiximp1:
        mov     eax,es:[edi+4+eax*4]    ;point to export.
        pop     edi
        add     esi,1
        sub     ecx,1
load1_fiximp2:

        ;Now perform the fixup.
        ;
        cmp     bh,0010b                ;Word segment?
        jz      load1_iSeg16
        cmp     bh,0111b                ;32-bit offset?
        jz      load1_i32BitOff
        cmp     bh,0110b                ;Seg:32-bit offset?
        jz      load1_iSeg1632BitOff
        cmp     bh,1000b                ;32-bit self relative?
        jz      load1_iSelf32Off


IFDEF DEBUGLE
        jmp     load1_bad_fixup6
ENDIF

        jmp     load1_bad_fixup
        ;
load1_iSeg16:
        ;Deal with a 16-bit segment.
        ;
        test    dh,4

IFDEF DEBUGLE
        jnz     load1_bad_fixup7
ENDIF

        jnz     load1_bad_fixup
        ;
        or      edi,edi
        js      load1_iNeg0
        mov     ebx,[load1._ObjBase]
        mov     ebx,es:LE_OBJ.LE_OBJ_Base[ebx]
        add     edi,ebx
        mov     ebx,[load1._PageCount+4] ;Get page number.
        shl     ebx,12
        add     edi,ebx                 ;Point to the right page.
        mov     ax,es:[eax+4]           ;Get the target segment.
        mov     es:[edi],ax             ;Store target.

; MED 06/10/96
        test    dh,1                    ; see if additive value
        jne     load1_bad_fixup         ; yes, don't allow additives on segment fixups

load1_iNeg0:
        jmp     load1_fix3
        ;
load1_i32BitOff:
        ;Deal with a 32-bit offset.
        ;
        or      edi,edi
        js      load1_iNeg1
        mov     ebx,[load1._ObjBase]
        mov     ebx,es:LE_OBJ.LE_OBJ_Base[ebx]
        add     edi,ebx
        mov     ebx,[load1._PageCount+4] ;Get page number.
        shl     ebx,12
        add     edi,ebx                 ;Point to the right page.
        mov     eax,es:[eax]
        mov     es:[edi],eax

; MED 06/10/96
        test    dh,1                    ; see if additive value
        je      load1_fix3              ; no
        movzx   eax,WORD PTR es:[esi]   ; get additive value
        add     esi,2
        sub     ecx,2
        add     es:[edi],eax            ;Store target.
        jmp     load1_fix3

load1_iNeg1:

        test    dh,1    ; MED 06/12/96
        jz      load1_iNeg1a
        add     esi,2
        sub     ecx,2
load1_iNeg1a:

        jmp     load1_fix3


load1_iSelf32Off:
        ;Deal with a 32-bit self relative offset.

        or      edi,edi
        js      load1_isfNeg1
        mov     ebx,[load1._ObjBase]
        mov     ebx,es:LE_OBJ.LE_OBJ_Base[ebx]
        add     edi,ebx
        mov     ebx,[load1._PageCount+4] ;Get page number.
        shl     ebx,12
        add     edi,ebx                 ;Point to the right page.
        mov     ebx,edi
        add     ebx,4
        mov     eax,es:[eax]
        sub     eax,ebx
        mov     es:[edi],eax

; MED 06/10/96
        test    dh,1                    ; see if additive value
        je      load1_fix3              ; no
        movzx   eax,WORD PTR es:[esi]   ; get additive value
        add     esi,2
        sub     ecx,2
        add     es:[edi],eax            ;Store target.

load1_isfNeg1:
        jmp     load1_fix3

load1_iSeg1632BitOff:
        ;Deal with an FWORD fixup by splitting into a seg16 and 32-bit
        ;offset relocation entry.
        ;
        or      edi,edi
        js      load1_iNeg2
        mov     ebx,[load1._ObjBase]
        mov     ebx,es:LE_OBJ.LE_OBJ_Base[ebx]
        add     edi,ebx
        mov     ebx,[load1._PageCount+4] ;Get page number.
        shl     ebx,12
        add     edi,ebx                 ;Point to the right page.
        push    eax
        movzx   eax,WORD PTR es:[eax+4]
        mov     es:[edi+4],ax           ;Store target.
        pop     eax
        mov     eax,es:[eax]
        mov     es:[edi],eax

; MED 06/10/96
        test    dh,1                    ; see if additive value
        je      load1_fix3              ; no
        movzx   eax,WORD PTR es:[esi]   ; get additive value
        add     esi,2
        sub     ecx,2
        add     es:[edi],eax            ;Store target.

load1_iNeg2:
        jmp     load1_fix3

;Deal with a 16-bit segment.
load1_Seg16:

        ;EBP    - Page offset within segment.
        ;w[esi] - offset within page.
        ;b[esi+2] - target object+1.
        ;
        test    dh,4

IFDEF DEBUGLE
        jnz     load1_bad_fixup8
ENDIF

        jnz     load1_bad_fixup
        ;
        mov     edi,[load1._ObjBase]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movsx   eax,WORD PTR es:[esi]
        or      eax,eax
        js      load1_Neg0
        add     edi,eax                 ;Point to the right offset.
        movzx   eax,BYTE PTR es:[esi+2] ;Get the target segment.
        dec     eax
        shl     eax,3
        add     ax,[load1._BaseSeg]
        mov     es:[edi],ax             ;Store target.
        ;
load1_Neg0:
        add     esi,2+1
        sub     ecx,2+1
        jmp     load1_fix3
        ;
load1_16BitOff:
        ;Deal with a 16-bit offset.
        ;
        ;EBP    - Page offset within segment.
        ;w[esi] - offset within page.
        ;b[esi+2] - target object+1
        ;w[esi+3] - target offset.
        ;
        mov     edi,[load1._ObjBase]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movsx   eax,WORD PTR es:[esi]
        or      eax,eax
        js      load1_Neg3
        add     edi,eax                 ;Point to the right offset.
        mov     ax,WORD PTR es:[esi+3]  ;Get target offset.
        mov     es:[edi],ax
load1_Neg3:
        add     esi,2+1+2
        sub     ecx,2+1+2

; MED 02/06/07, check for 32-bit offset target, seems like would be only be
;  the result of incorrect coding, but apparently it can happen
        test    dh,4
        jz      load1_fix3
        add     esi,2
        sub     ecx,2

        jmp     load1_fix3
        ;
load1_32BitOff:
        ;Deal with a 32-bit offset.
        ;
        ;EBP    - Page offset within segment.
        ;w[esi] - offset within page.
        ;b[esi+2] - target object+1
        ;w[esi+3] - target offset.
        ;
        mov     edi,[load1._ObjBase]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movsx   eax,WORD PTR es:[esi]
        or      eax,eax
        js      load1_Neg1
        add     edi,eax                 ;Point to the right offset.
        movzx   eax,BYTE PTR es:[esi+2] ;Get the target segment.
        dec     eax
        push    edx
        mov     edx,size LE_OBJ
        mul     edx
        pop     edx
        add     eax,[load1._ObjMem]      ;point to target segment details.
        mov     eax,es:LE_OBJ.LE_OBJ_Base[eax] ;Get target segments offset from start of image.

COMMENT !
        movzx   ebx,WORD PTR es:[esi+3] ;Get target offset.
        test    dh,4
        jz      load1_Big0
        mov     ebx,es:[esi+3]          ;Get target offset.
load1_Big0:
        add     eax,ebx
        mov     es:[edi],eax
END COMMENT !
; MED 06/12/96, allow for additive bit
        test    dh,4
        jnz     load1_Big0
        movzx   ebx,WORD PTR es:[esi+3] ;Get 16-bit target offset.
        add     esi,2+1+2               ; adjust offset, byte count
        sub     ecx,2+1+2

stuff1:
        add     eax,ebx
        mov     es:[edi],eax
        test    dh,1                    ; check for additive value
        je      load1_fix3              ; none
        movzx   eax,WORD PTR es:[esi]   ; get additive value
        add     esi,2
        sub     ecx,2
        add     es:[edi],eax            ;Store target.
        jmp     load1_fix3

load1_Big0:
        mov     ebx,es:[esi+3]          ;Get 32-bit target offset.
        add     esi,2+1+4               ; adjust offset, byte count
        sub     ecx,2+1+4
        jmp     stuff1

load1_Neg1:
        add     esi,2+1+2
        sub     ecx,2+1+2

        test    dh,1                    ; MED 06/12/96
        jz      load1_Neg1a
        add     esi,2
        sub     ecx,2
load1_Neg1a:

        test    dh,4
        jz      load1_fix3
        add     esi,2
        sub     ecx,2
        jmp     load1_fix3

load1_Self32Off:
        ;Deal with a 32-bit self relative offset.
        ;
        ;EBP    - Page offset within segment.
        ;w[esi] - offset within page.
        ;b[esi+2] - target object+1
        ;w[esi+3] - target offset.
        ;
        mov     edi,[load1._ObjBase]
        mov     ebx,es:LE_OBJ.LE_OBJ_Flags[edi]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movsx   eax,WORD PTR es:[esi]
        or      eax,eax
        js      load1_sfNeg1
        add     edi,eax                 ;Point to the right offset.
        mov     ebx,edi
        movzx   eax,BYTE PTR es:[esi+2] ;Get the target segment.
        dec     eax
        push    edx
        mov     edx,size LE_OBJ
        mul     edx
        pop     edx
        add     eax,[load1._ObjMem]      ;point to target segment details.
        mov     eax,es:LE_OBJ.LE_OBJ_Base[eax]
        push    ebx
        movzx   ebx,WORD PTR es:[esi+3] ;Get target offset.
        test    dh,4
        jz      load1_sfBig0
        mov     ebx,es:[esi+3]          ;Get target offset.
load1_sfBig0:
        add     eax,ebx
        pop     ebx
        add     ebx,4
        sub     eax,ebx
        mov     es:[edi],eax
        ;
load1_sfNeg1:
        add     esi,2+1+2
        sub     ecx,2+1+2
        test    dh,4
        jz      load1_fix3
        add     esi,2
        sub     ecx,2
        jmp     load1_fix3
        ;
load1_Seg1632BitOff:
        ;Deal with an FWORD fixup by splitting into a seg16 and 32-bit
        ;offset relocation entry.
        ;
        ;EBP    - Page offset within segment.
        ;w[esi] - offset within page.
        ;b[esi+2] - target object+1
        ;w[esi+3] - target offset.
        ;
        mov     edi,[load1._ObjBase]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movsx   eax,WORD PTR es:[esi]
        or      eax,eax
        js      load1_Neg2
        add     edi,eax                 ;Point to the right offset.
        add     edi,4                   ;Point to the seg bit.
        movzx   eax,BYTE PTR es:[esi+2] ;Get the target segment.
        dec     eax
        shl     eax,3
        add     ax,[load1._BaseSeg]
        mov     es:[edi],ax             ;Store target.
        ;
        mov     edi,[load1._ObjBase]
        mov     edi,es:LE_OBJ.LE_OBJ_Base[edi]
        mov     eax,[load1._PageCount+4] ;Get page number.
        shl     eax,12
        add     edi,eax                 ;Point to the right page.
        movzx   eax,WORD PTR es:[esi]
        add     edi,eax                 ;Point to the right offset.
        movzx   eax,BYTE PTR es:[esi+2] ;Get the target segment.
        dec     eax
        push    edx
        mov     edx,size LE_OBJ
        mul     edx
        pop     edx
        add     eax,[load1._ObjMem]      ;point to target segment details.
        test    es:LE_OBJ.LE_OBJ_Flags[eax],LE_OBJ_Flags_Big
        pushfd
        mov     eax,es:LE_OBJ.LE_OBJ_Base[eax] ;Get target segments offset from start of image.
        movzx   ebx,WORD PTR es:[esi+3] ;Get target offset.
        test    dh,4
        jz      load1_Big1
        mov     ebx,es:[esi+3]          ;Get target offset.
load1_Big1:
        popfd
        jz      load1_NotFlat1
        add     ebx,eax
load1_NotFlat1:
        mov     es:[edi],ebx
        ;
load1_Neg2:
        add     esi,2+1+2
        sub     ecx,2+1+2
        test    dh,4
        jz      load1_fix3
        add     esi,2
        sub     ecx,2
        ;
load1_fix3:
        inc     [load1._EntryEIP]
        or      ecx,ecx
        jnz     load1_fix2
        ;
load1_fix4:
        inc     _ebp
        inc     [load1._PageCount+4]
        dec     [load1._PageCount+0]
        jnz     load1_fix1
        ;
load1_fix400:
        add     [load1._ObjBase],size LE_OBJ
        dec     [load1._ObjCount]
        jnz     load1_fix0

        mov     esi,[load1._FixupMem]
        Sys     RelMemLinear32
        mov     [load1._FixupMem],0

;
;Setup entry CS:EIP.
;
        mov     ebx,[LEHeader.LE_EntryCS]
        or      ebx,ebx
        jz      load1_NoEntryCS
        dec     ebx
        mov     eax,size LE_OBJ
        mul     ebx
        shl     ebx,3
        mov     esi,[LEHeader.LE_EntryEIP]
        mov     edi,[load1._ObjMem]
        add     edi,eax
        add     esi,es:LE_OBJ.LE_OBJ_Base[edi]
        test    es:LE_OBJ.LE_OBJ_Flags[edi],LE_OBJ_Flags_Big ;FLAT segment?
        jnz     load1_FlatEIP
        sub     esi,[load1._ProgMem+0]
load1_FlatEIP:
        add     bx,[load1._BaseSeg]
        mov     [load1._EntryEIP],esi
load1_NoEntryCS:
        mov     [load1._EntryCS],bx
;
;Setup entry SS:ESP
;
        mov     ebx,[LEHeader.LE_EntrySS]
        or      ebx,ebx
        jz      load1_NoEntrySS
        dec     ebx
        mov     eax,size LE_OBJ
        mul     ebx
        shl     ebx,3
        mov     esi,[LEHeader.LE_EntryESP]
        mov     edi,[load1._ObjMem]
        add     edi,eax
        add     esi,es:LE_OBJ.LE_OBJ_Base[edi]
        test    es:LE_OBJ.LE_OBJ_Flags[edi],LE_OBJ_Flags_Big ;FLAT segment?
        jnz     load1_FlatESP
        sub     esi,[load1._ProgMem+0]
load1_FlatESP:
        add     bx,[load1._BaseSeg]
        mov     [load1._EntryESP],esi
load1_NoEntrySS:
        mov     [load1._EntrySS],bx
;
;Setup entry ES & DS.
;
        mov     ax,[load1._PSP]
        mov     [load1._EntryES],ax
        mov     [load1._EntryDS],ax
        mov     eax,[LEHeader.LE_AutoDS]
        or      eax,eax
        jz      load1_NoAutoDS
        dec     eax
        shl     eax,3
        add     ax,[load1._BaseSeg]
        mov     [load1._EntryDS],ax
;
;Convert object definitions into 3P segment definitions for CWD.
;
load1_NoAutoDS:
;        mov     _ebp,[LEHeader.LE_ObjNum]       ;number of objects.
        mov     eax,[LEHeader.LE_ObjNum]        ;number of objects.
        mov     _ebp,eax

        mov     esi,[load1._ObjMem]
        mov     edi,esi
load1_makesegs0:
        mov     eax,es:LE_OBJ.LE_OBJ_Flags[esi] ;Get objects flags.
        xor     ebx,ebx
        test    eax,LE_OBJ_Flags_Exec           ;Executable?
        jnz     load1_makesegs1
        inc     ebx                             ;Make it Data.
        test    eax,LE_OBJ_Flags_Write          ;Writeable?
        jz      load1_makesegs1
;       add     ebx,2                           ;Read only data.
load1_makesegs1:
        shl     ebx,21
        test    eax,LE_OBJ_Flags_Big            ;Big bit set?
        jz      load1_makesegs2
        or      ebx,1 shl 26                    ;Force 32-bit.
        or      ebx,1 shl 27                    ;assume 32-bit is FLAT.
        jmp     load1_makesegs3
load1_makesegs2:
        or      ebx,1 shl 25                    ;Force 16-bit.
load1_makesegs3:
        mov     eax,es:LE_OBJ.LE_OBJ_Size[esi]
        cmp     eax,100000h                     ;>1M?
        jc      load1_makesegs4
        shr     eax,12
        or      eax,1 shl 20
load1_makesegs4:
        or      ebx,eax                         ;Include length.
        mov     eax,es:LE_OBJ.LE_OBJ_Base[esi]
        sub     eax,[load1._ProgMem+0]           ;lose load address.
        mov     DWORD PTR es:[edi+0],eax
        mov     DWORD PTR es:[edi+4],ebx
        add     esi,size LE_OBJ
        add     edi,4+4
        dec     _ebp
        jnz     load1_makesegs0
        ;
        ;Shrink OBJ memory to fit segment definitions.
        ;
        mov     eax,4+4
        mul     [LEHeader.LE_ObjNum]    ;number of objects.
        mov     ecx,eax
        mov     esi,[load1._ObjMem]
        Sys     ResMemLinear32
        jc      load1_mem_error         ;shouldn't be able to happen.
        mov     [load1._ObjMem],esi     ;set new Obj mem address.
;
;Setup selectors.
;
        mov     ecx,[LEHeader.LE_ObjNum]
        mov     esi,[load1._ObjMem]
        mov     bx,[load1._BaseSeg]     ;base selector.
load1_SegLoop:
        push    ebx
        push    ecx
        push    esi
        ;
        mov     eax,es:[esi+4]          ;Get limit.
        mov     ecx,eax
        and     ecx,0fffffh             ;mask to 20 bits.
        test    eax,1 shl 20            ;G bit set?
        jz      load1_NoGBit
        shl     ecx,12
        or      cx,0fffh
load1_NoGBit:
        or      ecx,ecx
        jz      load1_NoDecLim
        cmp     ecx,-1
        jz      load1_NoDecLim
        dec     ecx
load1_NoDecLim:
        mov     edx,es:[esi]            ;get base.
        ;
        test    eax,1 shl 27            ;FLAT segment?
        jz      load1_NotFLATSeg
        ;
        push    fs
        mov     fs,[load1._PSP]
        mov     fs:[EPSP_Struc.EPSP_NearBase],0 ;Make sure NEAR functions work.
        pop     fs
        ;
        add     edx,[load1._ProgMem+0]
        or      ecx,-1                  ;Update the limit.
        xor     edx,edx
        jmp     load1_DoSegSet
        ;
load1_NotFLATSeg:
        add     edx,[load1._ProgMem+0]   ;offset within real memory.
        ;
load1_DoSegSet:
        Sys     SetSelDet32
        ;
        mov     eax,es:[esi+4]          ;Get class.
        shr     eax,21                  ;move type into useful place.
        and     eax,0fh                 ;isolate type.
        or      eax,eax
        jz      load1_CodeSeg
        mov     eax,es:[esi+4]          ;Get type bits.
        mov     cx,0                    ;Set 16 bit seg.
        test    eax,1 shl 25
        jnz     load1_GotBBit
        mov     cx,1
        test    eax,1 shl 26            ;32 bit seg?
        jnz     load1_GotBBit
        mov     cx,0                    ;Set 16 bit seg.
load1_GotBBit:
        call    _DSizeSelector
        jmp     load1_SegDone
        ;
load1_CodeSeg:
        mov     eax,es:[esi+4]          ;Get type bits.
        mov     cx,0                    ;Set 16 bit seg.
        test    eax,1 shl 25
        jnz     load1_Default
        mov     cx,1
        test    eax,1 shl 26            ;32 bit seg?
        jnz     load1_Default
        mov     cx,0                    ;Set 16 bit seg.
load1_Default:
        Sys     CodeSel
        ;
load1_SegDone:
        pop     esi
        pop     ecx
        pop     ebx
        add     esi,8                   ;next definition.
        add     ebx,8                   ;next selector.
        dec     ecx
        jnz     load1_SegLoop
;
;Close the input file.
;
        xor     ebx,ebx
        xchg    ebx,[load1._Handle]
        mov     ah,3eh
        int     21h
;
;Check if this is an exec or just a load.
;
        cmp     [load1._Flags],0
        jz      load1_Exec

;
;Switch back to parents PSP if this is a debug load.
;
        cmp     [load1._Flags],2
        jz      load1_NoPSwitch2
        push    fs
        mov     fs,[load1._PSP]
        mov     bx,fs:[EPSP_Struc.EPSP_Parent]
        pop     fs
        mov     ah,50h                  ;set PSP
        int     21h
;        mov     _ebp,[load1._ObjMem]
        mov     eax,[load1._ObjMem]
;--- this is to be returned by the "real" EBP register        
        mov     [ebp+0],eax
;
;Return program details to caller.
;
load1_NoPSwitch2:
        mov     edx,[load1._EntryEIP]
        mov     cx,[load1._EntryCS]
        mov     eax,[load1._EntryESP]
        mov     bx,[load1._EntrySS]
        mov     si,[load1._EntryES]
        mov     di,[load1._EntryDS]
        clc
        jmp     load1_exit
;
;Run it.
;
load1_Exec:
        mov     eax,[load1._Flags]
        mov     ebx,[load1._EntryEIP]
        mov     cx,[load1._EntryCS]
        mov     edx,[load1._EntryESP]
        mov     si,[load1._EntrySS]
        mov     di,[load1._PSP]
        push    ebp
        mov     bp,[load1._EntryDS]
        call    ExecModule
        pop     ebp
        clc
;
;Shut down anything still hanging around.
;
load1_error:
        pushfd
        push    eax
;
;Make sure file is closed.
;
        pushfd
        xor     ebx,ebx
        xchg    ebx,[load1._Handle]
        or      ebx,ebx
        jz      load1_NoClose
        mov     ah,3eh
        int     21h
;
;Make sure all work spaces are released.
;
load1_NoClose:
        xor     esi,esi
        xchg    esi,[load1._ObjMem]
        or      esi,esi
        jz      load1_NoObjRel
        Sys     RelMemLinear32
;
;Restore previous state.
;
load1_NoObjRel:
        popfd
        jnc     load1_RelPSP
        cmp     [load1._PSP],0
        jz      load1_NoRelRes
;
;Restore vectors & DPMI state.
;
load1_RelPSP:
        push    ds
        pop     gs
        push    ds
        pop     fs
        push    ds
        pop     es
        mov     eax,[load1._Flags]
        mov     bx,[load1._PSP]
        call    DeletePSP
;
;Return to caller.
;
load1_NoRelRes:
        pop     eax
        popfd
        ;
load1_exit:
        ;

        ret
;
;Not enough memory error.
;
load1_mem_error:

        mov     ax,3
        stc
        jmp     load1_error
;
;Couldn't find the file.
;
load1_no_file_error:
        mov     ax,1
        stc
        jmp     load1_error
;
;Fixup type we don't understand.
;
IFDEF DEBUGLE
load1_bad_fixup1:
        push    1
        jmp     load1_bad_fixupx
load1_bad_fixup2:
        push    2
        jmp     load1_bad_fixupx
load1_bad_fixup3:
        push    3
        jmp     load1_bad_fixupx
load1_bad_fixup4:
        push    4
        jmp     load1_bad_fixupx
load1_bad_fixup5:
        push    5
        jmp     load1_bad_fixupx
load1_bad_fixup6:
        push    6
        jmp     load1_bad_fixupx
load1_bad_fixup7:
        push    7
        jmp     load1_bad_fixupx
load1_bad_fixup8:
        push    8
        jmp     load1_bad_fixupx
load1_bad_fixup9:
        push edx
        movzx edx,dh
        @dprintf -1,<"4X">,edx
        pop edx
        push    9
load1_bad_fixupx:
        pop     eax
        @dprintf -1,<" Bad fixup: %u",10>,eax
ENDIF

load1_bad_fixup:
if 0
        mov     eax,[load1._EntryEIP]   ;Get the relocation number.
;        push    ds
;        mov     ds,cs:apiDSeg16
;        assume ds:GROUP16
        mov     b[ErrorM11_0+0]," "
        mov     b[ErrorM11_0+1]," "
        mov     b[ErrorM11_0+2]," "
        mov     ecx,8
        mov     edi,offset ErrorM11_1
        call    Bin2Hex
;        pop     ds
;        assume ds:GROUP32
endif
        mov     ax,2
        stc
        jmp     load1_error
;
;Not an LE file.
;Corrupt file or file we don't understand.
;
load1_file_error:
IFDEF DEBUGLE
        @dprintf -1,<"File error type 1",10>
ENDIF
        mov     ax,2
        stc
        jmp     load1_error

LoadLE  endp


