
;--- structs & equates

McbChunkStruc struct
mcbChunkLast    dd ?       ;+0 pointer to previous chunk in chain.
mcbChunkNext    dd ?       ;+4 pointer to next chunk in chain.
mcbBiggest      dw ?       ;+8 biggest free block in this chunk.
mcbChunkSize    dw ?       ;+10 size of this chunk.
mcbChunkSel     dw ?       ;+12 chunk's selector.
                dw ?       ;+14
McbChunkStruc ends

MCBCHUNKLEN     equ sizeof McbChunkStruc  ;length of chunk control entry.

McbStruc struct
mcbID           db ?       ;+0 ID for corruption checking.
mcbLast         db ?       ;+1 previous MCB pointer status.
mcbLastSize     dw ?       ;+2 pointer to previous MCB.
mcbFreeUsed     db ?       ;+4 free or used status.
mcbNext         db ?       ;+5 next MCB pointer status.
mcbNextSize     dw ?       ;+6 pointer to next MCB.
McbStruc ends

MCBLEN          equ sizeof McbStruc       ;length of an MCB entry.

MZHeader struct
		MZHdr <>
		db 3Ch - $ dup (?)
NewHdrPos	dd ?	;3C new header position            
MZHeader ends

;--- stack frame for int 31h API and int 21h extension

IFRM struct
union
_EDI	dd ?
_DI		dw ?
ends
union
_ESI	dd ?
_SI		dw ?
ends
union
_EBP	dd ?
_BP		dw ?
ends
union
		dd ?
		dw ?
ends
union
_EBX	dd ?
_BX		dw ?
struct
_BL		db ?
_BH		db ?
ends
ends
union
_EDX	dd ?
_DX		dw ?
struct
_DL		db ?
_DH		db ?
ends
ends
union
_ECX	dd ?
_CX		dw ?
struct
_CL		db ?
_CH		db ?
ends
ends
union
_EAX	dd ?
_AX		dw ?
struct
_AL		db ?
_AH		db ?
ends
ends
_GS		dd ?
_FS		dd ?
_ES		dd ?
_DS		dd ?
union
i16		IRET16 <>
i32		IRET32 <> 
ends
IFRM ends

;--- macros

;*******************************************************************************
;Call old int 31h handler.
;*******************************************************************************
cwAPI_CallOld   macro
        call    cs:[int31call]
endm

cwAPI_CallOldCC macro
        call    cs:[int31callcc]
endm

if 0
;*******************************************************************************
;Convert character in AL to upper case.
;*******************************************************************************
UpperChar       macro
        local __0
        cmp     al,61h          ; 'a'
        jb      __0
        cmp     al,7Ah          ; 'z'
        ja      __0
        and     al,5Fh          ;convert to upper case.
__0:    ;
endm
endif


_DATA32 segment

	align 4

;-------------------------------------------------------------------------
;
OldInt31 df 0
	align 4
OldInt1Br       dd 0
Int1bhcall      dd 0
CtrlBrkEventTab RealRegsStruc <>
	align 4
OldInt23        df 0
OldInt24        df 0
CriticalPrompt  db 13,10,'Critical Error: Abort, Retry, Ignore, Fail? $'
CriticalKeys    db 'aArRiIfF'
CriticalCodes   db 2,2,1,1,0,0,3,3   ; must follow CriticalKeys!
nullCmdl        db 0

	align 4
;--- stack (size 1200h) used in dpmi mode only for int 31h, ax=FF01/FF02 (IntXX/FarCallReal)
DPMIStackSSSP   label dword
DPMIStackOfs    dw ?       ; real-mode SP
DPMIStackSeg    dw 0       ; real-mode SS

api61_SelectorBase   dd ?
api61_SelectorSize   dd ?
api63_SelectorBase   dd ?
api63_SelectorSize   dd ?
api73_BlockBase      dd ?
api73_BlockHandle    dd ?
api73_BlockSize      dd ?
api74_dpmembuff      db 30h dup (?)	;DPMI meminfo struct (9 dwords, 12 bytes reserved)
api75_OldEBX         dd 0
api75_OldESI         dd 0

api82_Command        df 0  ;CreatePSP() local vars
api82_PSP            dw 0
api82_Name           df 0
api82_Environment    dw 0
api82_Flags          dd 0

api83_Flags          dd ?  ;DeletePSP() local vars
api83_PSP            dw ?

DllNameSpace    db 256 dup (?)  ;FindModule local variables
MODNameSpace    db 256 dup (?)

api86_ID      db 256 dup (?)    ;SearchModule local variables
TemporaryDTA  db 80h DUP (?)    ;temporary DTA storage so PSP isn't munged, MED 01/03/96
api86_Handle  dw 0
api86_DTA     df 0
api86_Path    df 0
api86_Name    df 0
EntryDTAAddress df 0            ; Entry DTA address
api86_Length  dd 0
api86_Count   dd 0
api86_Mask    dd 0
api86_Masks label byte
        db ".DLL"
        db ".EXE"
        db 0

	align 4
api88_Name        df 0  ;_Exec local variables
api88_Handle      dw 0
api88_Flags       dd 0
api88_Command     df 0
api88_Environment dw 0
api88_ExecAdd     dd 0  ;address binary loader (Load3P/LoadLE)
api90_OldInt21h   df 0  ;ExecModule local variables
	align 4
api90_PSP         dd 0
	align 2
OldInt1Bp       df 0
Int1bRegs       RealRegsStruc <>

	align 4
MouseETarget     dd 0,0    ; mouse event target???
;
apiSystemFlags  dw 0,0
DescriptorBuffer Desc <>

apiDSeg16       dd ?	;data selector GROUP16
apiDSeg32       dd ?	;data selector GROUP32
;
apiNewHeader    NewHeaderStruc <>       ;make space for a header.
	org apiNewHeader
apiMZHeader     MZHeader <>
;
LastResource    dd 0,0
;
PatchTable      dd 16 dup (0)  ;cwApi_GetPatch (0fffah)

mcbLastChunk    dd 0
ExecMCount      dd 0

int31call       dd 0
int31callcc     dd 0

UserTermRoutine16 label PF16
UserTermRoutine DF 0       ; user termination address ( int 31h, ax=ff31h )
UserTermDump    DF 0       ; dump location for register info ( int 31h, ax=ff31h )
UserTermFlag    DB 0       ; modified by int 31h, ax=0ff31h (set user termination proc)

	align 4

;--- CauseWay int 31h functions, AH=0FFh

APICallTable label dword
        dd cwAPI_Info               ;00
        dd cwAPI_IntXX              ;01
        dd cwAPI_FarCallReal        ;02
        dd cwAPI_GetSel             ;03
        dd cwAPI_RelSel             ;04
        dd cwAPI_CodeSel            ;05
        dd cwAPI_AliasSel           ;06
        dd cwAPI_GetSelDet          ;07
        ;
        dd cwAPI_GetSelDet32        ;08
        dd cwAPI_SetSelDet          ;09
        dd cwAPI_SetSelDet32        ;0A
        dd cwAPI_GetMem             ;0B
        dd cwAPI_GetMem32           ;0C
        dd cwAPI_ResizeMem          ;0D
        dd cwAPI_ResizeMem32        ;0E
        dd cwAPI_RelMem             ;0F
        ;
        dd cwAPI_GetMemLinear       ;10
        dd cwAPI_GetMemLinear32     ;11
        dd cwAPI_ResizeMemLinear    ;12
        dd cwAPI_ResizeMemLinear32  ;13
        dd cwAPI_RelMemLinear       ;14
        dd cwAPI_RelMemLinear32     ;15
        dd cwAPI_GetMemNear         ;16
        dd cwAPI_ResizeMemNear      ;17
        ;
        dd cwAPI_RelMemNear         ;18
        dd cwAPI_Linear2Near        ;19
        dd cwAPI_Near2Linear        ;1A
        dd cwAPI_LockMem            ;1B
        dd cwAPI_LockMem32          ;1C
        dd cwAPI_UnLockMem          ;1D
        dd cwAPI_UnLockMem32        ;1E
        dd cwAPI_LockMemNear        ;1F
        ;
        dd cwAPI_UnLockMemNear      ;20
        dd cwAPI_GetMemDOS          ;21
        dd cwAPI_ResizeMemDOS       ;22
        dd cwAPI_RelMemDOS          ;23
        dd cwAPI_Exec               ;24
        dd cwAPI_GetDOSTrans        ;25
        dd cwAPI_SetDOSTrans        ;26
        dd cwAPI_GetMCBSize         ;27
        ;
        dd cwAPI_SetMCBSize         ;28
        dd cwAPI_GetSels            ;29
        dd cwAPI_cwLoad             ;2A
        dd cwAPI_cwcInfo            ;2B
        dd cwAPI_GetMemSO           ;2C
        dd cwAPI_ResizeMemSO        ;2D
        dd cwAPI_RelMemSO           ;2E
        dd cwAPI_UserDump           ;2F

        dd cwAPI_SetDump            ;30
        dd cwAPI_UserErrTerm        ;31
        dd cwAPI_CWErrName          ;32
APILOWMAX equ ($ - APICallTable) / 4

APIHIGHSTART equ 0F7h

        dd cwAPI_DbgNtfModLoad      ;F7 (dummy)
        ;
        dd cwAPI_DbgNtfModUnload    ;F8 (dummy)
        dd cwAPI_ID                 ;F9
        dd cwAPI_GetPatch           ;FA (useless)
        dd cwAPI_cwcLoad            ;FB
        dd cwAPI_LinearCheck        ;FC
        dd cwAPI_ExecDebug          ;FD
        dd cwAPI_Cleanup            ;FE - calls LoseFileHandles, a dummy
        dd 0                        ;FF
;
apiExtraCallTable label byte
        dw 0600h
        dd dpmiAPI_Lock
        dw 0601h
        dd dpmiAPI_UnLock
        dw 0303h
        dd dpmiAPI_GetCallBack
        dw 0304h
        dd dpmiAPI_RelCallBack
endapiExtraCallTable label byte

_DATA32 ends

int31call16:
        pushf
        db   66h
        push cs
        push lowword offset @ret
        jmp  cs:[OldInt31]                 ;pass it onto previous handler.
@ret:
        ret
int31call16cc:
        pushf
        db   66h
        push cs
        push lowword offset @ret2
        jmp  cs:[OldInt31]                 ;pass it onto previous handler.
@ret2:
        push eax
        lahf
        mov  b[ebp].IFRM.i16._fl,ah
        pop  eax
        ret
int31call32:
        pushfd
        call cs:[OldInt31]                 ;pass it onto previous handler.
        ret
int31call32cc:
        pushfd
        call cs:[OldInt31]                 ;pass it onto previous handler.
        push eax
        lahf
        mov  b[ebp].IFRM.i32._fl,ah
        pop  eax
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;This must be just before cwAPIpatch
;
cwIdentity      db "CAUSEWAY"
cwMajorVersion  db 0
cwMinorVersion  db 0
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;CW API int 31h patch.
;
cwAPIpatch      proc    near

ifndef NOI21RMHOOK
;
;Check if we're allowed to interfere.
;
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        cmp     Int21hExecCount,0
        pop     ds
        assume ds:nothing
        jnz     api1_jmpprev
endif
        push    ds
        push    es
        push    fs
        push    gs
        pushad
;
;Check if this is a CauseWay function.
;
        cmp     ah,255
        jnz     api1_checkextra
        cmp     al,APILOWMAX
        jc      @F
        cmp     al,APIHIGHSTART
        jc      api1_Nope
        sub     al,APIHIGHSTART - APILOWMAX
@@:
        movzx   edi,al
        lea     edi,[edi*4+offset APICallTable]
        cmp     DWORD PTR cs:[edi],0
        jz      api1_Nope
        jmp     api1_GotCall
;
;Scan the table of other relevant functions.
;
api1_checkextra:
        mov     edi,offset apiExtraCallTable+2
api1_extra:
        cmp     ax,cs:[edi-2]
        jz      api1_GotCall
        add     edi,4+2
        cmp     edi,offset endapiExtraCallTable
        jb      api1_extra
;
;Not an internal function or anything we want to interfere with so pass control
;to original handler.
;
api1_Nope:
        popad                       ;Restore registers.
        lea     esp,[esp+4*4]       ;no need to restore segment registers
api1_jmpprev:
        jmp     cs:[OldInt31]       ;pass it onto previous handler.

api1_GotCall:
        cld                                  ;Default direction.
        test    b cs:apiSystemFlags,1
        jz      api1_32Bit0
        movzx   ebp,sp
        and     b [ebp].IFRM.i16._fl,not 1   ;clear carry.
        test    b [ebp].IFRM.i16._fl+1,2     ;Were interrupts enabled?
        jmp     api1_cres
api1_32Bit0:
        mov     ebp,esp                      ;Make registers addressable.
        and     b [ebp].IFRM.i32._fl,not 1   ;clear carry.
        test    b [ebp].IFRM.i32._fl+1,2     ;Were interrupts enabled?
api1_cres:
;
;turning interrupts back on?
;
        jz      api1_NoInts
        sti
api1_NoInts:
;
;Call the function handler.
;
        call    DWORD PTR cs:[edi]           ;Pass control to handler.
        popad
        pop     gs
        pop     fs
        pop     es
        pop     ds
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api1_32Bit1
        iret
api1_32Bit1:
        iretd                       ;Return to caller.
;
cwAPIpatch      endp

        assume ds:nothing

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;AX     = 0600h
;BX:CX  = Starting linear address of memory to lock
;SI:DI  = Size of region to lock in bytes
;
dpmiAPI_Lock    proc    near
        mov     ax,[ebp].IFRM._AX
        mov     bx,[ebp].IFRM._BX
        mov     cx,[ebp].IFRM._CX
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        pushad
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        mov     edx,ebx     ;key value
        mov     ecx,esi     ;second value
        mov     ax,Res_LOCK
        call    RegisterResource
        popad
        cwAPI_CallOldCC
        jnc     api2_0
        mov     [ebp].IFRM._AX,ax
api2_0: 
        ret
dpmiAPI_Lock    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;AX     = 0601h
;BX:CX  = Starting linear address of memory to unlock
;SI:DI  = Size of region to unlock in bytes
;
dpmiAPI_UnLock  proc    near
        mov     ax,[ebp].IFRM._AX
        mov     bx,[ebp].IFRM._BX
        mov     cx,[ebp].IFRM._CX
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        pushad
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        mov     edx,ebx    ;key value
        mov     ecx,esi    ;second value
        mov     ax,Res_LOCK
        call    ReleaseResource
        popad
        cwAPI_CallOldCC
        jnc     api3_0
        mov     [ebp].IFRM._AX,ax
api3_0:
        ret
dpmiAPI_UnLock  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;AX = 0303h
;DS:(E)SI = Selector:Offset of procedure to call
;ES:(E)DI = Selector:Offset of real mode call structure
;
;Returns
;
;If function was successful:
;Carry flag is clear.
;CX:DX = Segment:Offset of real mode call address
;
dpmiAPI_GetCallBack proc near
        mov     ax,[ebp].IFRM._AX
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._ESI
        mov     es,[ebp].IFRM._ES
        mov     edi,[ebp].IFRM._EDI
        cwAPI_CallOldCC
        jc      api4_9
;
        mov     [ebp].IFRM._CX,cx
        mov     [ebp].IFRM._DX,dx
;
        shl     ecx,16
        mov     cx,dx
        mov     edx,ecx
        mov     ecx,esi
        mov     ebx,ds
        mov     ax,Res_CALLBACK
        call    RegisterResource
;
api4_9:
        ret
dpmiAPI_GetCallBack endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;AX = 0304h
;CX:DX = Real mode call-back address to free
;
dpmiAPI_RelCallBack proc near
        mov     ax,[ebp].IFRM._AX
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        cwAPI_CallOldCC
        jc      api5_9
;
        shl     ecx,16
        mov     cx,dx
        mov     edx,ecx
        mov     ax,Res_CALLBACK
        call    ReleaseResource
;
api5_9:
        ret
dpmiAPI_RelCallBack endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Call user termination routine on exception
;
;On Entry:
;
; AX = 0ff31h
; CL = 0 if 16-bit termination routine
; CL = nonzero if 32-bit termination routine
; DS:[E]SI = user termination routine address, if ES is zero or an invalid
;  selector value, then the user termination routine call is removed
; ES:[E]DI = user information dump area
;
;Returns:
;
; None
;
cwAPI_UserErrTerm       PROC    NEAR
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     UserTermFlag,0          ; initialize flag
        mov     edx,[ebp].IFRM._DS
        lsl     eax,edx
        jnz     uetret                  ; invalid selector
        mov     al,[ebp].IFRM._CL       ; get bitness flag
        test    al,al
        setne   al                      ; 0 if 0, 1 if nonzero
        inc     eax                     ; 1 if 16-bit, 2 if 32-bit
        mov     UserTermFlag,al
        cmp     al,1                    ; see if 16-bit
        jne     uet32                   ; no

        mov     eax,[ebp].IFRM._ESI
        mov     WORD PTR UserTermRoutine+0,ax
        mov     WORD PTR UserTermRoutine+2,dx
        movzx   eax,[ebp].IFRM._DI
        jmp     uetstoreesedi
uet32:
        mov     eax,[ebp].IFRM._ESI
        mov     DWORD PTR UserTermRoutine+0,eax
        mov     WORD PTR UserTermRoutine+4,dx
        mov     eax,[ebp].IFRM._EDI
uetstoreesedi:
        mov     DWORD PTR UserTermDump+0,eax
        mov     eax,[ebp].IFRM._ES
        mov     WORD PTR UserTermDump+4,ax

uetret:
        ret
cwAPI_UserErrTerm       ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Disable/enable error display and CW.ERR creation
;
;On Entry:
;
; AX = 0ff30h
; CL = 0, disable error display and CW.ERR file creation
; CL = nonzero, enable error display and CW.ERR file creation
;
;Returns:
;
; None
;
cwAPI_SetDump   PROC    NEAR
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     al,[ebp].IFRM._CL
        mov     EnableDebugDump,al
        ret
cwAPI_SetDump   ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
; change CW.ERR file name, with optional path/drivespec
;
;On Entry:
;
; AX = 0ff32h
; CX:[E]DX = new CW.ERR ASCIIZ file name
;
;Returns:
;
; None
;
cwAPI_CWErrName PROC    NEAR
        mov     edx,[ebp].IFRM._EDX
        test    BYTE PTR cs:apiSystemFlags,1
        jz      cen2
        movzx   edx,dx                  ; 16-bit, zero high word of edx
cen2:
        mov     ax,[ebp].IFRM._CX
        mov     es,eax
        mov     ebx,OFFSET NewCWErrName ; ds:ebx -> destination, es:edx -> source
        mov     cl,80                   ; don't allow more than 80 chars in file name
        movzx   ecx,cl
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
cenloop:
        mov     al,es:[edx]
        mov     [ebx],al
        inc     edx
        inc     ebx
        test    al,al                   ; stop at null terminator
        loopnz  cenloop
        ret
cwAPI_CWErrName ENDP

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Setup user buffer display in CW.ERR file
;
;On Entry:
;
; AX = 0ff2fh
; ES:[E]DI -> user buffer to display in CW.ERR
; CX = count of bytes to display from buffer in CW.ERR
; BL = 'A' if ASCII dump (non-binary display of bytes, control characters
;      display as periods)
; BH = nonzero if preset ASCII buffer to word value, ignored for non-ASCII
; DX = word value to fill ASCII dump buffer if BH is nonzero, ignored
;      for non-ASCII
;
;Returns:
;
; Carry set on ASCII dump invalid buffer address
;
cwAPI_UserDump  PROC    NEAR
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     eax,[ebp].IFRM._ES
        mov     edi,[ebp].IFRM._EDI
        mov     ecx,[ebp].IFRM._ECX
        verr    ax
        jnz     udfail                  ; can't read to user selector
        mov     DebugUserSel,ax
        mov     es,eax
        lsl     eax,eax
        cmp     eax,10000h
        jnc     @F
        movzx   edi,di
@@:
        mov     DebugUserOff,edi
        mov     DebugUserCount,cx

; check for out of bounds
        movzx   edx,cx
        add     edx,edi
        jc      udfail
        dec     edx                     ; make relative 0
        cmp     eax,edx
        jb      udfail

        mov     al,[ebp].IFRM._BL
        cmp     al,'A'
        je      aflag
        xor     al,al
aflag:
        mov     DebugAsciiFlag,al
        test    al,al
        je      udret                   ; if not ASCII then no fill
        mov     al,[ebp].IFRM._BH       ; check fill flag
        test    al,al                   ; no fill, carry flag reset by default
        je      udret

; fill the allocation with value in dx to count specified in cx
        mov     eax,es
        verw    ax
        jnz     udfail                  ; can't write to user selector
        mov     ax,[ebp].IFRM._DX
        push    ax
        push    ax
        pop     eax
        push    ecx
        shr     ecx,1
        rep     stosd
        pop     ecx
        and     ecx,3
        je      udsuccess
        mov     es:[edi],al             ; finish off remainder bytes
        dec     ecx
        je      udsuccess
        inc     edi
        mov     es:[edi],ah
        dec     ecx
        je      udsuccess
        inc     edi
        mov     es:[edi],al
udsuccess:
        clc                             ; flag success
udret:
        ret
udfail:
        mov     DebugUserCount,0
        jmp     exit_with_carry         ; flag failure
cwAPI_UserDump  ENDP


exit_with_carry:
        test    BYTE PTR cs:apiSystemFlags,1
        jz      @F
        or      b[ebp].IFRM.i16._fl,1
        ret
@@:
        or      b[ebp].IFRM.i32._fl,1
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Notify Debugger of new module loaded.
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cwAPI_DbgNtfModLoad proc    near
        ret
cwAPI_DbgNtfModLoad endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Notify Debugger of module unloading.
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cwAPI_DbgNtfModUnload   proc    near
        ret
cwAPI_DbgNtfModUnload   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get CauseWay identifier, return PageDirLinear and Page1stLinear info
;AX = 0xfff9
;
;Returns:
;
;ESI    - PageDirLinear  (valid in Raw/VCPI mode only!!!)
;EDI    - Page1stLinear  (valid in Raw/VCPI mode only!!!)
;ECX:EDX = "CAUSEWAY"
;
cwAPI_ID        proc    near
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     esi,PageDirLinear
        mov     edi,Page1stLinear
        pop     ds
        mov     ecx,"CAUS"
        mov     edx,"EWAY"
        mov     [ebp].IFRM._ESI,esi
        mov     [ebp].IFRM._EDI,edi
        mov     [ebp].IFRM._EDX,edx
        mov     [ebp].IFRM._ECX,ecx
        ret
cwAPI_ID        endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get patch table address. This table isn't used!
;AX = 0xfffa
;
;Returns:
;
;EDX    - Linear address of patch table.
;
cwAPI_GetPatch  proc    near
        mov     ebx,cs:apiDSeg32
        Sys     GetSelDet32
        add     edx,offset PatchTable
        mov     [ebp].IFRM._EDX,edx
        ret
cwAPI_GetPatch  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get address of transfer buffer.
;Ax=0ff25
;
;Returns:
;
;BX     - Real mode segment
;DX     - Protected mode selector.
;ECX    - Buffer size.
;
cwAPI_GetDOSTrans proc near
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd    ; use _cwEnd so the assembler assumes a 16-bit segment
        mov     ax,es:[EPSP_Struc.EPSP_TransReal]
        mov     [ebp].IFRM._BX,ax
        mov     ax,es:[EPSP_Struc.EPSP_TransProt]
        mov     [ebp].IFRM._DX,ax
        mov     eax,es:[EPSP_Struc.EPSP_TransSize]
        mov     [ebp].IFRM._ECX,eax
        ret
        assume es:nothing
cwAPI_GetDOSTrans endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set real mode transfer buffer address.
;
;On Entry:
;
;BX     - Real mode segment.
;DX     - Protected mode selector.
;ECX    - Buffer size.
;
cwAPI_SetDOSTrans proc near
        mov     bx,[ebp].IFRM._BX
        mov     dx,[ebp].IFRM._DX
        mov     ecx,[ebp].IFRM._ECX
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd    ; use _cwEnd so the assembler assumes a 16-bit segment
;        assume ds:GROUP32
        mov     es:[EPSP_Struc.EPSP_TransReal],bx
        mov     es:[EPSP_Struc.EPSP_TransProt],dx
        cmp     ecx,10000h
        jc      api13_0
        mov     ecx,65535
api13_0:
        mov     DWORD PTR es:[EPSP_Struc.EPSP_TransSize],ecx
        ret
        assume es:nothing
cwAPI_SetDOSTrans endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get various useful variable contents.
;
;Returns:
;
;AX     - 0-4G selector.
;BX     - Current PSP selector.
;ECX    - Transfer buffer size.
;DX     - Transfer buffer real mode segment.
;ESI    - Transfer buffer offset.
;DI     - System flags.
;ES     - Transfer buffer protected mode selector.
;
cwAPI_Info      proc    near
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     di,w[SystemFlags]
        mov     ax,RealSegment
        mov     bx,PSPSegment
        mov     es,ebx
        assume es:_cwEnd    ; use _cwEnd so the assembler assumes a 16-bit segment
        mov     dx,es:[EPSP_Struc.EPSP_TransReal]
        mov     ecx,es:[EPSP_Struc.EPSP_TransSize]
        mov     es,es:[EPSP_Struc.EPSP_TransProt]
        assume es:nothing
        xor     esi,esi
        mov     [ebp].IFRM._AX,ax
        mov     [ebp].IFRM._BX,bx
        mov     [ebp].IFRM._ECX,ecx
        mov     [ebp].IFRM._DX,dx
        mov     [ebp].IFRM._ESI,esi
        mov     [ebp].IFRM._DI,di
        mov     [ebp].IFRM._ES,es
        ret
cwAPI_Info      endp

;--- run real-mode code ( INT xx or far proc )

runrmcode proc        
        mov     edi,[ebp].IFRM._EDI
        mov     es,[ebp].IFRM._ES
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     api15_0
        movzx   edi,di
api15_0:
        xor     eax,eax
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cmp     DPMIStackSeg,ax   ;"DPMI stack" allocated?
        jz      NoStack           ;if no, use the one provided by the DPMI host (or DPMIEMU)
        sub     DPMIStackOfs,RawStackDif
        mov     eax,DPMIStackSSSP
        add     ax,RawStackDif
NoStack:
        mov     es:RealRegsStruc.Real_SSSP[edi],eax
        call    ecx
        cwAPI_CallOldCC
        cmp     DPMIStackSeg,0
        jz      DoneStack1
        add     DPMIStackOfs,RawStackDif
DoneStack1:
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api15_Use32Bit8
        mov     ebx,IFRM.i16._fl
        jmp     api15_Use16Bit8
api15_Use32Bit8:
        mov     ebx,IFRM.i32._fl
api15_Use16Bit8:
        mov     ax,ss:[ebp+ebx]
        and     ax,0000011000000000b                ;retain IF & DF.
        and     es:RealRegsStruc.Real_Flags[edi],1111100111111111b  ;lose IF & DF.
        or      es:RealRegsStruc.Real_Flags[edi],ax
        ret

runrmcode endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Pass control to a real mode interrupt handler.
;
;On Entry:
;
;BL     - INT number.
;ES:[E]DI       - Real mode register structure.
;
cwAPI_IntXX     proc    near
        mov     ecx, offset intxxproc
        jmp     runrmcode
intxxproc:
        test    BYTE PTR cs:apiSystemFlags,1
        jz      medUse32Bit8
        mov     ebx,IFRM.i16._fl
        jmp     medUse16Bit8
medUse32Bit8:
        mov     ebx,IFRM.i32._fl
medUse16Bit8:
        mov     ax,ss:[ebp+ebx]
        and     ah,11111100b                        ;clear Trap and INT flag.
        mov     es:RealRegsStruc.Real_Flags[edi],ax ; explicitly set flags on DPMI 300h call
        xor     cx,cx                               ;No stack parameters.
        mov     bh,ch                               ;no flags.
        mov     bl,ss:[ebp].IFRM._BL
        mov     ax,300h
        ret
cwAPI_IntXX     endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Pass control to a real mode far routine.
;
;On Entry:
;
;ES:[E]DI       - Real mode register structure.
;
cwAPI_FarCallReal proc near
        mov     ecx, offset farxxproc
        jmp     runrmcode
farxxproc:
        pushf
        pop     es:RealRegsStruc.Real_Flags[edi]
        xor     cx,cx                           ;No stack parameters.
        xor     bx,bx                           ;no flags.
        mov     ax,301h
        ret
cwAPI_FarCallReal endp

        assume ds:GROUP32

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate a selector.
;
;Returns:
;
;Carry set on error (no more selectors) else,
;
;BX     - new selector.
;
cwAPI_GetSel    proc    near
        xor     eax,eax
        xor     ebx,ebx
        call    _SetSelector
        jc      exit_with_carry
        mov     [ebp].IFRM._BX,bx
        ret
cwAPI_GetSel    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate multiple selectors.
;
;On Entry:
;
;CX     - Number of selectors.
;
;Returns:
;
;Carry set on error (not enough selectors) else,
;
;BX     - Base selector.
;
cwAPI_GetSels   proc    near
;
;Get selectors from DPMI.
;
        mov     cx,[ebp].IFRM._CX
        mov     ax,0000h
        cwAPI_CallOldCC
        jc      api18_9
;
        mov     [ebp].IFRM._BX,ax
        movzx   edx,ax
        movzx   ecx,cx
        mov     ax,Res_SEL
api18_0:
        call    RegisterResource
        add     edx,8
        dec     ecx
        jnz     api18_0
;
api18_9:
        ret
cwAPI_GetSels   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release a selector - API 0FF04h
;
;On Entry:
;
;BX     - Selector to release.
;
cwAPI_RelSel    proc    near
        mov     bx,[ebp].IFRM._BX
        mov     ds,[ebp].IFRM._DS
        mov     es,[ebp].IFRM._ES
        mov     fs,[ebp].IFRM._FS
        mov     gs,[ebp].IFRM._GS
        @dprintf DOPT_SEL,<"cwAPI_RelSel(bx=%X)",10>, ebx
        call    _RelSelector
        jc      exit_with_carry
        mov     [ebp].IFRM._DS,ds
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._FS,fs
        mov     [ebp].IFRM._GS,gs
        ret
cwAPI_RelSel    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Force a selector to be type CODE.
;
;On Entry:
;
;BX     - Selector.
;
cwAPI_CodeSel   proc    near
        mov     bx,[ebp].IFRM._BX
        call    _CodeSelector
        jc      exit_with_carry
        ret
cwAPI_CodeSel   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate a selector with type DATA that maps same memory as another selector.
;
;On Entry:
;
;BX     - selector.
;
;Returns:
;
;AX     - New selector.
;
cwAPI_AliasSel  proc    near
        mov     ax,000ah
        mov     bx,[ebp].IFRM._BX
        cwAPI_CallOldCC
        jc      api21_9
;
        mov     [ebp].IFRM._AX,ax
        push    eax
        push    edx
        movzx   edx,ax
        mov     ax,Res_SEL
        call    RegisterResource
        pop     edx
        pop     eax
;
api21_9:
        ret
cwAPI_AliasSel  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get selector base & limit.
;
;On Entry:
;
;BX     - selector.
;
;Returns:
;
;CX:DX  - Linear base.
;SI:DI  - Byte granular limit.
;
cwAPI_GetSelDet proc near
        mov     bx,[ebp].IFRM._BX
        call    _DetSelector
        jc      exit_with_carry
;
        mov     ecx,eax         ;get base.
        mov     dx,cx
        shr     ecx,16
        mov     esi,ebx         ;get limit.
        mov     di,si
        shr     esi,16
        mov     [ebp].IFRM._CX,cx
        mov     [ebp].IFRM._DX,dx
        mov     [ebp].IFRM._SI,si
        mov     [ebp].IFRM._DI,di
        ret
cwAPI_GetSelDet endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get selector base & limit.
;
;On Entry:
;
;BX     - selector.
;
;Returns:
;
;EDX    - base
;ECX    - limit
;
cwAPI_GetSelDet32 proc near
        mov     bx,[ebp].IFRM._BX
        call    _DetSelector
        jc      exit_with_carry
        mov     [ebp].IFRM._EDX,eax
        mov     [ebp].IFRM._ECX,ebx
        ret
cwAPI_GetSelDet32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set selectors base & limit.
;
;On Entry:
;
;BX     - selector
;CX:DX  - Linear base.
;SI:DI  - Byte granular limit.
;
cwAPI_SetSelDet proc near
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        shl     ecx,16          ;Get base to somewhere useful.
        mov     cx,dx
        mov     eax,ecx
        shl     esi,16          ;get limit to somewhere useful.
        mov     si,di
        mov     ebx,esi
        mov     cx,[ebp].IFRM._BX
        call    _SizeSelector
        jc      exit_with_carry
        ret
cwAPI_SetSelDet endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set a selectors details.
;
;On Entry:
;
;BX     - selector
;EDX    - base
;ECX    - limit.
;
cwAPI_SetSelDet32 proc near
        mov     cx,[ebp].IFRM._BX
        mov     eax,[ebp].IFRM._EDX
        mov     ebx,[ebp].IFRM._ECX
        call    _SizeSelector
        jc      exit_with_carry
        ret
cwAPI_SetSelDet32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate some memory with a selector attached.
;
;On Entry:
;
;CX:DX  - Size of block required in bytes. (-1:-1 to get maximum memory size)
;
;On Exit:
;
;Carry clear if OK &
;
;BX     - Selector to access the block with.
;
;Else if CX:DX was -1, CX:DX is size of largest block available.
;
cwAPI_GetMem    proc    near
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16
        mov     cx,dx

; MED 06/25/97
        push    ds              ; test padding flag
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        test    Pad1Flag,-1
        pop     ds
        assume ds:nothing
        je      gm2             ; padding flag not turned on
        test    ecx,0ffff0000h
        jne     gm2             ; don't pad >64K allocation
        add     ecx,1023
        and     ecx,NOT 1023    ; pad to 1K-boundary allocation
gm2:

        call    mcbGetMemLinear32
        jc      api26_2
        Sys     GetSel
        jc      api26_0
        mov     edx,esi         ;edx=base
        jecxz   api26_3
        dec     ecx             ;limit=length-1
api26_3:
        Sys     SetSelDet32
        mov     [ebp].IFRM._BX,bx
        jmp     api26_1
api26_0:
        call    mcbRelMemLinear32
        jmp     exit_with_carry
        ;
api26_2:
        mov     dx,cx
        shr     ecx,16
        mov     ax,[ebp].IFRM._CX
        shl     eax,16
        mov     ax,[ebp].IFRM._DX
        cmp     eax,-2
        jz      api26_5
        cmp     eax,-1
        jnz     api26_4
api26_5:
        mov     [ebp].IFRM._CX,cx
        mov     [ebp].IFRM._DX,dx
api26_4:
        jmp     exit_with_carry
api26_1:
        ret
cwAPI_GetMem    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate some memory and return selector:offset (16-bit).
;
;On Entry:
;
;CX:DX  - Size of block required in bytes.
;
;On Exit:
;
;Carry set on error else,
;
;SI:DI  - selector:offset of allocated memory.
;
cwAPI_GetMemSO  proc    near
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16
        mov     cx,dx
        call    mcbGetMemLinear32       ;allocate some memory.
        jc      exit_with_carry
        cmp     cs:mcbLastChunk,0       ;DPMI memory?
        jnz     api27_1
;
;Allocate a selector for this memory.
;
        Sys     GetSel
        jc      api27_8
        mov     edx,esi
        jecxz   api27_0
        dec     ecx                     ;limit=length-1
api27_0:
        Sys     SetSelDet32
        xor     dx,dx
        jmp     api27_7
;
;Get chunk's selector.
;
api27_1:
        push    es
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     es,RealSegment
        assume es:nothing
        mov     edi,cs:mcbLastChunk
        mov     bx,es:[edi].McbChunkStruc.mcbChunkSel ;get chunk selector.
        pop     es
        mov     edx,esi
        sub     edx,edi                 ;get blocks offset.
;
api27_7:
        mov     [ebp].IFRM._SI,bx
        mov     [ebp].IFRM._DI,dx
        ret
;
api27_8:
        call    mcbRelMemLinear32
        jmp     exit_with_carry

cwAPI_GetMemSO  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;On Entry:
;
;ECX    - Size of block required in bytes. (-1 to get maximum memory size)
;
;On Exit:
;
;Carry clear if OK &
;
;BX     - Selector to access the block with.
;
;Else if ECX was -1, ECX is size of largest block available.
;
cwAPI_GetMem32  proc near
        mov     ecx,[ebp].IFRM._ECX
        call    mcbGetMemLinear32
        jc      api28_2
        Sys     GetSel
        jc      api28_0
        mov     edx,esi
        jecxz   api28_3
        dec     ecx                     ;limit=length-1
        Sys     SetSelDet32
api28_3:
        mov     [ebp].IFRM._BX,bx
        ret
api28_0:
        call    mcbRelMemLinear32
        jmp     api28_err
        ;
api28_2:
        cmp     [ebp].IFRM._ECX,-1
        jz      api28_5
        cmp     [ebp].IFRM._ECX,-2
        jnz     api28_4
api28_5:
        mov     [ebp].IFRM._ECX,ecx
api28_4:
api28_err:
        jmp     exit_with_carry

cwAPI_GetMem32  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated block of memory.
;
;On Entry:
;
;BX     - Selector for block.
;CX:DX  - New size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK.
;
cwAPI_ResizeMem    proc    near
        mov     bx,[ebp].IFRM._BX
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16                  ;convert new size to 32-bit.
        mov     cx,dx
        push    ecx
        Sys     GetSelDet32             ;Get selector base address.
        mov     esi,edx
        pop     ecx
        jc      api29_9
        call    mcbResizeMemLinear32    ;re-size the memory.
        jc      api29_9
        mov     edx,esi
        dec     ecx
        Sys     SetSelDet32
        ret
api29_9:
        jmp     exit_with_carry
cwAPI_ResizeMem    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated block of memory.
;
;On Entry:
;
;SI:DI  - Selector:offset for block.
;CX:DX  - New size of block required in bytes.
;
;On Exit:
;
;Carry set on error else,
;
;SI:DI  - selector:offset new block address.
;
cwAPI_ResizeMemSO  proc    near
        mov     ds,[ebp].IFRM._DS
        mov     es,[ebp].IFRM._ES
        mov     fs,[ebp].IFRM._FS
        mov     gs,[ebp].IFRM._GS
;
        mov     bx,[ebp].IFRM._SI
        Sys     GetSelDet32             ;get selectors details.
        jc      api30_9
        mov     esi,edx
        movzx   eax,[ebp].IFRM._DI
        add     esi,eax                 ;get memory blocks address.
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16                  ;convert new size to 32-bit.
        mov     cx,dx
        call    mcbResizeMemLinear32    ;re-size the memory.
        jc      api30_9
;
;Check new block type.
;
        cmp     cs:mcbLastChunk,0       ;DPMI memory?
        jnz     api30_1
;
;Update selectors details.
;
        mov     edx,esi
        dec     ecx
        Sys     SetSelDet32
        xor     dx,dx
        jmp     api30_7
;
;Get chunk's selector.
;
api30_1:
        push    es
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     es,RealSegment
        assume es:nothing
        mov     edi,cs:mcbLastChunk
        mov     bx,es:[edi].McbChunkStruc.mcbChunkSel ;get chunk selector.
        pop     es
        mov     edx,esi
        sub     edx,edi                 ;get blocks offset.
;
;Check if the old block had it's own selector.
;
        cmp     [ebp].IFRM._DI,0        ;offset of zero?
        jnz     api30_7
        push    ebx
        mov     bx,[ebp].IFRM._SI
        @dprintf DOPT_MEM,<"cwAPI_ResizeMemSO: calling int 31h, RelSel, bx=%X",10>,ebx
        Sys     RelSel                  ;release the selector.
        pop     ebx
;
api30_7:
        mov     [ebp].IFRM._SI,bx
        mov     [ebp].IFRM._DI,dx
        clc
;
api30_9:
        mov     [ebp].IFRM._DS,ds
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._FS,fs
        mov     [ebp].IFRM._GS,gs
        jc      exit_with_carry
        ret
cwAPI_ResizeMemSO  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated block of memory.
;
;On Entry:
;
;BX     - Selector for block.
;ECX    - New size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK.
;
cwAPI_ResizeMem32  proc    near
        mov     bx,[ebp].IFRM._BX
        mov     ecx,[ebp].IFRM._ECX
        push    ecx
        Sys     GetSelDet32             ;Get selector base address.
        mov     esi,edx
        pop     ecx
        jc      api31_9
        call    mcbResizeMemLinear32    ;re-size the memory.
        jc      api31_9
        mov     edx,esi
        dec     ecx
        Sys     SetSelDet32
api31_9:
        jc      exit_with_carry
        ret
cwAPI_ResizeMem32  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release previously allocated block of memory. Is be used for memory allocated
;by either GetMem or GetMem32.
;
;
;On Entry:
;
;BX     - Selector for block to release.
;
cwAPI_RelMem    proc    near
        mov     bx,[ebp].IFRM._BX
        Sys     GetSelDet32             ;Get selector base address.
        jc      api32_9
        mov     esi,edx
        mov     ds,[ebp].IFRM._DS
        mov     es,[ebp].IFRM._ES
        mov     fs,[ebp].IFRM._FS
        mov     gs,[ebp].IFRM._GS
        @dprintf DOPT_MEM,<"cwAPI_RelMem: calling int 31h, RelSel, bx=%X",10>,ebx
        Sys     RelSel                  ;release the selector.
        mov     [ebp].IFRM._DS,ds
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._FS,fs
        mov     [ebp].IFRM._GS,gs
        call    mcbRelMemLinear32       ;release the memory.
api32_9:
        jc      exit_with_carry
        ret
cwAPI_RelMem    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release block of memory allocated via GetMemSO.
;
;On Entry:
;
;SI:DI  - Selector:offset for block to release.
;
cwAPI_RelMemSO  proc    near
;
;Zero any segment registers using this selector.
;
        mov     bx,[ebp].IFRM._SI
        and     bx,0ffffh-3
        jz      api33_bad               ; MED 11/18/96, screen out null pointer releases
        xor     ecx,ecx
        mov     eax,[ebp].IFRM._DS
        and     ax,0ffffh-3
        cmp     ax,bx
        jnz     api33_0
        mov     [ebp].IFRM._DS,ecx
        mov     ds,ecx
api33_0:
        mov     eax,[ebp].IFRM._ES
        and     ax,0ffffh-3
        cmp     ax,bx
        jnz     api33_1
        mov     [ebp].IFRM._ES,ecx
        mov     es,ecx
api33_1:
        mov     eax,[ebp].IFRM._FS
        and     ax,0ffffh-3
        cmp     ax,bx
        jnz     api33_2
        mov     [ebp].IFRM._FS,ecx
        mov     fs,ecx
api33_2:
        mov     eax,[ebp].IFRM._GS
        and     ax,0ffffh-3
        cmp     ax,bx
        jnz     api33_3
        mov     [ebp].IFRM._GS,ecx
        mov     gs,ecx
api33_3:
        mov     bx,[ebp].IFRM._SI
        Sys     GetSelDet32             ;get selectors details.
        jc      api33_9
        mov     esi,edx
        movzx   eax,[ebp].IFRM._DI
        add     esi,eax                 ;get memory blocks address.
        call    mcbRelMemLinear32       ;release the memory.
;
;Check if block had it's own selector.
;
        cmp     [ebp].IFRM._DI,0        ;offset of zero?
        jnz     api33_4
        mov     bx,[ebp].IFRM._SI
        @dprintf DOPT_MEM,<"cwAPI_RelMemSO: calling int 31h, RelSel, bx=%X",10>,ebx
        Sys     RelSel                  ;release the selector.
;
api33_4:
        clc
;
api33_9:
        jc      exit_with_carry
        ret

; MED 11/18/96
api33_bad:
        stc
        jmp     api33_9

cwAPI_RelMemSO  endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate a block of memory without a selector.
;
;On Entry:
;
;CX:DX  - Size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK &,
;
;SI:DI  - Linear address of block allocated.
;
cwAPI_GetMemLinear proc near
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16
        mov     cx,dx
        call    mcbGetMemLinear32
        jc      api34_9
        mov     di,si
        shr     esi,16
        mov     [ebp].IFRM._SI,si
        mov     [ebp].IFRM._DI,di
        clc
api34_9:
        jc      exit_with_carry
        ret
cwAPI_GetMemLinear endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate a block of memory without a selector.
;
;On Entry:
;
;ECX    - Size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK &,
;
;ESI    - Linear address of block allocated.
;
cwAPI_GetMemLinear32 proc near
        mov     ecx,[ebp].IFRM._ECX
        call    mcbGetMemLinear32
        jc      api35_9
        mov     [ebp].IFRM._ESI,esi
api35_9:
        jc      exit_with_carry
        ret
cwAPI_GetMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated block of memory without a selector.
;
;On Entry:
;
;SI:DI  - Linear address of block to re-size.
;CX:DX  - Size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK &,
;
;SI:DI  - New linear address of block.
;
cwAPI_ResizeMemLinear proc near
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        mov     cx,[ebp].IFRM._CX
        mov     dx,[ebp].IFRM._DX
        shl     ecx,16
        mov     cx,dx
        shl     esi,16
        mov     si,di
        call    mcbResizeMemLinear32
        jc      api36_9
        mov     di,si
        shr     esi,16
        mov     [ebp].IFRM._SI,si
        mov     [ebp].IFRM._DI,di
        clc
api36_9:
        jc      exit_with_carry
        ret
cwAPI_ResizeMemLinear endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated block of memory without a selector.
;
;On Entry:
;
;ESI    - Linear address of block to re-size.
;ECX    - Size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK &,
;
;ESI    - New linear address of block.
;
cwAPI_ResizeMemLinear32 proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ecx,[ebp].IFRM._ECX
        call    mcbResizeMemLinear32
        jc      api37_9
        mov     [ebp].IFRM._ESI,esi
api37_9:
        jc      exit_with_carry
        ret
cwAPI_ResizeMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release previously allocated block of memory (linear address).
;
;On Entry:
;
;SI:DI  - Linear address of block to release.
;
;On Exit:
;
cwAPI_RelMemLinear proc near
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        shl     esi,16
        mov     si,di
        call    mcbRelMemLinear32
        jc      exit_with_carry
        ret
cwAPI_RelMemLinear endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release previously allocated block of memory (linear address).
;
;On Entry:
;
;ESI    - Linear address of block to release.
;
;On Exit:
;
cwAPI_RelMemLinear32 proc near
        mov     esi,[ebp].IFRM._ESI
        call    mcbRelMemLinear32
        jc      exit_with_carry
        ret
cwAPI_RelMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate an application relative block of memory.
;
;On Entry:
;
;EBX    - Size of block required in bytes.
;
;On Exit:
;
;Carry clear if OK &,
;
;ESI    - Application relative linear address of block allocated.
;
cwAPI_GetMemNear proc near
        mov     ecx,[ebp].IFRM._EBX
        call    mcbGetMemLinear32
        jc      api40_9
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        sub     esi,ds:[EPSP_Struc.EPSP_NearBase]
        mov     [ebp].IFRM._ESI,esi
        clc
api40_9:
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_GetMemNear endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a previously allocated application relative block of memory.
;
;On Entry:
;
;EBX    - Size of block required in bytes.
;ESI    - application relative linear address of block to re-size.
;
;On Exit:
;
;Carry clear if OK &,
;
;ESI    - New application relative linear address of block.
;
cwAPI_ResizeMemNear proc near
        mov     ecx,[ebp].IFRM._EBX
        mov     esi,[ebp].IFRM._ESI
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        add     esi,ds:[EPSP_Struc.EPSP_NearBase]
        call    mcbResizeMemLinear32
        jc      api41_9
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        sub     esi,ds:[EPSP_Struc.EPSP_NearBase]
        mov     [ebp].IFRM._ESI,esi
        clc
api41_9:
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_ResizeMemNear endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release previously allocated application relative block of memory.
;
;On Entry:
;
;ESI    - Application relative linear address of block to release.
;
cwAPI_RelMemNear proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        add     esi,ds:[EPSP_Struc.EPSP_NearBase]
        call    mcbRelMemLinear32
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_RelMemNear endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Convert linear address to application relative address.
;
;On Entry:
;
;ESI    - Linear address to convert.
;
;On Exit:
;
;ESI    - Application relative linear address.
;
cwAPI_Linear2Near proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        sub     esi,ds:[EPSP_Struc.EPSP_NearBase]
        mov     [ebp].IFRM._ESI,esi
        ret
        assume ds:nothing
cwAPI_Linear2Near endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Convert application relative address to linear address.
;
;On Entry:
;
;ESI    - Application relative linear address.
;
;On Exit:
;
;ESI    - Linear address to convert.
;
cwAPI_Near2Linear proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        add     esi,ds:[EPSP_Struc.EPSP_NearBase]
        mov     [ebp].IFRM._ESI,esi
        ret
        assume ds:nothing
cwAPI_Near2Linear endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Lock a region of memory.
;
;On Entry:
;AX = 0xff1b
;BX:CX  - Starting linear address of memory to lock.
;SI:DI  - Size of region to lock in bytes.
;
;On Exit:
;
;Carry set on error, none of the memory locked, else memory is locked.
;
cwAPI_LockMem   proc    near
        mov     bx,[ebp].IFRM._BX
        mov     cx,[ebp].IFRM._CX
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        mov     ax,0600h
        cwAPI_CallOld
        jc      api45_9
;
        mov     dx,bx      ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si      ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    RegisterResource
        clc
;
api45_9:
        jc      exit_with_carry
        ret
cwAPI_LockMem   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Lock a region of memory.
;
;On Entry:
;AX = 0xff1c
;ESI    - Starting linear address of memory to lock.
;ECX    - Size of region to lock in bytes.
;
;On Exit:
;
;Carry set on error, none of the memory locked, else memory is locked.
;
cwAPI_LockMem32 proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ecx,[ebp].IFRM._ECX
        xchg    esi,ecx
        mov     ebx,ecx
        shr     ebx,16
        mov     di,si
        shr     esi,16
        mov     ax,0600h
        cwAPI_CallOld
        jc      api46_9
        mov     dx,bx        ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si        ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    RegisterResource
        clc
api46_9:
        jc      exit_with_carry
        ret
cwAPI_LockMem32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Un-lock a region of memory.
;
;On Entry:
;AX = 0xff1d
;BX:CX  - Starting linear address of memory to unlock
;SI:DI  - Size of region to unlock in bytes
;
cwAPI_UnLockMem proc near
        mov     bx,[ebp].IFRM._BX
        mov     cx,[ebp].IFRM._CX
        mov     si,[ebp].IFRM._SI
        mov     di,[ebp].IFRM._DI
        mov     ax,0601h
        cwAPI_CallOld
        jc      api47_9
        mov     dx,bx       ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si       ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    ReleaseResource
        clc
api47_9:
        jc      exit_with_carry
        ret
cwAPI_UnLockMem endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Un-lock a region of memory.
;
;On Entry:
;AX = 0xff1e
;ESI    - Starting linear address of memory to unlock
;ECX    - Size of region to unlock in bytes
;
;NOTES:
;
;This will allow the memory to be swapped to disk by the VMM if neccessary.
;Areas below and above the specified memory will also be un-locked if the
;specified region is not page aligned.
;
cwAPI_UnLockMem32 proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ecx,[ebp].IFRM._ECX
        xchg    esi,ecx
        mov     ebx,ecx
        shr     ebx,16
        mov     di,si
        shr     esi,16
        mov     ax,0601h
        cwAPI_CallOld
        jc      api48_9
        mov     dx,bx      ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si      ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    ReleaseResource
        clc
api48_9:
        jc      exit_with_carry
        ret
cwAPI_UnLockMem32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Lock a region of memory using application relative address.
;
;On Entry:
;AX = 0xff1f
;ESI    - Starting linear address of memory to lock.
;EBX    - Size of region to lock in bytes.
;
;On Exit:
;
;Carry set on error, none of the memory locked, else memory is locked.
;
cwAPI_LockMemNear proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ebx,[ebp].IFRM._EBX
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        add     esi,ds:[EPSP_Struc.EPSP_NearBase]
        xchg    esi,ebx
        mov     di,si
        shr     esi,16
        mov     cx,bx
        shr     ebx,16
        mov     ax,0600h
        cwAPI_CallOld
        jc      api49_9
        mov     dx,bx      ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si      ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    RegisterResource
        clc
api49_9:
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_LockMemNear endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Un-lock a region of memory using application relative address.
;
;On Entry:
;AX = 0xff20
;ESI    - Starting linear address of memory to unlock
;EBX    - Size of region to unlock in bytes
;
cwAPI_UnLockMemNear proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ecx,[ebp].IFRM._ECX
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        add     esi,ds:[EPSP_Struc.EPSP_NearBase]
        xchg    esi,ebx
        mov     di,si
        shr     esi,16
        mov     cx,bx
        shr     ebx,16
        mov     ax,0601h
        cwAPI_CallOld
        jc      api50_9
        mov     dx,bx      ;edx=key value
        shl     edx,16
        mov     dx,cx
        mov     cx,si      ;ecx=second value
        shl     ecx,16
        mov     cx,di
        mov     ax,Res_LOCK
        call    ReleaseResource
        clc
api50_9:
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_UnLockMemNear endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Allocate a region of DOS (conventional) memory.
;
;On Entry:
;AX = 0xff21
;BX     - Number of paragraphs (16 byte blocks) required.
;
;On Exit:
;
;If function was successful:
;Carry flag is clear.
;
;AX     - Initial real mode segment of allocated block
;DX     - Initial selector for allocated block
;
;If function was not successful:
;Carry flag is set.
;
;AX     - DOS error code.
;BX     - Size of largest available block in paragraphs.
;
cwAPI_GetMemDOS proc near
        mov     bx,[ebp].IFRM._BX
        mov     ax,0100h
        cwAPI_CallOldCC
        mov     [ebp].IFRM._AX,ax
        jnc     api51_0
        mov     [ebp].IFRM._BX,bx
        jmp     api51_9
;
api51_0:
        mov     [ebp].IFRM._DX,dx
        movzx   edx,dx
        movzx   ecx,bx
        shl     ecx,4
        mov     ax,Res_DOSMEM
        call    RegisterResource
;
api51_9:
        ret
cwAPI_GetMemDOS endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Re-size a block of DOS (conventional) memory previously allocated with
;GetMemDOS.
;
;On Entry:
;AX = 0xff22
;BX     - New block size in paragraphs
;DX     - Selector of block to modify
;
;On Exit:
;
;If function was successful:
;Carry flag is clear.
;
;If function was not successful:
;Carry flag is set.
;
;AX     - DOS error code:
;BX     - Maximum block size possible in paragraphs
;
cwAPI_ResizeMemDOS proc near
        mov     bx,[ebp].IFRM._BX
        mov     dx,[ebp].IFRM._DX
        mov     ax,0102h
        cwAPI_CallOld
        jnc     api52_0
        mov     [ebp].IFRM._AX,ax
        mov     [ebp].IFRM._BX,bx
        jmp     api52_9
;
api52_0:
        mov     ax,Res_DOSMEM
        movzx   edx,dx
        call    ReleaseResource
        movzx   ecx,bx
        shl     ecx,4
        call    RegisterResource
        clc
;
api52_9:
        jc      exit_with_carry
        ret
cwAPI_ResizeMemDOS endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Release a block of DOS (conventional) memory previously allocated with
;GetMemDOS.
;
;On Entry:
;AX = 0xff23
;DX     - Selector of block to free.
;
;On Exit:
;
;If function was successful:
;Carry flag is clear.
;
;If function was not successful:
;Carry flag is set.
;
;AX     - DOS error code.
;
cwAPI_RelMemDOS proc near

        mov     ds,[ebp].IFRM._DS
        mov     es,[ebp].IFRM._ES
        mov     fs,[ebp].IFRM._FS
        mov     gs,[ebp].IFRM._GS

        mov     dx,[ebp].IFRM._DX
        mov     ax,0101h
        cwAPI_CallOld
        jnc     api53_0
        mov     [ebp].IFRM._AX,ax
        jmp     api53_9
;
api53_0:
        mov     ax,Res_DOSMEM
        movzx   edx,dx
        call    ReleaseResource
        clc
;
api53_9:
        mov     [ebp].IFRM._DS,ds
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._FS,fs
        mov     [ebp].IFRM._GS,gs
        jc      exit_with_carry
        ret
cwAPI_RelMemDOS endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Run another CauseWay program directly.
;
;On Entry:
;AX = 0xff24
;DS:EDX - File name.
;ES:ESI - Command line. First byte is length, then real data.
;CX     - Environment selector, 0 to use existing copy.
;
;On Exit:
;
;Carry set on error and AX = error code else AL=ErrorLevel
;
cwAPI_Exec      proc    near
        @dprintf DOPT_EXEC,<"cwAPI_Exec enter",10>
        mov     ds,[ebp].IFRM._DS
        mov     edx,[ebp].IFRM._EDX
        mov     es,[ebp].IFRM._ES
        mov     esi,[ebp].IFRM._ESI
        mov     cx,[ebp].IFRM._CX
        mov     ebx,0
        push    ebp
        call    _Exec
        pop     ebp
        jnc     api54_0
        mov     [ebp].IFRM._AX,ax
        jmp     api54_9
;
api54_0:
        mov     [ebp].IFRM._AL,al
;
api54_9:
        jc      exit_with_carry
        @dprintf DOPT_EXEC,<"cwAPI_Exec exit",10>
        ret
cwAPI_Exec      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Validate and get expanded length of a CWC'd file.
;
;On Entry:
;AX = 0xff2b
;BX     - File handle.
;
;On Exit:
;
;Carry set if not a CWC'd file else,
;
;ECX    - Expanded data size.
;
cwAPI_cwcInfo   proc    near
        mov     bx,[ebp].IFRM._BX
        call    GetCWCInfo
        mov     [ebp].IFRM._ECX,ecx
        mov     [ebp].IFRM._EAX,eax
        jc      exit_with_carry
        ret
cwAPI_cwcInfo   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Load/Expand a CWC'd data file into memory.
;
;On Entry:
;AX = 0xfffb
;BX     - Source file handle.
;ES:EDI - Destination memory.
;
;On Exit:
;
;Carry set on error and EAX is error code else,
;
;ECX    - Expanded data length.
;
cwAPI_cwcLoad proc near
        mov     bx,[ebp].IFRM._BX
        mov     es,[ebp].IFRM._ES
        mov     edi,[ebp].IFRM._EDI
        call    DecodeCWC
        mov     [ebp].IFRM._EAX,eax
        jc      api56_9
        mov     [ebp].IFRM._ECX,ecx
api56_9:
        jc      exit_with_carry
        ret
cwAPI_cwcLoad endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;--- in: AX = 0xfffc
;---     ESI=address to check
;--- exit: C if address invalid (page fault)

cwAPI_LinearCheck proc near
        mov     esi,[ebp].IFRM._ESI
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        ;
        mov     es,RealSegment
        mov     eax,es
        lsl     eax,eax
        cmp     eax,esi
        jc      api57_3
        mov     LinearAddressCheck,1
        mov     al,es:[esi]
        cmp     LinearAddressCheck,0 ;has flag been reset?
        jnz     api57_2
api57_3:
        stc
        jmp     api57_1
api57_2:
        mov     LinearAddressCheck,0 ;flag still set, that is: address is valid.
        clc
api57_1:
        ;
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_LinearCheck endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;--- in: AX = 0xfffd
;--- ds:edx: filename
;--- es:esi: cmdline
;--- cx: environment
;--- ebx=1 -> load for debug
;--- _exec returns: cx:edx = cs:eip
;---                bx:eax = ss:esp
;---                si = PSP
;---                di = AutoDS
;---               ebp = segment definition memory

cwAPI_ExecDebug proc near
        @dprintf DOPT_EXEC,<"cwAPI_ExecDebug enter",10>
        mov     ds,[ebp].IFRM._DS
        mov     edx,[ebp].IFRM._EDX
        mov     es,[ebp].IFRM._ES
        mov     esi,[ebp].IFRM._ESI
        mov     cx,[ebp].IFRM._CX
        mov     ebx,1
        push    ebp
        call    _Exec
        jnc     api58_0
        pop     ebp
        mov     [ebp].IFRM._AX,ax
        jmp     exit_with_carry
;
api58_0:
        shl     esi,16
        mov     si,di
        mov     edi,ebp
        pop     ebp
        mov     [ebp].IFRM._EAX,eax
        mov     [ebp].IFRM._BX,bx
        mov     [ebp].IFRM._CX,cx
        mov     [ebp].IFRM._EDX,edx
        mov     [ebp].IFRM._DI,si
        shr     esi,16
        mov     [ebp].IFRM._SI,si
        mov     [ebp].IFRM._EBP,edi
        clc
;
api58_9:
        @dprintf DOPT_EXEC,<"cwAPI_ExecDebug exit",10>
        ret
cwAPI_ExecDebug endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Load another CauseWay program as an overlay, ie, do relocations etc but don't
;actually execute it.
;
;On Entry:
;AX = 0xff2a
;DS:EDX - File name.
;
;On Exit:
;
;Carry set on error and AX = error code else,
;
;CX:EDX - Entry CS:EIP
;BX:EAX - Entry SS:ESP
;SI     - PSP.
;
cwAPI_cwLoad    proc near
        mov     ds,[ebp].IFRM._DS
        mov     edx,[ebp].IFRM._EDX
        mov     ebx,2
        xor     ax,ax
        mov     es,eax
        mov     fs,eax
        push    ebp
        call    _Exec
        pop     ebp
        jnc     api59_0
        mov     [ebp].IFRM._AX,ax
        jmp     exit_with_carry
;
api59_0:
        mov     [ebp].IFRM._CX,cx
        mov     [ebp].IFRM._EDX,edx
        mov     [ebp].IFRM._BX,bx
        mov     [ebp].IFRM._EAX,eax
        mov     [ebp].IFRM._SI,si
;
        @dprintf DOPT_EXEC,<"Done with cwAPI_cwLoad",10>
        ret
cwAPI_cwLoad    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cwAPI_Cleanup   proc    near
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        call    LoseFileHandles
        clc
        ret
        assume ds:nothing
cwAPI_Cleanup   endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Use DPMI services to allocate a single selector and initialise base & limit
;as specified with type=data/read/write.
;
;On Entry:-
;
;EAX    - Linear base.
;EBX    - limit.
;
;On Exit:-
;
;BX     - Segment selector.
;
_SetSelector    proc near
        ;
        push    eax
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        cmp     ebx,100000h
        jc      api61_ok
        cmp     ebx,-1
        jz      api61_ok
        add     ebx,4095
;        and     ebx,0FFFFFFFFh-4095
        and     bx,0F000h
        dec     ebx
        ;
api61_ok:
        mov     [api61_SelectorBase],eax
        mov     [api61_SelectorSize],ebx
        ;
        ;Get a new selector from DPMI.
        ;
        mov     ax,0000h
        mov     cx,1
        cwAPI_CallOld
        jc      api61_9
        mov     ebx,eax
;api61_GotSel:
        push    ebx
        mov     ecx,[api61_SelectorSize]
        mov     esi,[api61_SelectorBase]
        mov     eax,ds                       ;need RPL to base DPL on.
        lar     ax,ax
        and     ah,DescPL3
        or      ah,DescPresent+DescMemory+DescRWData
        ;
        mov     dx,apiSystemFlags           ;use default setting.
        shr     dx,14
        mov     al,dl
        xor     al,1
        or      al,b[apiSystemFlags+2]
        and     al,1
        shl     al,6
        ;
        cmp     ecx,0fffffh                 ; see if we need to set g bit
        jc      api61_3
        jz      api61_3
        shr     ecx,12                      ; div by 4096
        or      al,80h                      ; set g bit
api61_3:
        mov     [DescriptorBuffer.Limit],cx ;store low word of limit.
        shr     ecx,16
        or      cl,al
        mov     [DescriptorBuffer.Gran],cl  ;store high bits of limit and gran/code size bits.
        mov     [DescriptorBuffer.Base_l],si
        shr     esi,16
        mov     ebx,esi
        mov     [DescriptorBuffer.Base_m],bl
        mov     [DescriptorBuffer.Base_H],bh
        mov     [DescriptorBuffer.Access],ah
        pop     ebx
        ;
        ;Write new descriptor table entry.
        ;
        push    ebx
        mov     ax,000ch
        mov     edi,offset DescriptorBuffer
        push    ds
        pop     es
        cwAPI_CallOld
        pop     ebx
        jc      api61_9
        ;
        movzx   edx,bx
        mov     ax,Res_SEL
        call    RegisterResource
        ;
api61_8:
        clc
        jmp     api61_10
        ;
api61_9:
        stc
api61_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        ret

_SetSelector    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_RelSelector    proc    near
;
;Release a segment selector table entry.
;
;On Entry:-
;
;BX     - Segment selector.
;
        push    eax
        push    ebx
        push    ecx
        push    edx
        ;
        movzx   edx,bx
        mov     ax,Res_SEL
        @dprintf DOPT_SEL,<"_RelSelector: calling ReleaseResource(ax=Res_SEL, edx=%X)",10>,edx
        call    ReleaseResource
        ;
        @dprintf DOPT_SEL,<"_RelSelector: ReleaseResource() returned, calling int 31h, ax=1, bx=%X",10>,ebx
        mov     ax,0001h
        cwAPI_CallOld
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
_RelSelector    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_SizeSelector   proc near
;
;Set a segment selector table entry and return a suitable segment value. Will
;use whatever method is apropriate to the system environment.
;
;On Entry:-
;
;EAX    - Linear base address.
;EBX    - size in bytes.
;CX     - Selector.
;
;On Exit:-
;
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        mov     [api63_SelectorBase],eax
        mov     [api63_SelectorSize],ebx
        mov     bx,cx
        ;
        push    bx
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        mov     ax,000bh
        cwAPI_CallOld
        pop     bx
        ;
        push    bx
        mov     al,0
        mov     ecx,[api63_SelectorSize]
        mov     esi,[api63_SelectorBase]
        cmp     ecx,0fffffh                     ; see if we need to set g bit
        jc      api63_2
        jz      api63_2
        shr     ecx,12                          ; div by 4096
        or      al,80h                          ; set g bit
api63_2:
        mov     [DescriptorBuffer.Limit],cx     ;store low word of limit.
        shr     ecx,16
        or      cl,al
        and     [DescriptorBuffer.Gran],01110000b ;lose limit & G bit.
        or      [DescriptorBuffer.Gran],cl      ;store high bits of limit and G bit.
        mov     [DescriptorBuffer.Base_l],si    ;store low word of linear base.
        shr     esi,16
        mov     bx,si
        mov     [DescriptorBuffer.Base_m],bl    ;store mid byte of linear base.
        mov     [DescriptorBuffer.Base_H],bh    ;store high byte of linear base.
        pop     bx
        ;
        ;Write new descriptor table entry.
        ;
        push    bx
        mov     ax,000ch
        mov     edi,offset DescriptorBuffer
        push    ds
        pop     es
        cwAPI_CallOld
        pop     bx
        jc      api63_9
        ;
        clc
        jmp     api63_10
        ;
api63_9:
        stc
api63_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret

_SizeSelector   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_DetSelector    proc    near
;
;Fetch selector base & limit values.
;
;On Entry:-
;
;BX     - Selector.
;
;On Exit:-
;
;EAX    - Linear base address.
;EBX    - size in bytes.
;
        ;
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        push    bx
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        mov     ax,000bh
        cwAPI_CallOld
        pop     bx
        jc      api64_9
        ;
        mov     ah,DescriptorBuffer.Base_H
        mov     al,DescriptorBuffer.Base_m
        shl     eax,16
        mov     ax,DescriptorBuffer.Base_l
        ;
        xor     ebx,ebx
        mov     bl,DescriptorBuffer.Gran;limit 19-16 in bits 0-3
        and     bl,0fh
        shl     ebx,16
        mov     bx,DescriptorBuffer.Limit
        test    DescriptorBuffer.Gran,80h ;page granularity?
        jz      api64_0
        shl     ebx,12
        or      bx,0FFFh
api64_0:
        clc
        jmp     api64_10
api64_9:
        stc
api64_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        ret
_DetSelector    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_CodeSelector   proc near
        ;
        pushad
        call    _DSizeSelector
        popad
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        push    cx
        push    bx
        mov     ax,000bh
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        cwAPI_CallOld
        pop     bx
        pop     cx
        ;
        mov     ax,cs
        and     al,11b
        shl     al,5
        or      al,DescERCode
        and     DescriptorBuffer.Access,10010000b
        or      DescriptorBuffer.Access,al   ;
        and     DescriptorBuffer.Gran,255-(1 shl 6)    ;clear code size bit.
        and     cl,1
        shl     cl,6
        or      DescriptorBuffer.Gran,cl               ;code size bit.
        ;
        mov     ax,000ch
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        cwAPI_CallOld
        clc
        jmp     api65_10
        ;
api65_9:
        stc
api65_10:
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
api65_11:
        ret
_CodeSelector   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_DSizeSelector  proc near
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        push    cx
        push    bx
        mov     ax,000bh
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        cwAPI_CallOld
        pop     bx
        pop     cx
        ;
        and     DescriptorBuffer.Gran,255-(1 shl 6)    ;clear code size bit.
        and     cl,1
        shl     cl,6
        or      DescriptorBuffer.Gran,cl               ;code size bit.
        ;
        mov     ax,000ch
        push    ds
        pop     es
        mov     edi,offset DescriptorBuffer
        cwAPI_CallOld
        ;
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
_DSizeSelector  endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Get current MCB memory allocation block size.
;
;On Exit:
;
;ECX    - Current threshold.
;
cwAPI_GetMCBSize proc near
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        mov     ecx,ds:[EPSP_Struc.EPSP_mcbMaxAlloc]
        add     ecx,MCBCHUNKLEN+MCBLEN
        mov     [ebp].IFRM._ECX,ecx
        ret
        assume ds:nothing
cwAPI_GetMCBSize endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set new MCB memory allocation block size.
;
;On Entry:
;
;ECX    - New value to set.
;
;On Exit:
;
;Carry set on error else new value will be used.
;
cwAPI_SetMCBSize proc near
        mov     ecx,[ebp].IFRM._ECX
        add     ecx,4095
;        and     ecx,not 4095
        and     cx,0f000h
        cmp     ecx,65536+1
        jc      api68_1
        stc
        jmp     api68_9
        ;
api68_1:
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        cmp     ecx,MCBCHUNKLEN+MCBLEN
        jc      api68_9
        sub     ecx,MCBCHUNKLEN+MCBLEN
api68_0:
        mov     ds:[EPSP_Struc.EPSP_mcbMaxAlloc],ecx
        clc
;
api68_9:
        jc      exit_with_carry
        ret
        assume ds:nothing
cwAPI_SetMCBSize endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Replacement for CauseWay API GetMemLinear32 function.
;in:  ecx=size in bytes
;out: esi=linear address
;
mcbGetMemLinear32 proc near
        push    ds
        push    es
        push    fs
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    edi
        push    ebp
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     mcbLastChunk,0
        pop     ds
        assume ds:GROUP16
;
;Round size up to next dword to keep things aligned.
;
        cmp     ecx,-1
        jz      api69_GetMax
        cmp     ecx,-2
        jz      api69_GetMax
        add     ecx,3
        and     ecx,not 3
;
;Check MCB allocation system is enabled.
;
        cmp     mcbAllocations,0
        jz      api69_GetMax
;
;Check block size is small enough for these functions.
;
        cmp     ecx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        jc      mGML32_0
;
;Size is above threshold so use normal API service for this request.
;
api69_GetMax:
        mov     ebx,ecx
        call    _GetMemory
        jnc     mGML32_9
        cmp     ecx,-2
        jz      api69_RetMax
        cmp     ecx,-1
        stc
        jnz     mGML32_9
api69_RetMax:
        pop     ebp
        pop     edi
        pop     edx
        pop     ecx
        mov     ecx,ebx
        push    ecx
        push    edx
        push    edi
        push    ebp
        stc
        jmp     mGML32_9
;
;Size is below threshold so use local allocation scheme.
;
mGML32_0:
;
;Check if initial block has been claimed yet, allocate and initialise one if not.
;
        cmp     DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead],0
        jnz     mGML32_10
        mov     ebx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        add     ebx,MCBCHUNKLEN+MCBLEN              ;chunk size.
        call    _GetMemory                          ;allocate it.
        jc      mGML32_9                            ;oops, appear to be out of memory.
        mov     DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead],esi
;
;Initialise this chunk.
;
        mov     es,RealSegment
        sub     ebx,MCBCHUNKLEN+MCBLEN              ;chunk size.
        mov     es:[esi].McbChunkStruc.mcbChunkSize,bx
        mov     es:[esi].McbChunkStruc.mcbChunkNext,0   ;set forward link address.
        mov     es:[esi].McbChunkStruc.mcbChunkLast,0   ;store back link address.
        mov     eax,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        mov     es:[esi].McbChunkStruc.mcbBiggest,ax     ;set biggest chunk size.
        Sys     GetSel
        jc      mGML32_9
        mov     es:[esi].McbChunkStruc.mcbChunkSel,bx
        push    edx
        push    ecx
        mov     edx,esi
        mov     ecx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        add     ecx,MCBCHUNKLEN+MCBLEN
        Sys     SetSelDet32
        pop     ecx
        pop     edx
        add     esi,MCBCHUNKLEN                     ;skip chunk link info.
        mov     es:[esi].McbStruc.mcbID,"C"         ;set ID byte.
        mov     es:[esi].McbStruc.mcbLast,"D"       ;mark it as last block in back link.
        mov     es:[esi].McbStruc.mcbLastSize,0     ;clear back link entry.
        mov     es:[esi].McbStruc.mcbFreeUsed,"J"   ;mark it as a free block,
        mov     es:[esi].McbStruc.mcbNext,"D"       ;last block in MCB chain,
        mov     eax,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        mov     es:[esi].McbStruc.mcbNextSize,ax
;
;Scan through all mcb's in all chunks looking for a big enough block.
;
mGML32_10:
        mov     es,RealSegment                      ;0-4G selector.
        mov     esi,DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead]  ;start of local allocation trail.
mGML32_1:
        mov     edi,esi                             ;keep a copy for chunk chaining.
        mov     ebp,edi                             ;keep a copy for mcbBiggest
        ;
        cmp     es:[edi].McbChunkStruc.mcbBiggest,cx     ;check if this chunk has a big
        jc      mGML32_6_0                          ;enough free block.
        add     esi,MCBCHUNKLEN
;
;Find first free and big enough block.
;
mGML32_2:
        cmp     es:[esi].McbStruc.mcbFreeUsed,"J"   ;Free block?
        jz      mGML32_5
mGML32_6:
        cmp     es:[esi].McbStruc.mcbNext,"M"       ;Normal block (not end of chain)?
        jz      mGML32_4
;
;Reached the end of the chain for this chunk so we need to move onto the next
;chunk in the chain.
;
mGML32_6_0:
        cmp     es:[edi].McbChunkStruc.mcbChunkNext,0   ;already have a link?
        jnz     mGML32_3
;
;Need another chunk to put in the chain so try and allocate it via normal
;CauseWay API.
;
        mov     ebx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        add     ebx,MCBCHUNKLEN+MCBLEN              ;chunk size.
        call    _GetMemory
        jc      mGML32_9                            ;oops, appear to be out of memory.
;
;Update current chunk with address of new chunk and initialise new chunk.
;
        sub     ebx,MCBCHUNKLEN+MCBLEN              ;chunk size.
        mov     es:[esi].McbChunkStruc.mcbChunkSize,bx
        mov     es:[edi].McbChunkStruc.mcbChunkNext,esi ;store forward link address.
        mov     es:[esi].McbChunkStruc.mcbChunkLast,edi ;store back link address.
        mov     es:[esi].McbChunkStruc.mcbChunkNext,0   ;clear new forward link address.
        mov     eax,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        mov     es:[esi].McbChunkStruc.mcbBiggest,ax    ;set biggest chunk size.
        Sys     GetSel
        jc      mGML32_9
        mov     es:[esi].McbChunkStruc.mcbChunkSel,bx
        push    edx
        push    ecx
        mov     edx,esi
        mov     ecx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        add     ecx,MCBCHUNKLEN+MCBLEN
        Sys     SetSelDet32
        pop     ecx
        pop     edx
        add     esi,MCBCHUNKLEN                     ;skip chunk link info.
        mov     es:[esi].McbStruc.mcbID,"C"         ;set ID.
        mov     es:[esi].McbStruc.mcbLast,"D"       ;mark it as last block in back link.
        mov     es:[esi].McbStruc.mcbLastSize,0     ;clear back link entry.
        mov     es:[esi].McbStruc.mcbFreeUsed,"J"   ;mark it as a free block,
        mov     es:[esi].McbStruc.mcbNext,"D"       ;last block in MCB chain,
        mov     eax,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        mov     es:[esi].McbStruc.mcbNextSize,ax
;
;Chain to next chunk.
;
mGML32_3:
        mov     esi,es:[edi].McbChunkStruc.mcbChunkNext ;pickup forward link address.
        jmp     mGML32_1                            ;scan this MCB chain.
;
;Move to next MCB.
;
mGML32_4:
        movzx   eax,es:[esi].McbStruc.mcbNextSize   ;get block length.
        add     eax,MCBLEN                          ;include size of an MCB.
        add     esi,eax
        jmp     mGML32_2
;
;Check if this block is big enough.
;
mGML32_5:
        cmp     es:[esi].McbStruc.mcbNextSize,cx    ;Big enough block?
        jc      mGML32_6
;
;Found a big enough free block so make use of it.
;
        mov     es:[esi].McbStruc.mcbFreeUsed,"W"   ;mark it as used.
        movzx   ebx,es:[esi].McbStruc.mcbNextSize
        sub     ebx,ecx                             ;get spare size.
        cmp     ebx,MCBLEN+1                        ;smaller than an MCB?
        jc      mGML32_8
;
;Create a new MCB from whats left over.
;
        sub     bx,MCBLEN                           ;MCB comes out of this size.
        mov     es:[esi].McbStruc.mcbNextSize,cx    ;set allocated block's size.
        mov     al,es:[esi].McbStruc.mcbNext        ;get next status.
        mov     es:[esi].McbStruc.mcbNext,"M"       ;make sure its not end of chain now.
        mov     edi,esi
        add     edi,ecx
        add     edi,MCBLEN                          ;move to where new MCB will be.
        mov     es:[edi].McbStruc.mcbID,"C"
        mov     es:[edi].McbStruc.mcbLast,"M"       ;not last in back link chain.
        mov     es:[edi].McbStruc.mcbLastSize,cx    ;set back link size.
        mov     es:[edi].McbStruc.mcbFreeUsed,"J"   ;mark as free,
        mov     es:[edi].McbStruc.mcbNext,al        ;set end of chain status.
        mov     es:[edi].McbStruc.mcbNextSize,bx    ;and new block size.
;
;Check if old block used to be end of chain.
;
        cmp     al,"D"                  ;end of chain?
        jz      mGML32_8
;
;Update back link size of next block.
;
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        add     eax,MCBLEN              ;include MCB size.
        add     edi,eax
        mov     es:[edi].McbStruc.mcbLastSize,ax
        sub     es:[edi].McbStruc.mcbLastSize,MCBLEN
;
;Time to exit.
;
mGML32_8:
        add     esi,MCBLEN              ;skip the MCB.
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     mcbLastChunk,ebp
        pop     ds
        assume ds:GROUP16
        call    mcbSetBiggest
        clc
;
;Restore stacked registers.
;
mGML32_9:
        pop     ebp
        pop     edi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        pop     fs
        pop     es
        pop     ds
        ret
        assume ds:nothing
mcbGetMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Replacement for CauseWay API ResMemLinear32 function.
;
mcbResizeMemLinear32 proc near
        push    ds
        push    es
        push    fs
        push    eax
        push    ebx
        push    ecx
        push    edi
        push    ebp
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     mcbLastChunk,0
        pop     ds
        assume ds:GROUP16
        ;
        mov     edi,esi
        add     ecx,3
        and     ecx,not 3
;
;See if MCB allocations are enabled.
;
        cmp     mcbAllocations,0
        jz      mRsML32_30
;
;See if EDI is within the range of any of the chunks in the list.
;
        mov     es,RealSegment
        mov     esi,DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead]
        or      esi,esi                 ;check mcb's are active.
        jz      mRsML32_8
        ;
mRsML32_0:
        cmp     edi,esi
        jc      mRsML32_1
        movzx   eax,es:[esi].McbChunkStruc.mcbChunkSize
        add     eax,esi
        add     eax,MCBCHUNKLEN+MCBLEN  ;chunk size.
        cmp     edi,eax
        jc      mRsML32_2
mRsML32_1:
        cmp     es:[esi].McbChunkStruc.mcbChunkNext,0   ;Next link field set?
        jz      mRsML32_8
        mov     esi,es:[esi].McbChunkStruc.mcbChunkNext
        jmp     mRsML32_0
;
;In range of a chunk so deal with it here.
;
mRsML32_2:
        xchg    edi,esi
        mov     ebp,edi
;
;Check block size is small enough for these functions.
;
        cmp     ecx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        jc      mRsML32_3
;
;Can't deal with a block this big so convert it to a normal API block.
;
        mov     edi,esi
        mov     ebx,ecx
        call    _GetMemory              ;try and allocate a normal block.
        jc      mRsML32_9
;
;Copy existing block to new block.
;
        push    ecx
        push    esi
        push    edi
        xchg    esi,edi
        sub     esi,MCBLEN
        movzx   ecx,es:[esi].McbStruc.mcbNextSize
        add     esi,MCBLEN
        push    ds
        mov     ds,RealSegment
        ;
        push    ecx
        shr     ecx,2
        rep     movsd
        pop     ecx
        and     ecx,3
        rep     movsb
        pop     ds
        pop     esi
;
;Release original block and return address of new block.
;
        call    mcbRelMemLinear32
        pop     esi
        pop     ecx
        jmp     mRsML32_9
;
;Get block's current size.
;
mRsML32_3:
        sub     esi,MCBLEN              ;move back to MCB.
        cmp     cx,es:[esi].McbStruc.mcbNextSize
        jz      mRsML32_7
        jnc     mRsML32_4               ;extending block.
;
;Block is shrinking so build another MCB at the end of this one.
;
        movzx   ebx,es:[esi].McbStruc.mcbNextSize
        sub     ebx,ecx                 ;get size differance.
        cmp     ebx,MCBLEN+1            ;enough for a new MCB?
        jnc     api70_0
;
;Before we abandon this block size change as too small we should check if the
;next block is free and join the new space onto that if it is.
;
        cmp     es:[esi].McbStruc.mcbNext,"M"   ;end of the chain?
        jnz     mRsML32_7                       ;yep, can't be another block.
        mov     edi,esi
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     edi,eax                         ;point to the next block.
        cmp     es:[edi].McbStruc.mcbFreeUsed,"J" ;Free block?
        jnz     mRsML32_7                       ;no, so leave things alone.
        mov     es:[esi].McbStruc.mcbNextSize,cx;set new size.
        push    esi
        push    ecx
        mov     eax,ecx
        add     eax,MCBLEN
        add     esi,eax
        xchg    esi,edi
        mov     ecx,MCBLEN
        push    ds
        push    es
        pop     ds
        rep     movsb
        pop     ds
        sub     edi,MCBLEN
        add     es:[edi].McbStruc.mcbNextSize,bx;update block size.
        sub     es:[edi].McbStruc.mcbLastSize,bx;update last size.
        pop     ecx
        pop     esi
        cmp     es:[edi].McbStruc.mcbNext,"M"   ;end of chain?
        jnz     mRsML32_7
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        add     edi,eax
        add     edi,MCBLEN
        mov     es:[edi].McbStruc.mcbLastSize,ax
        jmp     mRsML32_7                       ;leave things as they are.
        ;
api70_0:
        sub     ebx,MCBLEN                      ;need space for a new MCB.
        mov     es:[esi].McbStruc.mcbNextSize,cx;set new size.
        mov     al,es:[esi].McbStruc.mcbNext    ;get next status.
        mov     es:[esi].McbStruc.mcbNext,"M"   ;force not end of chain.
        mov     edi,esi
        add     edi,ecx
        add     edi,MCBLEN                      ;move to new MCB.
        mov     es:[edi].McbStruc.mcbID,"C"     ;set ID.
        mov     es:[edi].McbStruc.mcbLast,"M"   ;not last in last chain.
        mov     es:[edi].McbStruc.mcbLastSize,cx;set last link size.
        mov     es:[edi].McbStruc.mcbNext,al    ;set next status.
        mov     es:[edi].McbStruc.mcbFreeUsed,"J";mark it as free.
        mov     es:[edi].McbStruc.mcbNextSize,bx;set next link size.
        cmp     al,"D"                          ;end of chain?
        jz      mRsML32_7
;
;Update last link size of next MCB.
;
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        mov     ebx,edi
        add     ebx,eax
        add     ebx,MCBLEN                      ;move to next block.
        mov     es:[ebx].McbStruc.mcbLastSize,ax
;
;Check if next block is free and join it to the newly created block if it is,
;
        cmp     es:[ebx].McbStruc.mcbFreeUsed,"J"   ;Free block?
        jnz     mRsML32_7
;
;Join next block to this one.
;
        movzx   eax,es:[ebx].McbStruc.mcbNextSize   ;get block's size.
        add     eax,MCBLEN                          ;include size of an mcb.
        add     es:[edi].McbStruc.mcbNextSize,ax
        mov     al,es:[ebx].McbStruc.mcbNext
        mov     es:[edi].McbStruc.mcbNext,al        ;copy next status.
        cmp     al,"D"                              ;end of chain?
        jz      mRsML32_7
;
;Update next blocks last link size.
;
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        add     edi,eax
        add     edi,MCBLEN                          ;move to next block.
        mov     es:[edi].McbStruc.mcbLastSize,ax
        jmp     mRsML32_7
;
;Need to expand the block so check out the block above this one.
;
mRsML32_4:
        cmp     es:[esi].McbStruc.mcbNext,"D"       ;end of the chain?
        jz      mRsML32_6
        mov     edi,esi
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN                          ;move to next block.
        add     edi,eax
        cmp     es:[edi].McbStruc.mcbFreeUsed,"J"   ;This block free?
        jnz     mRsML32_6
        mov     ebx,ecx
        sub     bx,es:[esi].McbStruc.mcbNextSize    ;Get size needed.
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        cmp     eax,ebx                             ;Big enough block?
        jc      mRsML32_6
;
;Next block is big enough, is it big enough to leave a free block behind still?
;
        sub     eax,ebx                             ;Get size differance.
        cmp     eax,MCBLEN+1
        jnc     mRsML32_5
;
;Swollow new block whole and update next blocks last link entry.
;
        movzx   eax,es:[edi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     es:[esi].McbStruc.mcbNextSize,ax    ;update block size.
        mov     al,es:[edi].McbStruc.mcbNext        ;get next status.
        mov     es:[esi].McbStruc.mcbNext,al
        cmp     al,"D"                              ;end of the chain?
        jz      mRsML32_7
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        mov     edi,esi
        add     edi,eax
        add     edi,MCBLEN
        mov     es:[edi].McbStruc.mcbLastSize,ax
        jmp     mRsML32_7
;
;Create a new MCB in the right place.
;
mRsML32_5:
        mov     al,es:[edi].McbStruc.mcbNext        ;Get next status.
        movzx   ebx,es:[edi].McbStruc.mcbNextSize   ;Get size of this block.
        add     bx,es:[esi].McbStruc.mcbNextSize
        mov     es:[esi].McbStruc.mcbNextSize,cx    ;set new size of this block.
        sub     ebx,ecx                             ;get size remaining.
        mov     edi,esi
        add     edi,ecx
        add     edi,MCBLEN
        mov     es:[edi].McbStruc.mcbID,"C"         ;set ID.
        mov     es:[edi].McbStruc.mcbLast,"M"       ;not last in last chain.
        mov     es:[edi].McbStruc.mcbLastSize,cx    ;last link size.
        mov     es:[edi].McbStruc.mcbFreeUsed,"J"   ;mark it as free again.
        mov     es:[edi].McbStruc.mcbNext,al        ;set next status.
        mov     es:[edi].McbStruc.mcbNextSize,bx    ;set next link size.
        cmp     al,"D"                              ;end of the chain?
        jz      mRsML32_7
        add     edi,ebx
        add     edi,MCBLEN                          ;move to next block.
        mov     es:[edi].McbStruc.mcbLastSize,bx
        jmp     mRsML32_7
;
;Have to try and allocate another block and copy the current blocks contents.
;
mRsML32_6:
        add     esi,MCBLEN
        mov     edi,esi
        call    mcbGetMemLinear32
        jc      mRsML32_9
        push    ecx
        push    esi
        push    edi
        xchg    esi,edi
        sub     esi,MCBLEN
        movzx   ecx,es:[esi].McbStruc.mcbNextSize
        add     esi,MCBLEN
        push    ds
        mov     ds,RealSegment
        push    ecx
        shr     ecx,2
        rep     movsd
        pop     ecx
        and     ecx,3
        rep     movsb
        pop     ds
        pop     esi
        call    mcbRelMemLinear32                   ;release original block.
        pop     esi
        pop     ecx
        clc
        jmp     mRsML32_9
;
;Setup block address to return and exit.
;
mRsML32_7:
        add     esi,MCBLEN
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     mcbLastChunk,ebp
        assume ds:GROUP16
        pop     ds
        call    mcbSetBiggest
        clc
        jmp     mRsML32_9
;
;Not in range of any local chunks so pass it to CauseWay API function.
;
mRsML32_8:
        cmp     ecx,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        jc      mRsML32_11
mRsML32_30:
        mov     esi,edi
        mov     ebx,ecx
        call    _ResizeMemory
        jmp     mRsML32_9
;
;Convert normal API block to local MCB block. This assumes that a none MCB block
;comeing through here is bigger than mcbMaxAlloc and therefore is shrinking.
;
mRsML32_11:
        call    mcbGetMemLinear32
        jc      mRsML32_9
;
;Copy original block contents.
;
        push    esi
        push    edi
        xchg    esi,edi
        push    ds
        mov     ds,RealSegment
        push    ecx
        shr     ecx,2
        rep     movsd
        pop     ecx
        and     ecx,3
        rep     movsb
        pop     ds
        pop     esi
        call    _RelMemory                  ;release original block.
        pop     esi
;
;Return to caller.
;
mRsML32_9:
        pop     ebp
        pop     edi
        pop     ecx
        pop     ebx
        pop     eax
        pop     fs
        pop     es
        pop     ds
        ret
        assume ds:nothing
mcbResizeMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Replacement for CauseWay API RelMemLinear32 function.
;
mcbRelMemLinear32 proc near
        push    ds
        push    es
        push    fs
        pushad
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        mov     edi,esi
;
;See if MCB blocks are enabled.
;
        cmp     mcbAllocations,0
        jz      mRML32_8
;
;See if EDI is within the range of any of the chunks in the list.
;
        mov     es,RealSegment
        mov     esi,DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead]
        or      esi,esi                             ;check mcb's are active.
        jz      mRML32_8
        ;
mRML32_0:
        cmp     edi,esi
        jc      mRML32_1
        movzx   eax,es:[esi].McbChunkStruc.mcbChunkSize
        add     eax,esi
        add     eax,MCBCHUNKLEN+MCBLEN              ;chunk size.
        cmp     edi,eax
        jc      mRML32_2
mRML32_1:
        cmp     es:[esi].McbChunkStruc.mcbChunkNext,0   ;Next link field set?
        jz      mRML32_8
        mov     esi,es:[esi].McbChunkStruc.mcbChunkNext
        jmp     mRML32_0
;
;In range of a chunk so deal with it here.
;
mRML32_2:
        xchg    edi,esi
        mov     ebp,edi
        sub     esi,MCBLEN
        mov     es:[esi].McbStruc.mcbFreeUsed,"J"   ;mark it as free.
;
;Check if next block is free and join it to this one if it is.
;
mRML32_11:
        cmp     es:[esi].McbStruc.mcbNext,"D"       ;last block in chain?
        jz      mRML32_3
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        mov     ebx,esi
        add     eax,MCBLEN
        add     ebx,eax
        cmp     es:[ebx].McbStruc.mcbFreeUsed,"J"   ;free block?
        jnz     mRML32_3
        movzx   eax,es:[ebx].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     es:[esi].McbStruc.mcbNextSize,ax    ;update block size.
        mov     al,es:[ebx].McbStruc.mcbNext
        mov     es:[esi].McbStruc.mcbNext,al        ;copy next status.
        cmp     al,"D"                              ;last block in chain?
        jz      mRML32_3
;
;Update next blocks last block pointer.
;
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     eax,MCBLEN
        add     esi,eax
        mov     es:[esi].McbStruc.mcbLastSize,ax
        sub     es:[esi].McbStruc.mcbLastSize,MCBLEN
        sub     esi,eax
;
;Check if current block is end of next chain and end of last chain. If it
;is we can release this chunk because it isn't being used anymore.
;
mRML32_3:
        cmp     es:[esi].McbStruc.mcbNext,"M"
        jz      mRML32_4
        cmp     es:[esi].McbStruc.mcbLast,"M"
        jz      mRML32_4
;
;Un-link and release this chunk, its not being used.
;
        mov     esi,edi                             ;Get chunk address.
        mov     edi,es:[esi].McbChunkStruc.mcbChunkLast
        mov     eax,es:[esi].McbChunkStruc.mcbChunkNext
        or      edi,edi                             ;First chunk?
        jnz     mRML32_5
        mov     DWORD PTR fs:[EPSP_Struc.EPSP_mcbHead],eax  ;Set new head chunk, 0 is valid.
        jmp     mRML32_6
mRML32_5:
        mov     es:[edi].McbChunkStruc.mcbChunkNext,eax           ;link to next chunk.
mRML32_6:
        xchg    eax,edi
        or      edi,edi                             ;is there a next chunk?
        jz      mRML32_7
        mov     es:[edi].McbChunkStruc.mcbChunkLast,eax           ;link to last chunk.
mRML32_7:
        mov     bx,es:[esi].McbChunkStruc.mcbChunkSel
        @dprintf DOPT_MEM,<"mcb_RelMem32Linear32: calling int 31h, RelSel, bx=%X",10>,ebx
        Sys     RelSel                              ;release this selector.
        call    _RelMemory                          ;release this memory for real.
        jmp     mRML32_9                            ;exit, we're all done.
;
;Check if previous block is free and join this one to it if it is.
;
mRML32_4:
        cmp     es:[esi].McbStruc.mcbLast,"M"       ;last block in last chain?
        jnz     mRML32_10
;
;Move back to previous block, see if its free and let the next block join code
;deal with it if it is.
;
        movzx   eax,es:[esi].McbStruc.mcbLastSize
        add     eax,MCBLEN
        sub     esi,eax
        cmp     es:[esi].McbStruc.mcbFreeUsed,"J"
        jz      mRML32_11
;
;Finished but we couldn't release this chunk.
;
mRML32_10:
        call    mcbSetBiggest
        clc
        jmp     mRML32_9
;
;Not in range of any local chunks so pass it to CauseWay API function.
;
mRML32_8:
        mov     esi,edi
        call    _RelMemory
        ;
mRML32_9:
        popad
        pop     fs
        pop     es
        pop     ds
        ret
        assume ds:nothing
mcbRelMemLinear32 endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set mcbBiggest entry in chunk header.
;
;On Entry:
;
;ES:EBP - Chunk header.
;
mcbSetBiggest   proc    near
        push    eax
        push    ecx
        push    esi
        mov     esi,ebp
        add     esi,MCBCHUNKLEN
        xor     ecx,ecx
api72_0:
        cmp     es:[esi].McbStruc.mcbFreeUsed,"J"
        jnz     api72_1
        cmp     cx,es:[esi].McbStruc.mcbNextSize
        jnc     api72_1
        mov     cx,es:[esi].McbStruc.mcbNextSize
api72_1:
        cmp     es:[esi].McbStruc.mcbNext,"M"   ;last block in last chain?
        jnz     api72_2
        movzx   eax,es:[esi].McbStruc.mcbNextSize
        add     esi,eax
        add     esi,MCBLEN
        jmp     api72_0
api72_2:
        mov     es:[ebp].McbChunkStruc.mcbBiggest,cx
        pop     esi
        pop     ecx
        pop     eax
        ret
mcbSetBiggest   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_GetMemory      proc    near
;
;Attempt to allocate a block of memory.
;
;On Entry:-
;
;EBX    - Size of block required. (-1 to get size of free memory).
;
;On Exit:-
;
;Carry set on error, else:-
;
;ESI    - linear address of memory.
;
;ALL registers may be corrupted.
;
        ;
        push    ecx
        push    edx
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        push    [api73_BlockSize]
        push    [api73_BlockBase]
        push    [api73_BlockHandle]
        ;
        cmp     ebx,-2
        jz      api73_Special
        cmp     ebx,-1                  ;special value to get memory free?
        jnz     api73_NotSpecial
api73_Special:
        call    _GetMemoryMax           ;call free memory code.
        stc
        pop     [api73_BlockHandle]
        pop     [api73_BlockBase]
        pop     [api73_BlockSize]
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     edx
        pop     ecx
        ret
        ;
api73_NotSpecial:
        mov     [api73_BlockSize],ebx   ;store real block size.
        ;
        mov     cx,bx
        shr     ebx,16
        mov     ax,0501h                ;Allocate memory block.
        cwAPI_CallOld
        jc      api73_9
        shl     ebx,16
        mov     bx,cx
        mov     [api73_BlockBase],ebx   ;store linear base address.
        shl     esi,16
        mov     si,di
        mov     [api73_BlockHandle],esi ;store access handle.
        ;
        mov     ax,Res_MEM
        mov     edx,ebx
        mov     ecx,esi
        mov     ebx,[api73_BlockSize]
        call    RegisterResource
        ;
        mov     esi,edx
        clc
        jmp     api73_10
        ;
api73_9:
        stc
api73_10:
        pop     [api73_BlockHandle]
        pop     [api73_BlockBase]
        pop     [api73_BlockSize]
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     edx
        pop     ecx
        ret

_GetMemory      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_GetMemoryMax proc near
;
;Work out biggest memory block remaining.
;
        ;
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;

COMMENT ! MED 02/15/96
        cmp     ebx,-1                  ;normal max reporting?
        jz      api74_normal

        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        cmp     ProtectedType,PT_DPMI
        assume ds:GROUP32
        pop     ds
        jz      api74_normal

        ;
        ;Get free disk space remaining.
        ;
        push    ds
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16

        push    eax
        push    ebx
        push    ecx
        push    esi
        push    edi
        xor     edx,edx
        cmp     VMMHandle,0
        jz      api74_500_1
        mov     dl,VMMName              ;get drive letter for this media.
        sub     dl,'A'                  ;make it real.
        inc     dl                      ;adjust for current type select.
        mov     ah,36h                  ;get free space.
        int     21h
        xor     edx,edx
        cmp     ax,-1                   ;invalid drive?
        jz      api74_500_1
        mul     cx                      ;Get bytes per cluster.
        mul     bx                      ;Get bytes available.
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
        jmp     api74_500_2
        ;
api74_500_1:
        mov     edx,TotalPhysical
api74_500_2:
        add     edx,FreePages
        ;
        pop     edi
        pop     esi
        pop     ecx
        pop     ebx
        pop     eax

        assume ds:GROUP32
        pop     ds
        mov     ebx,edx
        shl     ebx,12
        jmp     api74_exit
END COMMENT !

api74_normal:
        push    es
        mov     edi,offset api74_dpmembuff
        push    ds
        pop     es
        push    ebx
        mov     ax,0500h
        cwAPI_CallOld
        pop     ebx
        pop     es

        cmp     ebx,-2
        jnz     api74_normal2

        mov     ebx,d[api74_dpmembuff+1Ch]
        shl     ebx,12
        jmp     api74_exit

api74_normal2:
        mov     ebx,d[api74_dpmembuff]
        ;
api74_exit:
        pop     ds
        ret

_GetMemoryMax   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_ResizeMemory      proc    near
;
;Re-size a block of memory.
;
;On Entry:-
;
;ESI    - Linear address of memory.
;EBX    - New size.
;
;On Exit:-
;
;ESI    - New linear address of memory.
;
        ;
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    edi
        push    ebp
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        push    [api75_OldESI]
        mov     [api75_OldESI],esi
        push    [api75_OldEBX]
        mov     [api75_OldEBX],ebx
        push    LastResource
        push    LastResource+4
        ;
        mov     edx,esi
        mov     ax,Res_MEM
        call    FindResource
        jc      api75_9
        ;
        mov     ebx,[api75_OldEBX]
        mov     esi,ecx
        mov     di,si
        shr     esi,16
        mov     cx,bx
        shr     ebx,16                  ;set block size.
        mov     ax,0503h                ;release the block.
        cwAPI_CallOld
        jc      api75_9
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        xchg    esi,ebx
        ;
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        mov     edx,LastResource
        mov     es:[edx],esi            ;update base.
        add     edx,4
        mov     es:[edx],ebx            ;update handle.
        add     edx,4
        mov     ebx,[api75_OldEBX]
        mov     es:[edx],ebx            ;update size.
        ;
        clc
        jmp     api75_10
        ;
api75_9:
        stc
api75_10:
        pop     LastResource+4
        pop     LastResource
        pop     [api75_OldEBX]
        pop     [api75_OldESI]
        pop     es
        pop     ds
        pop     ebp
        pop     edi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
_ResizeMemory      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
_RelMemory      proc    near
;
;Release a previously allocated block of memory.
;
;On Entry:-
;
;ESI    - Near DS pointer.
;
        ;
        push    ds
        push    es
        pushad
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        push    LastResource
        push    LastResource+4
        ;
        mov     edx,esi
        mov     ax,Res_MEM
        call    FindResource
        jc      api76_9
        ;
        mov     esi,ecx
        mov     di,si
        shr     esi,16
        mov     ax,0502h                ;release the block.
        cwAPI_CallOld
        jc      api76_9
        ;
        mov     di,bx
        shr     ebx,16
        mov     si,bx
        mov     cx,dx
        shr     edx,16
        mov     bx,dx
        mov     ax,0703h                ;discard the block.
        cwAPI_CallOld
        ;
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        mov     esi,LastResource
        mov     edi,LastResource+4
        xor     eax,eax
        mov     es:[edi],ax
        mov     es:[edi+2],al
        mov     es:[esi],eax
        mov     es:[esi+4],eax
        mov     es:[esi+8],eax
        ;
        clc
        ;
api76_9:
        pop     LastResource+4
        pop     LastResource
        popad
        pop     es
        pop     ds
        ret
_RelMemory      endp


;-------------------------------------------------------------------------
;
;Register a new resource.
;
;On Entry:
;
;AL     - Resource type to register.
;EDX    - Key value.
;ECX    - optional second value.
;EBX    - optional third value.
;
RegisterResource proc near
;        @dprintf DOPT_EXEC,<"RegisterResource enter, type=%X, key=%lX",10>, eax, edx
        push    ds
        push    es
        push    fs
        pushad
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     es,RealSegment
        mov     fs,PSPSegment
        assume fs:_cwEnd         ; to make the assemble assume a 16-bit segment
        ;
        cmp     ResourceTracking,0
        jz      api77_done
        cmp     fs:[EPSP_Struc.EPSP_Resource],0   ;allocated memory for list yet?
        jnz     api77_0
        ;
        ;Allocate initial resource list space.
        ;
        pushad
        call    api77_GetAndInit
        jc      @F
        mov     fs:[EPSP_Struc.EPSP_Resource],esi
@@:
        popad
        cmp     fs:[EPSP_Struc.EPSP_Resource],0
        jz      api77_error
        ;
api77_0:
        ;Search list for free slot.
        ;
        cld
        mov     ebp,ecx                 ;Copy value 2.
        mov     ah,al                   ;Copy type.
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
api77_FreeLoop:
        mov     edi,esi
        add     edi,size ResHead        ;point to types.
        mov     ecx,ResCount            ;get number of entries.
api77_2:
        or      ecx,ecx
        jz      api77_2_0
        js      api77_2_0
        xor     al,al
        repne   scasb                   ;Find NULL entry.
        jz      api77_3
api77_2_0:
        cmp     DWORD PTR es:[esi+ResHead.ResHead_Next],0   ;link field setup?
        jz      api77_Extend
        mov     esi,es:[esi+ResHead.ResHead_Next]   ;link to next block.
        jmp     api77_FreeLoop
        ;
api77_Extend:
        ;Extend the list.
        ;
        push    esi
        call    api77_GetAndInit
        pop     ecx
        jc      api77_error
        mov     es:[esi+ResHead.ResHead_Prev],ecx   ;store back link address.
        mov     es:[ecx+ResHead.ResHead_Next],esi   ;store forward link address.
        jmp     api77_FreeLoop
;
;Check if enough entries are free.
;
api77_3:
        cmp     ah,Res_SEL
        jz      api77_sel
        cmp     ah,Res_MEM
        jz      api77_mem
        cmp     ah,Res_LOCK
        jz      api77_lock
        cmp     ah,Res_DOSMEM
        jz      api77_dosmem
        cmp     ah,Res_CALLBACK
        jz      api77_callback
        cmp     ah,Res_PSP
        jz      api77_psp
        jmp     api77_sel
;
;Check for 2 free entries for DOS memory.
;
api77_dosmem:
        cmp     BYTE PTR es:[edi],0
        jnz     api77_2
        dec     edi
        mov     ecx,edi
        sub     ecx,size ResHead        ;Get offset from list start.
        sub     ecx,esi
        shl     ecx,2
        add     ecx,size ResHead + ResNum
        add     ecx,esi
        mov     BYTE PTR es:[edi],ah    ;Store type.
        mov     DWORD PTR es:[ecx],edx
        inc     edi
        add     ecx,4
        mov     ah,Res_CHAIN
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebp
        jmp     api77_done
;
;Check for 3 free entries for callback.
;
api77_callback:
        cmp     WORD PTR es:[edi],0     ;2 more entries?
        jnz     api77_2
        dec     edi
        mov     ecx,edi
        sub     ecx,size ResHead        ;Get offset from list start.
        sub     ecx,esi
        shl     ecx,2
        add     ecx,size ResHead + ResNum
        add     ecx,esi
        mov     BYTE PTR es:[edi],ah    ;Store type.
        mov     DWORD PTR es:[ecx],edx
        inc     edi
        add     ecx,4
        mov     ah,Res_CHAIN
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebp
        inc     edi
        add     ecx,4
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebx
        jmp     api77_done
;
;Check for 3 free entries for memory.
;
api77_mem:
        cmp     WORD PTR es:[edi],0     ;2 more entries?
        jnz     api77_2
        dec     edi
        mov     ecx,edi
        sub     ecx,size ResHead        ;Get offset from list start.
        sub     ecx,esi
        shl     ecx,2
        add     ecx,size ResHead + ResNum
        add     ecx,esi
        mov     BYTE PTR es:[edi],ah    ;Store type.
        mov     DWORD PTR es:[ecx],edx
        inc     edi
        add     ecx,4
        mov     ah,Res_CHAIN
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebp
        inc     edi
        add     ecx,4
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebx
        jmp     api77_done
;
;Check for 2 free entries for lock's.
;
api77_lock:
        cmp     BYTE PTR es:[edi],0
        jnz     api77_2
        dec     edi
        mov     ecx,edi
        sub     ecx,size ResHead        ;Get offset from list start.
        sub     ecx,esi
        shl     ecx,2
        add     ecx,size ResHead + ResNum
        add     ecx,esi
        mov     BYTE PTR es:[edi],ah    ;Store type.
        mov     DWORD PTR es:[ecx],edx
        inc     edi
        add     ecx,4
        mov     ah,Res_CHAIN
        mov     BYTE PTR es:[edi],ah
        mov     DWORD PTR es:[ecx],ebp
        jmp     api77_done
;
;Only one free entry needed for selectors.
;
api77_psp:
api77_sel:
        dec     edi
        mov     ecx,edi
        sub     ecx,size ResHead        ;Get offset from list start.
        sub     ecx,esi
        shl     ecx,2
        add     ecx,size ResHead + ResNum
        add     ecx,esi
        mov     BYTE PTR es:[edi],ah    ;Store type.
        mov     DWORD PTR es:[ecx],edx
        jmp     api77_done
        ;
api77_error:
        stc
        jmp     api77_100
        ;
api77_done:
        clc
api77_100:
        popad
        pop     fs
        pop     es
        pop     ds
        ret
        ;

;--- subroutine
;--- modifies ESI/EDI

api77_GetAndInit:
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    ebp
        mov     cx,ResSize
        xor     bx,bx
        mov     ax,0501h                ;Allocate memory block.
        cwAPI_CallOld
        jc      api77_GAIerror
        shl     esi,16
        mov     si,di
        shl     ebx,16
        mov     bx,cx
        xchg    esi,ebx
        mov     edi,esi
        xor     eax,eax
        mov     ecx,ResSize/4
        cld
        rep     stosd                       ;init memory.
        mov     DWORD PTR es:[esi+ResHead.ResHead_Handle],ebx   ;store the handle.
        clc
api77_GAIerror:
        pop     ebp
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
        assume ds:nothing
RegisterResource endp


;-------------------------------------------------------------------------
;
;Release a resource entry. If it's a PSP then release its resources.
;
;On Entry:
;
;AL     - Resource type.
;EDX    - Key value.
;
ReleaseResource proc near
        push    ds
        push    es
        push    fs
        pushad
;        @dprintf DOPT_MEM or DOPT_SEL,<"ReleaseResource enter, type=%X, key=%lX",10>, eax, edx
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        assume fs:_cwEnd
        cmp     ResourceTracking,0
        jz      api78_9
        cmp     fs:[EPSP_Struc.EPSP_Resource],0
        jz      api78_9
        xchg    eax,edx
        mov     es,RealSegment
        cld
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
api78_LookLoop:
        mov     edi,esi
        add     edi,size ResHead + ResNum
        mov     ecx,ResNum              ;get number of entries.
api78_2:
        repne   scasd
        jz      api78_0
        mov     esi,es:[esi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jz      api78_9
        jmp     api78_LookLoop
        ;
api78_0:
        mov     ebx,edi
        sub     ebx,size ResHead + ResNum + 4
        sub     ebx,esi
        shr     ebx,2
        add     ebx,esi
        add     ebx,size ResHead
        cmp     BYTE PTR es:[ebx],dl    ;Right type?
        jz      api78_1
        cmp     dl,Res_SEL              ;Selector?
        jnz     api78_2
        cmp     BYTE PTR es:[ebx],Res_PSP
        jnz     api78_2
        ;
api78_1:
        ;At this point we have a match.
        ;
        xor     eax,eax
        sub     edi,4
        mov     dl,es:[ebx]
        xor     dh,dh
        cmp     dl,Res_SEL
        jz      api78_Rel_1
        cmp     dl,Res_LOCK
        jz      api78_Rel_2
        cmp     dl,Res_MEM
        jz      api78_Rel_3
        cmp     dl,Res_DOSMEM
        jz      api78_Rel_2
        cmp     dl,Res_CALLBACK
        jz      api78_Rel_3
        cmp     dl,Res_PSP
        jz      api78_psp
        jmp     api78_9
;
api78_Rel_3:
        mov     BYTE PTR es:[ebx],dh
        mov     DWORD PTR es:[edi],eax
        inc     ebx
        add     edi,4
api78_Rel_2:
        mov     BYTE PTR es:[ebx],dh
        mov     DWORD PTR es:[edi],eax
        inc     ebx
        add     edi,4
api78_Rel_1:
        mov     BYTE PTR es:[ebx],dh
        mov     DWORD PTR es:[edi],eax
        jmp     api78_9
;
;Release a PSP. Assumes that the PSP memory will be released by the caller.
;
api78_psp:
        mov     es:[ebx],dh
        push    w[PSPSegment]
        mov     bx,es:[edi]
        mov     fs,bx
;
;Remove links to all other modules.
;
        cmp     fs:[EPSP_Struc.EPSP_Imports],0
        jz      api78_no_imports
        push    ecx
        push    esi
        push    edi
        mov     esi,fs:[EPSP_Struc.EPSP_Imports]
        mov     ecx,es:[esi]
        add     esi,4
api78_imp0:
        or      ecx,ecx
        jz      api78_imp1
        mov     edi,es:[esi]
        call    UnFindModule
        add     esi,4
        dec     ecx
        jmp     api78_imp0
api78_imp1:
        pop     edi
        pop     esi
        pop     ecx
api78_no_imports:

; MED, 02/07/2000, always explicitly release program selectors and memory
;       cmp     fs:[EPSP_Struc.EPSP_Resource],0
;       jnz     @@normal_res
        ;
        ;This must be a cwLoad PSP.
        ;
        ;release program selectors and memory manually.
        ;
        pushad
        movzx   ecx,fs:[EPSP_Struc.EPSP_SegSize]
        shr     ecx,3
        mov     bx,fs:[EPSP_Struc.EPSP_SegBase]
        or      ecx,ecx
        jz      api78_frelsel9
api78_frelsel:
        @dprintf DOPT_MEM or DOPT_SEL,<"ReleaseResource: calling int 31h, RelSel(bx=%X)",10>,ebx
        Sys     RelSel
        add     bx,8
        dec     ecx
        jnz     api78_frelsel
api78_frelsel9:
        mov     esi,fs:[EPSP_Struc.EPSP_MemBase]
        Sys     RelMemLinear32
        mov     esi,fs:[EPSP_Struc.EPSP_Exports]
        or      esi,esi
        jz      api78_frelsel0
        Sys     RelMemLinear32
api78_frelsel0:
        popad
        ;
api78_normal_res:
        ;Take this PSP out of the linked list.
        ;
        pushad
        mov     ebx,fs
        Sys     GetSelDet32
        mov     esi,es:EPSP_Struc.EPSP_NextPSP[edx]
        mov     edi,es:EPSP_Struc.EPSP_LastPSP[edx]
        or      esi,esi
        jz      api78_ChainPSP0
        mov     es:EPSP_Struc.EPSP_LastPSP[esi],edi
api78_ChainPSP0:
        or      edi,edi
        jz      api78_ChainPSP1
        mov     es:EPSP_Struc.EPSP_NextPSP[edi],esi
api78_ChainPSP1:
        popad
        ;
        mov     ah,50h                  ;set PSP
        int     21h                     ;set new PSP.
        ;
        ;Search for PSP's and release them first.
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]   ;Get resource pointer.
api78_fPSP0:
        or      esi,esi
        jz      api78_NoPSPSearch
        mov     ebp,ResNum
        mov     edi,esi
        add     edi,size ResHead
        mov     edx,esi
        add     edx,size ResHead + ResNum
api78_fPSP1:
        cmp     BYTE PTR es:[edi],Res_PSP
        jnz     api78_fPSP2
        push    ebx
        mov     ebx,es:[edx]            ;Get selector.
        Sys     RelMem
        pop     ebx
api78_fPSP2:
        inc     edi
        add     edx,4
        dec     ebp
        jnz     api78_fPSP1
        mov     esi,es:[esi+ResHead.ResHead_Next]   ;link to next list.
        jmp     api78_fPSP0
        ;
api78_NoPSPSearch:
        ;Now release all other types of resource.
        ;
ifdef SRDPMISTATE
        cmp     fs:[EPSP_Struc.EPSP_DPMIMem],0 ;Any DPMI save buffer?
        jz      api78_psp_0
        xor     ebx,ebx
        xchg    bx,fs:[EPSP_Struc.EPSP_DPMIMem]
        Sys     RelMem
api78_psp_0:
endif
        cmp     fs:[EPSP_Struc.EPSP_INTMem],0 ;Any int/excep vector memory?
        jz      api78_psp_1
        xor     esi,esi
        xchg    esi,fs:[EPSP_Struc.EPSP_INTMem]
        Sys     RelMemLinear32
api78_psp_1:
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]       ;Get resource pointer.
        or      esi,esi
        jz      api78_psp_9
api78_psp_2:
        cmp     DWORD PTR es:[esi+ResHead.ResHead_Next],0   ;Found last entry in chain?
        jz      api78_psp_3
        mov     esi,DWORD PTR es:[esi+ResHead.ResHead_Next]
        jmp     api78_psp_2
api78_psp_3:
        mov     ebp,ResNum
        mov     edi,esi
        add     edi,size ResHead
        mov     edx,esi
        add     edx,size ResHead + ResNum
api78_psp_4:
        cmp     BYTE PTR es:[edi],Res_NULL
        jz      api78_psp_5
        cmp     BYTE PTR es:[edi],Res_SEL
        jz      api78_sel
        cmp     BYTE PTR es:[edi],Res_MEM
        jz      api78_mem
        cmp     BYTE PTR es:[edi],Res_LOCK
        jz      api78_lock
        cmp     BYTE PTR es:[edi],Res_DOSMEM
        jz      api78_dosmem
        cmp     BYTE PTR es:[edi],Res_CALLBACK
        jz      api78_callback
        cmp     BYTE PTR es:[edi],Res_PSP
        jz      api78_rel_psp
        jmp     api78_psp_5
;
;Release a selector.
;
api78_sel:
        mov     BYTE PTR es:[edi],Res_NULL
        push    esi
        push    edi
        push    ebp
        push    edx
        mov     ebx,es:[edx]
        @dprintf DOPT_SEL,<"ReleaseResource: calling _RelSelector, bx=%X",10>, ebx
        push    ResourceTracking
        mov     ResourceTracking,0
        call    _RelSelector
        pop     ResourceTracking
        pop     edx
        pop     ebp
        pop     edi
        pop     esi
        jmp     api78_psp_5
;
;Release some memory.
;
api78_mem:
        mov     ForcedFind+0,edx
        mov     ForcedFind+4,edi
        mov     BYTE PTR es:[edi],Res_NULL
        mov     eax,es:[edx]
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        push    esi
        push    edi
        push    ebp
        push    edx
        mov     esi,eax
        call    _RelMemory
        pop     edx
        pop     ebp
        pop     edi
        pop     esi
        sub     ebp,2
        jmp     api78_psp_5
;
;Release a lock.
;
api78_lock:
        mov     BYTE PTR es:[edi],Res_NULL
        mov     eax,es:[edx]
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        push    esi
        push    edi
        push    ebp
        push    edx
        mov     ecx,es:[edx]
        mov     esi,eax
        push    ResourceTracking
        mov     ResourceTracking,0
        Sys     UnLockMem32
        pop     ResourceTracking
        pop     edx
        pop     ebp
        pop     edi
        pop     esi
        dec     ebp
        jmp     api78_psp_5
;
;Release DOS memory.
;
api78_dosmem:
        mov     BYTE PTR es:[edi],Res_NULL
        mov     eax,es:[edx]
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        push    eax
        push    edx
        mov     edx,eax
        push    ResourceTracking
        mov     ResourceTracking,0
        Sys     RelMemDOS
        pop     ResourceTracking
        pop     edx
        pop     eax
        dec     ebp
        jmp     api78_psp_5
;
;Release a call-back.
;
api78_callback:
        mov     BYTE PTR es:[edi],Res_NULL
        mov     eax,es:[edx]
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        inc     edi
        add     edx,4
        mov     BYTE PTR es:[edi],Res_NULL
        push    eax
        push    ecx
        push    edx
        mov     ecx,eax
        mov     dx,cx
        shr     ecx,16
        mov     ax,0304h
        push    ResourceTracking
        mov     ResourceTracking,0
        cwAPI_CallOld
        assume ds:GROUP16
        pop     ResourceTracking
        pop     edx
        pop     ecx
        pop     eax
        sub     ebp,2
        jmp     api78_psp_5
;
;Release another PSP.
;
api78_rel_psp:
        push    esi
        push    edi
        push    ebp
        push    edx
        mov     ebx,es:[edx]
        Sys     RelMem
        pop     edx
        pop     ebp
        pop     edi
        pop     esi
;
api78_psp_5:
        inc     edi
        add     edx,4
        dec     ebp
        jnz     api78_psp_4
        mov     eax,DWORD PTR es:[esi+ResHead.ResHead_Prev]    ;get back link pointer.
        push    eax
        push    esi
        mov     esi,es:[esi+ResHead.ResHead_Handle] ;get memory handle.
        mov     di,si
        shr     esi,16
        mov     ax,0502h                    ;release the block.
        cwAPI_CallOld
        pop     esi
        push    ebx
        push    ecx
        push    edi
        mov     cx,si
        shr     esi,16
        mov     bx,si
        xor     si,si
        mov     di,ResSize
        mov     ax,0703h                    ;discard the block.
        cwAPI_CallOld
        pop     edi
        pop     ecx
        pop     ebx
        pop     esi
        or      esi,esi
        jz      api78_psp_9
        mov     DWORD PTR es:[esi+ResHead.ResHead_Next],0   ;make sure link pointer is clear
        jmp     api78_psp_3
api78_psp_9:
        ;
        ;Switch back to the old PSP
        ;
        pop     bx
        mov     ah,50h                      ;set PSP
        int     21h                         ;go back to old PSP.
        ;
api78_9:
        popad
        pop     fs
        pop     es
        pop     ds
        ret
        assume ds:nothing
ReleaseResource endp


;-------------------------------------------------------------------------
;
;Check if a resource exists.
;
;On Entry:
;
;AL     - Resource type.
;EDX    - Key value.
;
;On Exit:
;
;Carry clear if it does and,
;
;ECX    - Optional second value.
;EBX    - Optional third value.
;
FindResource    proc    near
        @dprintf DOPT_EXEC,<"FindResource enter, type=%X, key=%lX",10>, eax, edx
        push    ds
        push    es
        push    fs
        push    eax
        push    edx
        push    esi
        push    edi
        push    ebp
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     es,RealSegment
        mov     fs,PSPSegment
        assume fs:_cwEnd
        xchg    eax,edx
        xor     edi,edi
        xchg    edi,ForcedFind+0
        mov     ebp,ForcedFind+4
        or      edi,edi
        jnz     api79_Found
        cmp     ResourceTracking,0
        jz      api79_8
        cmp     fs:[EPSP_Struc.EPSP_Resource],0
        jz      api79_8
        ;
        mov     esi,fs:[EPSP_Struc.EPSP_Resource]
api79_LookLoop:
        mov     ecx,ResNum              ;get number of entries.
        mov     edi,esi
        add     edi,size ResHead + ResNum
        cld
api79_0:
        repne   scasd
        jz      api79_1
        mov     esi,es:[esi+ResHead.ResHead_Next]   ;link to next list.
        or      esi,esi
        jnz     api79_LookLoop
        stc
        jmp     api79_9
api79_1:
        mov     ebp,edi
        sub     ebp,size ResHead + ResNum + 4
        sub     ebp,esi
        shr     ebp,2
        add     ebp,size ResHead
        add     ebp,esi
        cmp     es:[ebp],dl
        jnz     api79_0
        sub     edi,4
        ;
api79_Found:
        cmp     dl,Res_SEL
        jz      api79_8
        cmp     dl,Res_MEM
        jz      api79_2
        cmp     dl,Res_LOCK
        jz      api79_8
        cmp     dl,Res_DOSMEM
        jz      api79_8
        cmp     dl,Res_CALLBACK
        jz      api79_8
        cmp     dl,Res_PSP
        jz      api79_8
        stc
        jmp     api79_9
        ;
api79_2:
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     LastResource,edi
        mov     LastResource+4,ebp
        add     edi,4
        mov     ecx,es:[edi]
        add     edi,4
        mov     ebx,es:[edi]
        ;
api79_8:
        clc
        ;
api79_9:
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     eax
        pop     fs
        pop     es
        pop     ds
        ret
        assume ds:nothing
FindResource    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;--- called by CreatePSP

SaveExecState   proc    near    uses ds es eax ebx ecx edx edi esi

        @dprintf DOPT_EXEC,<"SaveExecState enter",10>
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
;
;Get memory for the interrupt/exception vector save buffer.
;
        mov     ecx,(256*6)+(256*4)+(32*6)
        Sys     GetMemLinear32
        jc      api80_9
        mov     es,PSPSegment
        mov     es:[EPSP_Struc.EPSP_INTMem],esi
        mov     edi,esi
        mov     es,RealSegment
;
;Get protected mode interrupt vectors.
;
        xor     ebx,ebx
api80_GetPVect:
        Sys     GetVect
        mov     DWORD PTR es:[edi+0],edx
        mov     WORD PTR es:[edi+4],cx
        add     edi,6
        inc     ebx
        cmp     ebx,256
        jb      api80_GetPVect
;
;Get protected mode exception vectors.
;
        xor     ebx,ebx
api80_GetEVect:
        Sys     GetEVect
        mov     DWORD PTR es:[edi+0],edx
        mov     WORD PTR es:[edi+4],cx
        add     edi,6
        inc     ebx
        cmp     ebx,32
        jb      api80_GetEVect
;
;Get real mode interrupt vectors.
;
        xor     ebx,ebx
api80_GetRVect:
        Sys     GetRVect
        mov     WORD PTR es:[edi+0],dx
        mov     WORD PTR es:[edi+2],cx
        add     edi,4
        inc     ebx
        cmp     ebx,256
        jb      api80_GetRVect
ifdef SRDPMISTATE
;
;Get memory for DPMI state buffer.
;
        cmp     [DPMIStateSize],0
        jz      api80_NoDPMISave
        mov     ecx,[DPMIStateSize]
        Sys     GetMem32
        jc      api80_9
;
;Save DPMI state.
;
        mov     es,PSPSegment
        mov     es:[EPSP_Struc.EPSP_DPMIMem],bx
        mov     es,ebx
        xor     edi,edi
        mov     al,0 ;save state - rather questionable to do here
        test    BYTE PTR SystemFlags,1
        jz      api80_DPMISave32
        db 66h
api80_DPMISave32:
        call    [DPMIStateAddr]
api80_NoDPMISave:
endif
        clc
api80_9:
        ret
        assume ds:nothing
SaveExecState   endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;--- called by DeletePSP; restore state of caller

LoadExecState   proc    near    uses ds es eax ebx ecx edx edi

        @dprintf DOPT_EXEC,<"LoadExecState enter",10>
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     es,PSPSegment
        mov     edi,es:[EPSP_Struc.EPSP_INTMem]
        or      edi,edi
        jz      api81_NoIntRel
        mov     es,RealSegment
;
;Set 256 protected mode interrupt vectors.
;
        xor     ebx,ebx
api81_SetPVect:
        Sys     GetVect
        xchg    edx,DWORD PTR es:[edi+0]
        xchg    cx,WORD PTR es:[edi+4]
        Sys     SetVect
        cmp     edx,DWORD PTR es:[edi+0]
        jnz     api81_Pdone
        cmp     cx,WORD PTR es:[edi+4]
        jnz     api81_Pdone
        mov     WORD PTR es:[edi+4],-1
api81_Pdone:
        add     edi,6
        inc     bl
        jnz     api81_SetPVect
;
;Set 32 protected mode exception vectors.
;
        xor     ebx,ebx
api81_SetEVect:
        Sys     GetEVect
        xchg    edx,DWORD PTR es:[edi+0]
        xchg    cx,WORD PTR es:[edi+4]
        Sys     SetEVect
        cmp     edx,es:[edi+0]
        jnz     api81_Edone
        cmp     cx,es:[edi+4]
        jnz     api81_Edone
        mov     WORD PTR es:[edi+4],-1
api81_Edone:
        add     edi,6
        inc     ebx
        cmp     ebx,32
        jb      api81_SetEVect
;
;Set 256 real mode interrupt vectors.
;
        xor     ebx,ebx
api81_SetRVect:
        Sys     GetRVect
        xchg    dx,WORD PTR es:[edi+0]
        xchg    cx,WORD PTR es:[edi+2]
        Sys     SetRVect
        cmp     dx,es:[edi+0]
        jnz     api81_Rdone
        cmp     cx,es:[edi+2]
        jnz     api81_Rdone
        mov     WORD PTR es:[edi+2],-1
api81_Rdone:
        add     edi,4
        inc     bl
        jnz     api81_SetRVect
api81_NoIntRel:
ifdef SRDPMISTATE
;
;Restore DPMI stack state.
;
        mov     es,PSPSegment
        mov     ax,es:[EPSP_Struc.EPSP_DPMIMem]
        or      ax,ax
        jz      api81_NoDPMIRel
;--- wasn't active in latest CauseWay versions!
        jmp     api81_NoDPMIRel

        mov     es,eax
        xor     edi,edi
        mov     al,1
        test    BYTE PTR SystemFlags,1
        jz      api81_SaveDPMI32
        db 66h
api81_SaveDPMI32:
        call    [DPMIStateAddr]
api81_NoDPMIRel:
endif
        clc
        ret
        assume ds:nothing
LoadExecState   endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Create a new PSP complete with command line, environment & saved state.
;
;On Entry:
;
;EBX    - flags, as passed to _Exec (0=normal exec,1=load for debug,2=overlay)
;DS:EDX - program name
;ES:ESI - command line.
;CX     - environment.
;
;On Exit:
;
;Carry set on error &,
;
;BX     - new PSP (zero if error).
;
CreatePSP       proc    near
        @dprintf DOPT_EXEC,<"CreatePSP enter: %ls",10>,edx,ds
        mov     eax,ds
        push    ds
        push    es
        push    fs
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        mov     d[api82_Name+0],edx
        mov     w[api82_Name+4],ax
        mov     [api82_Flags],ebx
        mov     d[api82_Command+0],esi
        mov     w[api82_Command+4],es
        mov     [api82_Environment],cx
        ;
        cmp     w[api82_Command+4],0
        jnz     api82_CmdlOK
        mov     w[api82_Command+4],cs
        mov     d[api82_Command],offset nullCmdl
api82_CmdlOK:
        mov     [api82_PSP],0
;
;Allocate PSP memory.
;
        mov     ecx,size EPSP_Struc
        Sys     GetMemLinear32
        jc      api82_error             ;Not enough memory.
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        push    ResourceTracking
        mov     ResourceTracking,0
        Sys     GetSel
        jnc     api82_memOK
        pop     ResourceTracking
        pop     ds
        assume ds:GROUP32
        Sys     RelMemLinear32
        jmp     api82_error
api82_memOK:
        assume ds:GROUP16
        mov     edx,esi
        mov     ecx,size EPSP_Struc - 1
        Sys     SetSelDet32
        pop     ResourceTracking
        pop     ds
        assume ds:GROUP32
;
;Register this selector as a PSP.
;
        mov     ax,Res_PSP
        movzx   edx,bx
        call    RegisterResource        ;register this PSP.
        jnc     api82_memOK2
        ;
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        push    ResourceTracking
        mov     ResourceTracking,0
        @dprintf DOPT_SEL,<"CreatePSP: calling int 31h, RelSel, bx=%X",10>,ebx
        Sys     RelSel
        Sys     RelMemLinear32
        pop     ResourceTracking
        pop     ds
        assume ds:GROUP32
        jmp     api82_error
        ;
api82_memOK2:
        mov     [api82_PSP],bx
;
;Copy parent PSP to this PSP.
;
        push    ds
        mov     es,ebx
        assume es:_cwEnd
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     ds,PSPSegment
        assume ds:_cwEnd
        xor     esi,esi
        xor     edi,edi
        mov     ecx,256/4
        cld
        rep     movsd
        mov     ax,ds:[EPSP_Struc.EPSP_RealENV] ;copy real mode environment
        mov     es:[EPSP_Struc.EPSP_RealENV],ax ;segment.
        mov     eax,ds:[EPSP_Struc.EPSP_ExecCount]
        mov     es:[EPSP_Struc.EPSP_ExecCount],eax
        pop     ds
        assume ds:GROUP32
;
;Initialise PSP fields.
;
        push    fs
        mov     fs,apiDSeg16
        assume fs:GROUP16
        mov     fs,fs:PSPSegment
        assume fs:_cwEnd
        mov     es,ebx
        xor     edx,edx
        mov     DWORD PTR es:[EPSP_Struc.EPSP_Resource],edx ;Clear memory fields.
        mov     es:[EPSP_Struc.EPSP_INTMem],edx
ifdef SRDPMISTATE
;        mov     DWORD PTR es:[EPSP_Struc.EPSP_DPMIMem],edx ;this is type WORD!!!
        mov     es:[EPSP_Struc.EPSP_DPMIMem],dx
endif
        mov     es:[EPSP_Struc.EPSP_Parent],fs              ;set parent PSP.
        mov     DWORD PTR es:[EPSP_Struc.EPSP_DTA+0],80h    ;Use default PSP DTA.
        mov     WORD PTR es:[EPSP_Struc.EPSP_DTA+4],es
        mov     ax,fs:[EPSP_Struc.EPSP_TransProt]           ;inherit current transfer buffer.
        mov     es:[EPSP_Struc.EPSP_TransProt],ax
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:[EPSP_Struc.EPSP_TransReal],ax
        mov     eax,fs:[EPSP_Struc.EPSP_TransSize]
        mov     es:[EPSP_Struc.EPSP_TransSize],eax
        mov     eax,fs:[EPSP_Struc.EPSP_mcbMaxAlloc]
        mov     es:[EPSP_Struc.EPSP_mcbMaxAlloc],eax
        mov     es:[EPSP_Struc.EPSP_mcbHead],edx
        cmp     [api82_Flags],2                             ;cwLoad?
        jz      api82_NoNext
        mov     fs:[EPSP_Struc.EPSP_Next],es
api82_NoNext:
        mov     es:[EPSP_Struc.EPSP_Next],dx
        mov     es:[PSP_Struc.PSP_Environment],dx
        mov     es:[EPSP_Struc.EPSP_SegBase],dx
        mov     es:[EPSP_Struc.EPSP_SegSize],dx
        mov     es:[EPSP_Struc.EPSP_Exports],edx
        mov     es:[EPSP_Struc.EPSP_Imports],edx
        mov     es:[EPSP_Struc.EPSP_Links],edx
        mov     WORD PTR es:[EPSP_Struc.EPSP_EntryCSEIP+4],dx
        mov     WORD PTR es:[EPSP_Struc.EPSP_PSPSel],es
        pop     fs
;
;Update PSP linked list.
;
        push    es
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     esi,es:BasePSPAddress
        mov     es,es:RealSegment
        assume es:nothing
api82_ChainPSP0:
        cmp     es:EPSP_Struc.EPSP_NextPSP[esi],0
        jz      api82_ChainPSP1
        mov     esi,es:EPSP_Struc.EPSP_NextPSP[esi]
        jmp     api82_ChainPSP0
api82_ChainPSP1:
        Sys     GetSelDet32
        mov     es:EPSP_Struc.EPSP_NextPSP[esi],edx
        mov     es:EPSP_Struc.EPSP_LastPSP[edx],esi
        mov     es:EPSP_Struc.EPSP_NextPSP[edx],0
        pop     es
;
;Save full file name into EPSP.
;
        push    ds
        lds     esi,[api82_Name]
        mov     edi,EPSP_Struc.EPSP_FileName
        mov     BYTE PTR es:[edi],0
        cmp     b[esi],0
        jz      api82_SkipCopy
        mov     al,b[esi+1]
        cmp     al,":"                  ;drive specification?
        jnz     api82_GetDrive
        lodsb
        stosb
        mov     dl,al                   ;dl - zero based drive number (0=A,...)
        or      dl,20h
        sub     dl,'a'
        movsb
        jmp     api82_CheckPath
api82_GetDrive:
        mov     ah,19h                  ;get current disc
        int     21h
        mov     dl,al                   ;dl - zero based drive number (0=A,...)
        add     al,'A'                  ;make it a character
        stosb
        mov     al,":"
        stosb
api82_CheckPath:
        mov     al,b[esi]
        cmp     al,"\"                  ;path specification?
        jz      api82_CopyName
        mov     al,"\"                  ;store leading \
        stosb
        mov     BYTE PTR es:[edi],0
        inc     dl                      ;dl - drive number is 1 based (1=A,...)
        xchg    esi,edi
        mov     ah,47h                  ;get current directory without
        push    ds                      ;leading and trailing \
        push    es
        pop     ds
        int     21h                     ;get text
api82_cp1:
        lodsb
        or      al,al
        jnz     api82_cp1
        dec     esi
        xchg    esi,edi
        pop     ds
        mov     al,"\"                  ;store trailing \
        stosb
api82_CopyName:
        lodsb
        stosb
        or      al,al
        jnz     api82_CopyName
api82_SkipCopy:
        pop     ds
;
;Switch to this PSP.
;
        cmp     [api82_Flags],2         ;cwLoad?
        jz      api82_NoPSwitch0
        mov     ah,50h                  ;set PSP
        int     21h                     ;set new PSP.
;
;Set new DTA address.
;
        push    ds
        lds     edx,FWORD PTR es:[EPSP_Struc.EPSP_DTA]
        mov     ah,1ah
        int     21h
        pop     ds
;
;Preserve current state.
;
        call    SaveExecState           ;do old state save.
        jc      api82_error             ;Not enough memory.
api82_NoPSwitch0:
;
;Build command line.
;
        mov     es,[api82_PSP]
        mov     edi,80h
        mov     DWORD PTR es:[edi],0
        push    ds
        lds     esi,[api82_Command]
        movzx   ecx,b[esi]
        inc     ecx
        cld
        rep     movsb
        mov     BYTE PTR es:[edi],13
        pop     ds
;
;Check what's needed with the environment selector.
;
        cmp     [api82_Flags],2         ;cwLoad?
        jz      api82_CopyEnv           ;NoEnv
        mov     ax,[api82_Environment]
        or      ax,ax
        jz      api82_ParentEnv
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd
        mov     es:[PSP_Struc.PSP_Environment],ax
        jmp     api82_GotEnv

api82_CopyEnv:
        push    es
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd
        mov     ax,es:[PSP_Struc.PSP_Environment]  ;Get parents environment.
        mov     es,[api82_PSP]
        mov     es:[PSP_Struc.PSP_Environment],ax
        pop     es
        jmp     api82_NoEnv

;
;Need to make a copy of the parent environment string.
;
api82_ParentEnv:
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd
        mov     es,es:[EPSP_Struc.EPSP_Parent]     ;Get parent PSP.
        mov     es,es:[PSP_Struc.PSP_Environment]  ;Get parents environment.
;
;Find out how long current environment is.
;
        xor     esi,esi
api82_gp2:
        mov     al,es:[esi]             ;Get a byte.
        inc     esi
        or      al,al                   ;End of a string?
        jnz     api82_gp2               ;keep looking.
        mov     al,es:[esi]             ;Double zero?
        or      al,al
        jnz     api82_gp2               ;keep looking.
        add     esi,3                   ;Skip last 0 and word count.
        mov     ecx,esi
        add     ecx,256                 ;allow for exec name length.
        Sys     GetMem32
        jc      api82_error
        mov     eax,es
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd
        mov     es:[PSP_Struc.PSP_Environment],bx
        mov     ecx,esi                 ;get length again.
        push    ds
        mov     ds,eax
        mov     es,ebx
        xor     esi,esi
        xor     edi,edi
        rep     movsb                   ;copy current strings.
        pop     ds
;
;Add execution path and name to environment strings.
;
api82_GotEnv:
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:_cwEnd
        mov     es,es:[PSP_Struc.PSP_Environment]
        xor     edi,edi
api82_gp0:
        mov     al,es:[edi]             ;Get a byte.
        inc     edi
        or      al,al                   ;End of a string?
        jnz     api82_gp0               ;keep looking.
        mov     al,es:[edi]             ;Double zero?
        or      al,al
        jnz     api82_gp0               ;keep looking.
        add     edi,3                   ;Skip last 0 and word count.
;
;Now copy the file name from EPSP to the environment.
;
        push    ds
        mov     ds,[api82_PSP]
        mov     esi,EPSP_Struc.EPSP_FileName
api82_cp2:
        lodsb
        stosb
        or      al,al
        jnz     api82_cp2
        pop     ds
;
;Return to caller.
;
api82_NoEnv:
        clc
        jmp     api82_exit
;
;Not enough memory.
;
api82_error:
        stc
;
api82_exit:
        @dprintf DOPT_EXEC,<"CreatePSP exit",10>
        mov     bx,[api82_PSP]
        pop     fs
        pop     es
        pop     ds
        ret
        assume es:nothing
        assume fs:nothing
;
CreatePSP       endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;--- eax=loader flags (load0._Flags/load1._Flags)
;--- bx=PSP created by CreatePSP (load0._PSP/load1._PSP)

DeletePSP       proc    near
        @dprintf DOPT_EXEC,<"DeletePSP enter",10>
        push    ds
        push    es
        push    fs
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        ;
        mov     [api83_Flags],eax
        mov     [api83_PSP],bx
;
        cmp     [api83_Flags],2
        jz      api83_NoIRel
        call    LoadExecState
;
;Close all open files.
;
;        call    LoseFileHandles
;
;Restore resources.
;
api83_NoIRel:
        mov     es,apiDSeg16
        assume es:GROUP16
        cmp     DWORD PTR [TerminationHandler+0],offset cwClose
        jnz     api83_YesRelRes
        cmp     DebugDump,0
        jnz     api83_NoNRel
api83_YesRelRes:
        mov     es,[api83_PSP]
        assume es:nothing
        mov     bx,es:[EPSP_Struc.EPSP_Parent]
        cmp     d[api83_Flags],2
        jz      api83_NoPRel
        mov     ah,50h                  ;set PSP
        int     21h                     ;restore old PSP.
api83_NoPRel:
        mov     ebx,es
        Sys     RelMem
        cmp     d[api83_Flags],2
        jz      api83_NoNRel
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:nothing
        mov     es:[EPSP_Struc.EPSP_Next],0
api83_NoNRel:
        @dprintf DOPT_EXEC,<"DeletePSP exit",10>
        pop     fs
        pop     es
        pop     ds
        ret
;
DeletePSP       endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Find a module with the right main EXPORT name. If it's not already resident
;search for it on disk and load it if found.
;
;On Entry:
;
;ES:ESI - Module name linear address (ES must address 0-4G)
;
;On Exit:
;
;Carry set on error & EAX is error code else,
;
;EDI    - Linear address of modules PSP.
;
;Error codes match cwLoad values.
;
;All other registers preserved.
;
FindModule      proc    near
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    ebp
        push    ds
        push    es
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32

;
;Search resident modules first.
;
        push    fs
        mov     fs,apiDSeg16
        assume fs:GROUP16
        mov     edi,fs:BasePSPAddress   ;Point to start of PSP chain.
        assume fs:nothing
        pop     fs
        ;
        ;Skip partial path if there is one.
        ;
        push    esi
        movzx   ecx,BYTE PTR es:[esi]
        inc     esi
        mov     ebx,esi
        mov     ebp,ecx
api84_5:
        inc     esi
        cmp     BYTE PTR es:[esi-1],"\"
        jnz     api84_6
        mov     ebx,esi
        mov     ebp,ecx
api84_6:
        dec     ecx
        jnz     api84_5
        mov     esi,ebx
        ;
api84_imp1:
        mov     ebx,esi
        mov     edx,es:EPSP_Struc.EPSP_Exports[edi] ;Point to export memory.
        or      edx,edx                 ;Any exports?
        jz      api84_imp3
        mov     edx,es:[edx+4]          ;Point to module name.
        mov     ecx,ebp
        cmp     cl,es:[edx]             ;Right name length?
        jnz     api84_imp3
        inc     edx
api84_imp2:
        mov     al,es:[ebx]
        cmp     al,es:[edx]             ;right char?
        jnz     api84_imp3
        inc     ebx
        inc     edx
        dec     ecx
        jnz     api84_imp2
        ;
        pop     esi
        jmp     api84_imp5              ;got it!
        ;
api84_imp3:
        mov     edi,es:EPSP_Struc.EPSP_NextPSP[edi]
        or      edi,edi                 ;check there is something else to look at.
        jnz     api84_imp1
        pop     esi
;
;Shit, not a resident module so look for it on disk.
;

;
;Try current PSP's execution path.
;
        push    esi
        push    es
        mov     es,apiDSeg16
        assume es:GROUP16
        mov     es,es:PSPSegment
        assume es:nothing
        mov     bx,es:[PSP_Struc.PSP_Environment]
        pop     es
        push    edx
        push    ecx
        Sys     GetSelDet32
        mov     esi,edx
        pop     ecx
        pop     edx
api84_imp3_0:
        mov     al,es:[esi]             ;Get a byte.
        inc     esi
        or      al,al                   ;End of a string?
        jnz     api84_imp3_0            ;keep looking.
        mov     al,es:[esi]             ;Double zero?
        or      al,al
        jnz     api84_imp3_0            ;keep looking.
        add     esi,3                   ;Skip last 0 and word count.
        ;
        ;Copy up to last "\"
        ;
        mov     edi,offset DllNameSpace
        mov     ebp,edi
api84_imp3_1:
        mov     al,es:[esi]
        inc     esi
        mov     [edi],al
        inc     edi
        cmp     al,"\"
        jnz     api84_imp3_2
        mov     ebp,edi
api84_imp3_2:
        or      al,al
        jnz     api84_imp3_1
        mov     edi,ebp
        pop     esi

        cmp     BYTE PTR es:[esi+2],':' ; see if module name has absolute path
        jne     chkmodname
        cmp     BYTE PTR es:[esi],4
        jb      chkmodname              ; name not long enough for absolute path
        mov     edi,offset DllNameSpace ; reset DLL path to receive module name path only

        ;
        ;Check if module name has any path in it.
        ;
chkmodname:
        push    esi
        movzx   ecx,BYTE PTR es:[esi]
        inc     esi
        mov     ebp,esi
api84_0:
        inc     esi
        cmp     BYTE PTR es:[esi-1],"\"
        jnz     api84_1
        mov     ebp,esi
api84_1:
        dec     ecx
        jnz     api84_0
        mov     ecx,esi
        sub     ecx,ebp
        pop     esi
        push    esi
        inc     esi
api84_2:
        cmp     esi,ebp
        jnc     api84_3
        mov     al,es:[esi]
        mov     [edi],al
        inc     esi
        inc     edi
        jmp     api84_2
api84_3:
        mov     b[edi],0
        ;
        ;Copy module name into work space.
        ;
        mov     edi,offset MODNameSpace
        mov     [edi],cl
        inc     edi
api84_4:
        mov     al,es:[esi]
        mov     [edi],al
        inc     esi
        inc     edi
        dec     ecx
        jnz     api84_4
        pop     esi
        ;
        ;Try and find the module.
        ;
        push    es
        push    ds
        pop     es
        mov     esi,offset MODNameSpace
        mov     edi,offset DllNameSpace
        call    SearchModule            ;look for module with right name.
        pop     es
        jnc     api84_8
;
;Couldn't find the module so return an error.
;
        mov     eax,1
        jmp     api84_error
;
;Try loading the module found.
;
api84_8:
        push    ExecMCount
        mov     ExecMCount,ecx
;
;Set master PSP.
;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        push    PSPSegment
        mov     ah,51h                  ;get PSP
        int     21h
        push    bx
        mov     bx,BasePSP
        mov     ah,50h                  ;set PSP
        int     21h
;
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     edx,offset DllNameSpace
        Sys     cwLoad                  ;call ourselves.
;
;Go back to original PSP
;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        pop     bx
        mov     ah,50h                  ;set PSP
        int     21h
        pop     PSPSegment
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        pop     ExecMCount
        jc      api84_error
;
        mov     bx,si                   ;get the PSP
        push    ecx
        push    edx
        Sys     GetSelDet32             ;need a 32-bit address for it.
        mov     edi,edx
        pop     edx
        pop     ecx
        mov     WORD PTR es:[edi+EPSP_Struc.EPSP_EntryCSEIP+4],cx
        mov     DWORD PTR es:[edi+EPSP_Struc.EPSP_EntryCSEIP+0],edx
;
;Notify debugger of new module loaded.
;
        Sys DbgNtfModLoad
;
;Call DLL's initialisation code.
;
        or      cx,cx
        jz      api84_imp5
        xor     eax,eax
        mov     ax,WORD PTR es:[edi+EPSP_Struc.EPSP_EntryCSEIP+4]
        lar     eax,eax
        test    eax,00400000h
        jnz     api84_imp6_0
        mov     WORD PTR es:[edi+EPSP_Struc.EPSP_EntryCSEIP+2],cx
api84_imp6_0:
        push    ds
        push    es
        push    fs
        push    gs
        pushad
        push    es
        pop     fs
        mov     ds,esi
        mov     es,esi
        test    eax,00400000h
        mov     eax,0
        jnz     api84_imp6
        db 66h
api84_imp6:
        call    fs:[edi+EPSP_Struc.EPSP_EntryCSEIP]
        or      ax,ax
        popad
        pop     gs
        pop     fs
        pop     es
        pop     ds
        mov     eax,1
        jnz     api84_error
;
;Update modules referance count.
;
api84_imp5:
        inc     es:EPSP_Struc.EPSP_Links[edi]
;
;Return module PSP address to caller.
;
        xor     eax,eax
        clc
        jmp     api84_imp10
;
;Couldn't find name so return error to caller.
;
api84_error:
        stc
;
api84_imp10:
        pop     es
        pop     ds
        pop     ebp
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        ret
;
FindModule      endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Reverse the FindModule process.
;
;On Entry:
;
;ES:EDI - module PSP (ES must address 0-4G)
;
;This basicly DEC's the modules referance count and if it becomes zero it
;removes the module from memory.
;
UnFindModule    proc    near
        push    ds
        push    es
        push    fs
        push    gs
        pushad
;
        dec     es:EPSP_Struc.EPSP_Links[edi]
        jnz     api85_8
;
;Notify debugger of module unloading.
;
        Sys DbgNtfModUnload
;
;Get this PSP's selector.
;
        mov     fs,es:EPSP_Struc.EPSP_PSPSel[edi]
;
;Call DLL's exit code.
;
        cmp     WORD PTR fs:[EPSP_Struc.EPSP_EntryCSEIP+4],0
        jz      api85_imp5
        xor     eax,eax
        mov     ax,WORD PTR fs:[EPSP_Struc.EPSP_EntryCSEIP+4]
        lar     eax,eax
        push    ds
        push    es
        push    fs
        push    gs
        pushad
        mov     ax,fs
        mov     ds,eax
        mov     es,eax
        test    eax,00400000h
        mov     eax,1
        jnz     api85_imp6
        db 66h
api85_imp6:
        call    fs:[EPSP_Struc.EPSP_EntryCSEIP]
        or      ax,ax
        popad
        pop     gs
        pop     fs
        pop     es
        pop     ds
;
;Switch to the master PSP.
;
api85_imp5:
        assume ds:nothing
        mov     ds,apiDSeg16
        assume ds:GROUP16
        push    PSPSegment
        mov     bx,BasePSP
        mov     ah,50h                  ;set PSP
        int     21h
;
;Release module.
;
        mov     ebx,fs
        @dprintf DOPT_EXEC,<"UnfindModule: calling int 31h, RelSel(bx=%X)",10>,ebx
        Sys     RelSel
;
;Go back to original PSP
;
        pop     PSPSegment
        assume ds:GROUP32
;
api85_8:
        clc
        popad
        pop     gs
        pop     fs
        pop     es
        pop     ds
        ret
UnFindModule    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Search specified path for a file with the right module name.
;
;On Entry:
;
;DS:EDI - path string.
;ES:ESI - module name.
;
;On Exit:
;
;Carry set on error else,
;
;Path string specified in DS:EDI has file name appended to it.
;
;ECX    - module number within file.
;
SearchModule    proc    near
        mov     eax,ds
        push    ds
        push    es
        pushad
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        mov     d[api86_Path+0],edi
        mov     w[api86_Path+4],ax
        mov     d[api86_Name+0],esi
        mov     w[api86_Name+4],es

; MED 01/05/96
        push    eax
        push    es
        mov     ah,2fh                  ; get DTA in es:ebx
        int     21h
        mov     DWORD PTR EntryDTAAddress+0,ebx
        mov     WORD PTR EntryDTAAddress+4,es
        pop     es

        mov     edx,OFFSET TemporaryDTA
        mov     ah,1ah                  ; set DTA in ds:edx
        int     21h
        pop     eax

;
;Get path length so we don't have to keep scanning the string.
;
        push    ds
        mov     ds,eax
        xor     ecx,ecx
api86_0:
        cmp     b[edi],0
        jz      api86_1
        inc     edi
        inc     ecx
        jmp     api86_0
api86_1:
        pop     ds
        mov     d[api86_Length],ecx
;
;Setup initial mask pointer.
;
        mov     d[api86_Mask],offset api86_Masks
;
;Get DTA address.
;
        push    es
        mov     ah,2fh
        int     21h
        mov     d[api86_DTA+0],ebx
        mov     w[api86_DTA+4],es
        pop     es
;
;Work through all mask types.
;
api86_2:
        mov     esi,d[api86_Mask]
        cmp     b[esi],0                ;end of the list?
        jz      api86_9
;
;Add new mask to path string.
;
        add     d[api86_Mask],4         ;move to next mask.
        les     edi,[api86_Path]
        add     edi,d[api86_Length]     ;point to end of path string.
        mov     BYTE PTR es:[edi],"*"
        inc     edi
        movsd                           ;copy extension.
        mov     BYTE PTR es:[edi],0
;
;Work through all files with the right extension.
;
        push    ds
        lds     edx,[api86_Path]
        xor     cx,cx
        mov     ah,4eh                  ;find first file
        int     21h
        pop     ds
        jc      api86_2
        jmp     api86_4
;
api86_3:
        mov     ah,4fh
        int     21h
        jc      api86_2
;
;Add this name to the path string?
;
api86_4:
        les     edi,[api86_DTA]
        test    BYTE PTR es:[edi+21],16 ;DIR?
        jnz     api86_3
        push    ds
        mov     eax,d[api86_Length]     ;point to end of path string.
        lds     esi,[api86_Path]
        add     esi,eax
        lea     edi,[edi+1eh]           ;point to file name.
        mov     ecx,13
        cld
api86_5:
        mov     al,es:[edi]
        mov     [esi],al
        inc     esi
        inc     edi
        or      al,al
        jz      api86_6
        dec     ecx
        jnz     api86_5
api86_6:
        mov     b[esi],0                ;terminate the name.
        pop     ds
;
;Find out what the files "module" name is.
;
        ;
        ;Open the file.
        ;
        push    ds
        lds     edx,[api86_Path]
        mov     ax,3d00h
        int     21h
        pop     ds
        jc      api86_7
        mov     w[api86_Handle],ax
        mov     bx,ax
        mov     d[api86_Count],0
        ;
api86_11:
        ;See what sort of file it is.
        ;
        mov     edx,offset api86_ID
        mov     ecx,2
        mov     ah,3fh
        int     21h
        jc      api86_7
        cmp     ax,cx
        jnz     api86_7
        cmp     w[api86_ID],"ZM"
        jz      api86_MZ
        cmp     w[api86_ID],"P3"
        jz      api86_3P
        jmp     api86_7
;
;Process an MZ section.
;
api86_MZ:
        mov     edx,offset api86_ID+2
        mov     ecx,1bh-2
        mov     ah,3fh
        int     21h
        jc      api86_7
        cmp     ax,cx
        jnz     api86_7
        cmp     w[api86_ID+18h],40h
        jz      api86_LE
        ;
        ;Find out how long the MZ bit is.
        ;
        mov     ax,w[api86_ID+2+2]      ;get length in 512 byte blocks

; MED 04/26/96
        cmp     WORD PTR [api86_ID+2],0
        je      medexe4                 ; not rounded if no modulo

        dec     ax                      ;lose 1 cos its rounded up

medexe4:
        add     ax,ax                   ;mult by 2
        mov     dh,0
        mov     dl,ah
        mov     ah,al
        mov     al,dh                   ;mult by 256=*512
        add     ax,w[api86_ID+2]        ;add length mod 512
        adc     dx,0                    ;add any carry to dx
        mov     cx,ax
        sub     cx,1bh                  ;account for the header.
        sbb     dx,0
        xchg    cx,dx                   ;swap round for DOS.
        mov     ax,4201h                ;set new file offset.
        int     21h
        jmp     api86_11
;
;Process what should be an LE section.
;
api86_LE:
        mov     ecx,3ch
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h                     ;move to LE offset.
        mov     edx,offset api86_ID
        mov     ecx,4
        mov     ah,3fh
        int     21h                     ;read LE offset.
        jc      api86_7
        cmp     ax,4
        jnz     api86_7
        cmp     d[api86_ID],0
        jz      api86_7
        mov     ecx,d[api86_ID]
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h                     ;point to LE section.
        mov     edx,offset api86_ID+4
        mov     ah,3fh
        mov     ecx,2
        int     21h                     ;read ID string.
        jc      api86_7
        cmp     ax,2
        jnz     api86_7

        cmp     w[api86_ID+4],"EL"
        jz      medle1

IFDEF LXWORK
        mov     eax,DWORD PTR [api86_ID+4]
        mov     cs:[0],al
        cmp     w[api86_ID+4],"XL"      ; MED
        jnz     api86_7
        mov     cs:[0],bl
ENDIF
        jmp     api86_7

        ;
        ;Process an LE section.
        ;
medle1:
        mov     ecx,LE_Header.LE_ResidentNames-2
        mov     dx,cx
        shr     ecx,16
        mov     ax,4201h
        int     21h                     ;move to module name offset.
        mov     edx,offset api86_ID+4
        mov     ecx,4
        mov     ah,3fh
        int     21h                     ;read module name offset.
        jc      api86_7
        cmp     ax,4
        jnz     api86_7
        mov     ecx,d[api86_ID]
        add     ecx,d[api86_ID+4]
        mov     dx,cx
        shr     ecx,16
        mov     ax,4200h
        int     21h                     ;move to module name.
        mov     edx,offset api86_ID
        mov     ecx,256
        mov     ah,3fh
        int     21h                     ;read the module name.
        ;
        ;See if this module has right name.
        ;
        push    es
        les     edx,[api86_Name]        ;point to name we're looking for.
        mov     edi,offset api86_ID
        movzx   ecx,b[edi]
        cmp     cl,es:[edx]             ;right length?
        jnz     api86_le7
api86_le5:
        inc     edx
        inc     edi
        mov     al,es:[edx]
        cmp     al,[edi]
        jnz     api86_le7
        dec     ecx
        jnz     api86_le5
        pop     es
        ;
        ;Close the file.
        ;
        xor     bx,bx
        xchg    bx,w[api86_Handle]
        mov     ah,3eh
        int     21h
        jmp     api86_8
        ;
api86_le7:
        pop     es
        jmp     api86_7
;
;Process a 3P section.
;
api86_3P:
        mov     edx,offset api86_ID+2
        mov     ecx,size NewHeaderStruc-2
        mov     ah,3fh
        int     21h
        jc      api86_7
        cmp     ax,cx
        jnz     api86_7
        sub     d[api86_ID+NewHeaderStruc.NewSize],size NewHeaderStruc
        ;
        ;Check this file has exports.
        ;
        cmp     d[api86_ID+NewHeaderStruc.NewExports],0
        jz      api86_3p6
        ;
        ;Skip segment definitions.
        ;
        movzx   edx,w[api86_ID+NewHeaderStruc.NewSegments]
        shl     edx,3
        Sys     cwcInfo
        jc      api86_3p0
        mov     edx,eax
api86_3p0:
        sub     DWORD PTR [api86_ID+NewHeaderStruc.NewSize],edx
        mov     cx,dx
        shr     edx,16
        xchg    cx,dx
        mov     ax,4201h
        int     21h
        ;
        ;Skip relocations.
        ;
        mov     edx,d[api86_ID+NewHeaderStruc.NewRelocs]
        shl     edx,2
        or      edx,edx
        jz      api86_3p1
        Sys     cwcInfo
        jc      api86_3p1
        mov     edx,eax
api86_3p1:
        sub     d[api86_ID+NewHeaderStruc.NewSize],edx
        mov     cx,dx
        shr     edx,16
        xchg    cx,dx
        mov     ax,4201h
        int     21h
        ;
        ;Load export details.
        ;
        mov     ecx,d[api86_ID+NewHeaderStruc.NewExports]
        Sys     GetMemLinear32
        jc      api86_7                 ;treat memory error as no file.
        mov     edx,ecx
        push    ds
        mov     ds,apiDSeg16
        assume ds:GROUP16
        mov     ds,RealSegment
        assume ds:GROUP32
        Sys     cwcInfo
        jc      api86_3p2

        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        sub     d[api86_ID+NewHeaderStruc.NewSize],ecx
        pop     ds

        push    es
        mov     eax,ds
        mov     es,eax
        mov     edi,esi
        Sys     cwcLoad
        pop     es
        jc      api86_3p3
        jmp     api86_3p4

api86_3p2:
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        sub     d[api86_ID+NewHeaderStruc.NewSize],edx
        pop     ds

        mov     ecx,edx
        mov     edx,esi
        call    ReadFile
        jc      api86_3p3
        cmp     eax,ecx
        jz      api86_3p4
api86_3p3:
        pop     ds
        Sys     RelMemLinear32
        jmp     api86_7
api86_3p4:
        mov     eax,ds
        mov     es,eax
        pop     ds
        ;
        ;See if this module has right name.
        ;
        push    ds
        lds     edx,[api86_Name]        ;point to name we're looking for.
        mov     edi,esi
        add     edi,es:[edi+4]          ;get offset of module name.
        movzx   ecx,BYTE PTR es:[edi]
        cmp     cl,[edx]                ;right length?
        jnz     api86_3p7
api86_3p5:
        inc     edx
        inc     edi
        mov     al,[edx]
        cmp     al,es:[edi]
        jnz     api86_3p7
        dec     ecx
        jnz     api86_3p5
        pop     ds
        ;
        ;Release EXPORT record memory.
        ;
        Sys     RelMemLinear32
        ;
        ;Close the file.
        ;
        xor     bx,bx
        xchg    bx,w[api86_Handle]
        mov     ah,3eh
        int     21h
        jmp     api86_8
;
;Not this one, move to next part of the file.
;
api86_3p7:
        pop     ds
        Sys     RelMemLinear32
api86_3p6:
        mov     edx,d[api86_ID+NewHeaderStruc.NewSize]
        mov     cx,dx
        shr     edx,16
        xchg    cx,dx
        mov     ax,4201h
        int     21h
        inc     d[api86_Count]
        jmp     api86_11
;
;Make sure file is closed.
;
api86_7:
        xor     bx,bx
        xchg    bx,w[api86_Handle]
        or      bx,bx
        jz      api86_3
        mov     ah,3eh
        int     21h
        jmp     api86_3
;
api86_8:
        clc
        jmp     api86_10
;
api86_9:
        stc
;
api86_10:

; MED 01/05/96
        pushfd
        push    ds
        lds     edx,EntryDTAAddress
        mov     ah,1ah                  ; set DTA in ds:edx
        int     21h
        pop     ds
        popfd

        popad
        mov     ecx,d[api86_Count]
        pop     es
        pop     ds
        ret
;
SearchModule    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Search a modules export table for name specified.
;
;On Entry:
;
;ES:EDI - Export table.
;ES:EBP - function name.
;
;On Exit:
;
;Carry set on error else,
;
;ES:EDI - matched export entry.
;
;All other registers preserved.
;
FindFunction    proc    near

        @dprintf DOPT_EXEC,<"FindFunction enter",10>
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    ebp
        ;
        mov     edx,es:[edi]            ;get number of exports.
        add     edi,4+4                 ;skip entry count and module name.
        ;
api87_imp11:
        push    edi
        mov     edi,es:[edi]            ;point to name string.
        mov     ebx,ebp
        xor     ecx,ecx
        mov     cl,es:[edi+6]
        cmp     cl,es:[ebx]
        jnz     api87_imp13
        add     edi,6+1

        @dprintf DOPT_EXEC,<"Checking against function: %ls",10>,edi,es
        inc     ebx
        ;
api87_imp12:
        mov     al,es:[edi]
        cmp     al,es:[ebx]
        jnz     api87_imp13
        inc     edi
        inc     ebx
        dec     ecx
        jnz     api87_imp12
        jmp     api87_imp14
        ;
api87_imp13:
        pop     edi
        add     edi,4
        dec     edx
        jnz     api87_imp11
        jmp     api87_error
        ;
api87_imp14:
        pop     edi
        mov     edi,es:[edi]
        clc
        jmp     api87_10
        ;
api87_error:
        stc
api87_10:
        pop     ebp
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        @dprintf DOPT_EXEC,<"FindFunction exit",10>
        ret
FindFunction    endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Load, relocate and execute the application code. All format loader.
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
;2      - Not a recognisable format.
;3      - Not enough memory.
;
_Exec   proc    near
        @dprintf DOPT_EXEC,<"_Exec enter, ss:esp=%X:%lX",10>,ss,esp
        mov     eax,ds
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
;Preserve details.
;
        mov     d[api88_Name+0],edx
        mov     w[api88_Name+4],ax
        mov     [api88_Flags],ebx
        mov     d[api88_Command+0],esi
        mov     w[api88_Command+4],es
        mov     [api88_Environment],cx
        mov     [api88_Handle],0
;
;Try and open the file.
;
        push    ds
        lds     edx,[api88_Name]
        mov     ax,3d00h                ;open, read only.
        int     21h
        pop     ds
        jc      api88_no_file_error
        mov     [api88_Handle],ax       ;store the handle.
;
;Find out what format the current section is.
;
api88_0:
        mov     bx,[api88_Handle]
        mov     edx,offset apiMZHeader
        mov     ecx,sizeof apiMZHeader
        mov     ah,3fh
        int     21h
        jc      api88_file_error
        cmp     ax,cx
        jnz     api88_file_error
        ;
        cmp     [edx].MZHeader.Signature,"ZM"      ;MZ EXE?
        jz      api88_MZ
        cmp     [edx].MZHeader.Signature,"EL"      ;LE EXE?
        jz      api88_LE
        cmp     [edx].MZHeader.Signature,"P3"      ;3P EXE?
        jz      api88_3P

IFDEF LXWORK
        cmp     [edx].MZHeader.Signature,"XL"      ;LX EXE? -- MED
        jz      api88_LE
        mov     cs:[0],dl
ENDIF

        jmp     api88_file_error
        ;
api88_MZ:
        ;Look for an LE offset.
        ;
        cmp     [edx].MZHeader.RelocFirst,40h       ;LE offset present?
        jb      api88_MZ2
        mov     eax,[edx].MZHeader.NewHdrPos
        or      eax,eax                             ;any "new hdr" offset?
        jz      api88_MZ2
        shld    ecx,eax,16
        mov     edx,eax
        mov     ax,4200h
        int     21h
        jmp     api88_0
        ;
api88_MZ2:
        ;Get MZ length and skip it.
        ;
        movzx   eax,[edx].MZHeader.NumPages ;get length in 512 byte blocks

; MED 04/26/96
        cmp     [edx].MZHeader._Length,0
        je      medexe5                 ; not rounded if no modulo
        dec     eax                     ;lose 1 cos its rounded up
medexe5:
        shl     eax,9                   ;*512
        movzx   ecx,apiMZHeader._Length ;add length mod 512
        add     eax,ecx
        shld    ecx,eax,16
        mov     edx,eax
        mov     ax,4200h                ;set absolute position.
        int     21h
        jmp     api88_0                 ;and now try with the new header position
;
;Run an LE format program.
;
api88_LE:
        mov     [api88_ExecAdd],offset LoadLE
        jmp     api88_exec
;
;Run a 3P format program.
;
api88_3P:
        mov     [api88_ExecAdd],offset Load3P
;
;Close the file.
;
api88_exec:
        mov     bx,[api88_Handle]
        mov     [api88_Handle],0
        mov     ah,3eh
        int     21h
;
;Fetch registers & call format specific loader.
;
        mov     edx,d[api88_Name+0]
        mov     ax,w[api88_Name+4]
        mov     ebx,[api88_Flags]
        mov     esi,d[api88_Command+0]
        mov     es,w[api88_Command+4]
        mov     cx,[api88_Environment]
        mov     ds,eax
        assume ds:nothing
        call    cs:[api88_ExecAdd]	; it's a NEAR32 address

        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
api88_exit:
        pushfd
        push    eax
        cmp     [api88_Handle],0
        jz      api88_e0
        mov     bx,[api88_Handle]
        mov     ah,3eh
        int     21h
api88_e0:
        pop     eax
        popfd
;
        pop     ds
        @dprintf DOPT_EXEC,<"_Exec exit",10>
        ret
;
api88_no_file_error:
        mov        eax,1
        stc
        jmp     api88_exit
api88_file_error:
        mov     eax,2
        stc
        jmp     api88_exit
api88_mem_error:
        mov     eax,3
        stc
        jmp     api88_exit
;
_Exec   endp


;-------------------------------------------------------------------------
;
;Execute module. Called by Load3P and LoadLE
;
;On Entry:
;
;EAX    - Flags.
;EBX    - Entry EIP
;CX     - Entry CS
;EDX    - Entry ESP
;SI     - Entry SS
;DI     - Entry ES (=PSP)
;BP     - Entry DS
;

EMFRM struct 4
_Patched     dw 0	;patch flags for OldInt21h
_errorcode   dw 0
;_Flags       dd 0
_EntryEIP    dd 0
_EntryCS     dw 0
_EntryESP    dd 0
_EntrySS     dw 0	;must follow api90_EntryESP
;_EntryES    dd 0	;the PSP; direct move DI -> ES
;_EntryDS     dd 0
EMFRM ends

ExecModule      proc    near

local   oldpsp:dword
local   api90:EMFRM

        test    b cs:apiSystemFlags,1
        jz      @F
        movzx   ebp,bp
@@:
        push    ds
        push    es
        @dprintf -1,<"ExecModule enter, cs:eip=%X:%lX, ss:esp=%X:%lX",10>,ecx,ebx,esi,edx
        ;
;        mov     [api90._Flags],eax     ;flags not used
        mov     [api90._EntryEIP],ebx
        mov     [api90._EntryCS],cx
        mov     [api90._EntryESP],edx
        mov     [api90._EntrySS],si
;        mov     [api90._EntryES],edi
        mov     es,edi
;        mov     eax,[ebp+0]
;        mov     [api90._EntryDS],eax
        mov     [api90._Patched],0
;
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        xchg    edi,[api90_PSP]
        mov     oldpsp,edi
        mov     es:[EPSP_Struc.EPSP_Links],8000000h
;
;Patch Int 21h for function 4ch.
;
        cmp     w[api90_OldInt21h+4],0      ;Already patched?
        jnz     api90_NoPatch21h
        ;
        mov     bl,21h
        Sys     GetVect
        test    BYTE PTR apiSystemFlags,1   ;16/32 bit?
        jz      api90_Use32Bit100
        movzx   edx, dx
api90_Use32Bit100:
        mov     d[api90_OldInt21h+0],edx
        mov     w[api90_OldInt21h+4],cx
        mov     bl,21h
        mov     ecx,cs
        mov     edx,offset api90_Int21Patch
        Sys     SetVect                     ;put us in the running.
        mov     [api90._Patched],-1
;
;Patch exception termination handler address.
;
api90_NoPatch21h:
        mov     es,apiDSeg16
        assume es:GROUP16
        push    DWORD PTR es:[TerminationHandler+0]
        push    DWORD PTR es:[TerminationHandler+4]
        mov     DWORD PTR es:[TerminationHandler+0],offset api90_KillIt
        mov     DWORD PTR es:[TerminationHandler+4],cs
        push    ebp
        mov     es,[api90_PSP]
        assume es:nothing
        mov     DWORD PTR es:[EPSP_Struc.EPSP_SSESP+0],esp
        mov     WORD PTR es:[EPSP_Struc.EPSP_SSESP+4],ss
;        mov     ds,[api90._EntryDS]
        mov     ds,[ebp+0]    ;entry DS hold by org. BP
        assume ds:nothing
        mov     eax,[api90._EntryEIP]
        mov     dx,[api90._EntryCS]
        lss     esp,fword ptr [api90._EntryESP]      ;fetch new stack address.
        push    edx
        push    eax
        xor     eax,eax
        mov     ebx,eax
        mov     ecx,eax
        mov     edx,eax
        mov     esi,eax
        mov     edi,eax
        mov     ebp,eax
        mov     fs,eax
        mov     gs,eax
        sti
        retd                                ;pass control to program.
        ;
api90_Int21Patch:
        cmp     ah,4ch                      ;terminate?
        jz      api90_CheckKillIt
        cmp     ah,31h                      ;TSR?
        jnz     api90_OldVect
;
;Clean things up and exit.
;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16

;--- why using TSREnd here?
        mov     ax,TSREnd
        sub     ax,PSPSegment

        mov     edi,offset Int21Buffer
        mov     es,cs:apiDSeg32
        mov     es:RealRegsStruc.Real_AX[edi],3100h
        mov     es:RealRegsStruc.Real_DX[edi],ax
        mov     bl,21h
        Sys     IntXX
        jmp     api90_KillIt                ;just incase!
        ;
api90_OldVect:
        jmp     FWORD PTR cs:[api90_OldInt21h]  ;pass control to old handler.
        assume ds:nothing
        ;
api90_CheckKillIt:
ifndef NOI21RMHOOK
        push    ds
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        cmp     Int21hExecCount,0
        pop     ds
        assume ds:nothing
        jnz     api90_OldVect
endif
;
;Clean things up and return to caller.
;
api90_KillIt:
        mov     ds,cs:apiDSeg32              ;make our data addressable.
        assume ds:GROUP32
        mov     es,[api90_PSP]
        lss     esp,FWORD PTR es:[EPSP_Struc.EPSP_SSESP]
        pop     ebp
        mov     [api90._errorcode],ax
        mov     eax,oldpsp
        mov     [api90_PSP],eax
        mov     es,apiDSeg16
        assume es:GROUP16
        pop     DWORD PTR es:[TerminationHandler+4]
        pop     DWORD PTR es:[TerminationHandler+0]
        assume es:nothing
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        sti                                 ;turn interrupts back on.
        ;
        cmp     [api90._Patched],0
        jz      api90_NoRel21h
        mov     edx,d[api90_OldInt21h+0]
        xor     ecx,ecx
        xchg    cx,w[api90_OldInt21h+4]
        mov     bl,21h
        Sys     SetVect                     ;restore old handler.
api90_NoRel21h:
;
;Get mouse event target state.
;
        xor     eax,eax
        xor     ebx,ebx
        cmp     int33h.UserOK,0
        jz      api90_meventnodum
        test    BYTE PTR apiSystemFlags,1
        jz      api90_mevent32
        movzx   eax,w[int33h.UserCode+0]
        movzx   ebx,w[int33h.UserCode+2]
        jmp     api90_mevent16
api90_mevent32:
        mov     eax,d[int33h.UserCode+0]
        movzx   ebx,w[int33h.UserCode+4]
api90_mevent16:
        mov     edx,cs
        cmp     bx,dx
        jnz     api90_meventnodum
        cmp     eax,offset Int33hDummy
        jnz     api90_meventnodum
        xor     eax,eax
        xor     ebx,ebx
api90_meventnodum:
;        mov     ds,apiDSeg16
;        assume ds:GROUP16
        mov     MouseETarget+0,eax
        mov     MouseETarget+4,ebx
        ;
;       mov     ax,0                        ;reset mouse if present.
;       int     33h
        mov     ax,[api90._errorcode]
        ;
        pop     es
        pop     ds
        assume ds:nothing
        ret
;
ExecModule      endp

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
LoseFileHandles proc near

if 0
        push    ds
        push    es
        push    fs
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     fs,PSPSegment
        cmp     WORD PTR fs:[PSP_HandlePtr+2],0
        jz      api92_donehandles
        les     di,DWORD PTR fs:[PSP_HandlePtr]
        mov     cx,WORD PTR fs:[PSP_Handles]

  if 0
api92_RelHandles:
        cmp     BYTE PTR es:[di],255
        jz      api92_NoRelHandle
        movzx   bx,BYTE PTR es:[di]
        mov     ah,3eh
        int     21h
api92_NoRelHandle:
        inc     di
        dec     cx
        jnz     api92_RelHandles
  endif

        mov     eax,fs
        mov     ebx,es
        cmp     ax,bx
        jz      api92_donehandles
        Sys     RelMem

  if 0
;
;Reduce real mode handle count.
;
        mov     edi,offset Int21Buffer
        mov     es,cs:apiDSeg32
        mov     ax,TotalHandles         ;get current count.
        sub     ax,WORD PTR fs:[PSP_Handles]
        or      ax,1                    ;force bit 0 for DOS bug.
        mov     es:[edi].RealRegsStruc.Real_AX,6700h
        mov     es:[edi].RealRegsStruc.Real_EBX,eax
        mov     bl,21h
        push    ds
        pop     es
        Sys     IntXX
        sub     TotalHandles,20         ;update total handle count.
  endif

api92_donehandles:

        pop     fs
        pop     es
        pop     ds
endif

        ret
LoseFileHandles endp


;-------------------------------------------------------------------------
;
;Read some data from a file.
;
;On Entry:
;
;DS:EDX - Address to read to.
;ECX    - length to read.
;EBX    - file handle.
;
;On Exit:
;
;EAX    - bytes read.
;
ReadFile        proc    near
        push    ecx
        push    edx
api93_0:
        push    ecx
        cmp     ecx,65536-16            ;size of chunks to load.
        jc      api93_1
        mov     ecx,65536-16
api93_1:
        mov     ah,3fh
        int     21h                     ;read from the file.
        pop     ecx
        jc      api93_2                 ;DOS error so exit NOW.
        movzx   eax,ax                  ;get length read.
        add     edx,eax                 ;move memory pointer.
        sub     ecx,eax                 ;update length counter.
        jz      api93_2                 ;read as much as was wanted.
        or      eax,eax                 ;did we read anything?
        jnz     api93_0
api93_2:
        mov     eax,edx
        pop     edx
        sub     eax,edx
        pop     ecx
        ret
ReadFile        endp


;-------------------------------------------------------------------------
;
;Patch exceptions to do register dump and terminate.
;
PatchExc        proc    far
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
;Patch some interrupts.
;
        mov     bl,23h
        Sys     GetVect
        test    BYTE PTR apiSystemFlags,1
        jz      api94_iUse32
        movzx   edx,dx
api94_iUse32:
        mov     d[OldInt23+0],edx
        mov     w[OldInt23+4],cx
api94_iDone3216:
        mov     edx,offset Int23Handler
        mov     ecx,cs
        Sys     SetVect
        ;
        mov     bl,24h
        Sys     GetVect
        test    BYTE PTR apiSystemFlags,1
        jz      api94_i24Use32
        movzx   edx,dx
api94_i24Use32:
        mov     d[OldInt24+0],edx
        mov     w[OldInt24+4],cx
api94_i24Done3216:
        mov     edx,offset Int24Handler
        mov     ecx,cs
        Sys     SetVect
;
;Patch 1Bh vector.
;
        ;
        ;Get a call back.
        ;
        push    ds
        push    es
        push    ds
        pop     es
        mov     esi,offset CtrlBrkEvent
        mov     edi,offset CtrlBrkEventTab
        push    cs
        pop     ds
        Sys     GetCallBack
        pop     es
        pop     ds
        jc      api94_0

        ;
        ;Get current protected mode 1Bh vector.
        ;
        push    cx
        push    dx
        mov     bl,1bh
        Sys     GetVect
        test    BYTE PTR apiSystemFlags,1
        jz      api94_1
        movzx   edx,dx
api94_1:
        mov     d[OldInt1Bp+0],edx
        mov     w[OldInt1Bp+4],cx
        ;
        ;Install our own handler.
        ;
        mov     edx,offset Int1bHandler
        mov     ecx,cs
        mov     bl,1bh
        Sys     SetVect
        pop     dx
        pop     cx

        ;
        ;Get current real mode 1Bh vector.
        ;
        push    cx
        push    dx
        mov     bl,1bh
        Sys     GetRVect
        mov     w[OldInt1Br+0],dx
        mov     w[OldInt1Br+2],cx
        pop     dx
        pop     cx
        mov     w[Int1bhcall+0],dx
        mov     w[Int1bhcall+2],cx
        ;
        ;Install call-back.
        ;
        mov     bl,1bh
        Sys     SetRVect
api94_0:
        mov     ds,apiDSeg16
        assume ds:GROUP16
        or      apiExcepPatched,-1
        pop     ds
        ret
        assume es:nothing
PatchExc        endp


;-------------------------------------------------------------------------
;
;Remove exception patchs.
;
UnPatchExc      proc    far
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
;
;Release real mode hardware interupt vectors and call back entries.
;
        cmp     [OldInt1Br],0
        jz      api95_no1b
        mov     dx,w[OldInt1Br+0]
        mov     cx,w[OldInt1Br+2]
        mov     bl,1bh
        Sys     SetRVect
        mov     dx,w[Int1bhcall+0]
        mov     cx,w[Int1bhcall+2]
        Sys     RelCallBack
        mov     edx,d[OldInt1Bp+0]
        mov     cx,w[OldInt1Bp+4]
        mov     bl,1bh
        Sys     SetVect
;
api95_no1b:
        mov     edx,d[OldInt23+0]
        mov     cx,w[OldInt23+4]
        jcxz    api95_i0
        mov     bl,23h
        Sys     SetVect
api95_i0:
        mov     edx,d[OldInt24+0]
        mov     cx,w[OldInt24+4]
        jcxz    api95_i1
        mov     bl,24h
        Sys     SetVect
api95_i1:
        pop     ds
        assume ds:nothing
        ret
UnPatchExc      endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
;--- this is a real-mode callback, ES:(E)DI -> RMCS, DS:(E)SI -> Real-Mode stack

CtrlBrkEvent    proc    far
        cld
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api96_start32
        movzx   esi, si
        movzx   edi, di
api96_start32:
        lodsw   [esi]
        mov     es:RealRegsStruc.Real_IP[edi],ax
        lodsw   [esi]
        mov     es:RealRegsStruc.Real_CS[edi],ax
        lodsw   [esi]
        mov     es:RealRegsStruc.Real_Flags[edi],ax
        add     es:RealRegsStruc.Real_SP[edi],6
        ;
        ;Give protected mode handler a shout.
        ;
        int     1bh
        ;
        ;Go back to caller.
        ;
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api96_Use32Bit2
        iret
api96_Use32Bit2:
        iretd
;
CtrlBrkEvent    endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==
;
;Base handler for protected mode int 1bh's.
;
Int1bHandler    proc    far
        push    es
        pushad
        ;
        ;Need to chain to original real mode handler.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int1bRegs
        mov     eax,cs:[OldInt1Br]
        mov     es:RealRegsStruc.Real_CSIP[edi],eax
        mov     es:RealRegsStruc.Real_SSSP[edi],0
        mov     ax,0302h
        xor     ecx,ecx
        xor     ebx,ebx
        int     31h
        ;
        ;Return to caller.
        ;
        popad
        pop     es
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api97_0
        iret
api97_0:
        iretd
;
Int1bHandler    endp


;-------------------------------------------------------------------------
;
;Handle a ctrl-break key press by terminating this application cleanly.
;
Int23Handler    proc    near
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        sti
        jmp     [TerminationHandler]
Int23Handler    endp

        assume ds:nothing

;-------------------------------------------------------------------------
Int24Handler    proc    near
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        push    gs
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        ;
api99_0:
        mov     edx,offset CriticalPrompt
        mov     ah,9
        int     21h
        mov     ah,1
        int     21h
        mov     edi,offset CriticalKeys
        mov     ecx,sizeof CriticalKeys
        cld
        push    ds
        pop     es
        repne   scasb
        jnz     api99_0
        movzx   eax,b[edi+7]
        cmp     eax,2
        jz      api99_Terminate
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
        test    BYTE PTR cs:apiSystemFlags,1
        jz      api99_Use32_2
        iret
api99_Use32_2:
        iretd
        ;
api99_Terminate:
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        push    ds
        push    ds
        push    ds
        pop     gs
        pop     fs
        pop     es
        sti
        jmp     [TerminationHandler]
;
Int24Handler    endp

        assume ds:nothing

ifndef NOEXPORTS

_DATA32 segment
;
;Export table for base PSP. EXPORT's internal functions.
;
apiEXPORT_cwMainCode equ 0
apiEXPORT_cwMainData equ 1
apiEXPORT_apiCode    equ 2
apiEXPORT_apiData    equ 3

	align 4

apiExports      dd NUMEXPORTS
;
        dd offset apiExportName
;
apiStartExports label dword
        dd offset apiExport01
        dd offset apiExport02
        dd offset apiExport03
        dd offset apiExport04
        dd offset apiExport05
        dd offset apiExport06
        dd offset apiExport07
        dd offset apiExport08
        dd offset apiExport09
        dd offset apiExport10
NUMEXPORTS equ ($ - offset apiStartExports) / 4
;
apiExportName   db 15,"CAUSEWAY_KERNAL"
;

EXPORTSTRUC struct
dwOfs	dd ?
wSeg	dw ?
bLen	db ?
EXPORTSTRUC ends

@Export macro a,b,c
local name_
	EXPORTSTRUC <offset a,b,sizeof name_>
name_ db c
endm

apiExport01 label EXPORTSTRUC
	@Export __cwAPI_CreatePSP,     apiEXPORT_apiCode, "__CWAPI_CREATEPSP"
apiExport02 label EXPORTSTRUC
	@Export __cwAPI_DeletePSP,     apiEXPORT_apiCode, "__CWAPI_DELETEPSP"
apiExport03 label EXPORTSTRUC
	@Export __cwAPI_ExecModule,    apiEXPORT_apiCode, "__CWAPI_EXECMODULE"
apiExport04 label EXPORTSTRUC
	@Export __cwAPI_DSizeSelector, apiEXPORT_apiCode, "__CWAPI_DSIZESELECTOR"
apiExport05 label EXPORTSTRUC
	@Export __cwAPI_FindModule,    apiEXPORT_apiCode, "__CWAPI_FINDMODULE"
apiExport06 label EXPORTSTRUC
    @Export __cwAPI_UnFindModule,  apiEXPORT_apiCode, "__CWAPI_UNFINDMODULE"
apiExport07 label EXPORTSTRUC
	@Export __cwAPI_FindFunction,  apiEXPORT_apiCode, "__CWAPI_FINDFUNCTION"
apiExport08 label EXPORTSTRUC
	@Export __cwAPI_GetMemLinear32,apiEXPORT_apiCode, "__CWAPI_GETMEMLINEAR32"
apiExport09 label EXPORTSTRUC
	@Export __cwAPI_ResMemLinear32,apiEXPORT_apiCode, "__CWAPI_RESMEMLINEAR32"
apiExport10 label EXPORTSTRUC
	@Export __cwAPI_RelMemLinear32,apiEXPORT_apiCode, "__CWAPI_RELMEMLINEAR32"
_DATA32 ends

__cwAPI_CreatePSP proc far
        call    CreatePSP
        ret
__cwAPI_CreatePSP endp

__cwAPI_DeletePSP proc far
        call    DeletePSP
        ret
__cwAPI_DeletePSP endp

__cwAPI_ExecModule proc far
        call    ExecModule
        ret
__cwAPI_ExecModule endp

__cwAPI_DSizeSelector proc far
        call    _DSizeSelector
        ret
__cwAPI_DSizeSelector endp

__cwAPI_FindModule proc far
        call    FindModule
        ret
__cwAPI_FindModule endp

__cwAPI_UnFindModule proc far
        call    UnFindModule
        ret
__cwAPI_UnFindModule endp

__cwAPI_FindFunction proc far
        call    FindFunction
        ret
__cwAPI_FindFunction endp

__cwAPI_GetMemLinear32 proc far
        call    mcbGetMemLinear32
        ret
__cwAPI_GetMemLinear32 endp

__cwAPI_ResMemLinear32 proc far
        call    mcbResizeMemLinear32
        ret
__cwAPI_ResMemLinear32 endp

__cwAPI_RelMemLinear32 proc far
        call    mcbRelMemLinear32
        ret
__cwAPI_RelMemLinear32 endp

endif

;-------------------------------------------------------------------------------
;
;Final init stuff.
;
cwOpen  proc    near

        @dprintf DOPT_EXEC,<"cwOpen enter, ss:esp=%X:%lX",10>,ss,esp
;        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        ;
        mov     d[TerminationHandler+0],offset cwClose
        mov     w[TerminationHandler+4],cs

;--- copy MainExec to a safe place
        mov     esi,offset MainExec
        mov     edi,offset DllNameSpace
        mov     es,cs:apiDSeg32
        mov     ecx,sizeof MainExec / 4
        rep     movsd

;
;Now we know the machines details, re-size the program
;block again to release as much memory as possible.
;
;        mov     edi,offset Int21Buffer
        mov     edi,offset PageInt
        push    ds
        pop     es
        mov     ax,RealPSPSegment
        mov     [edi].RealRegsStruc.Real_ES,ax
        mov     bx,TSREnd
if SMARTRMALLOC
        mov     RawStackPos, RawStackTotal ;increase size to normal.
@@:
 if 0 ; activate for debugging ( clears the stack space with -1 )
        mov cx, _cwInit
        mov dx, _cwMain
        sub cx, dx
        shl cx, 4   ; cx=offset for _cwInit in GROUP16
        push di
        push eax
        mov di, cx
        mov cx, RawStackTotal / 4
        or eax, -1
        rep stosd
        pop eax
        pop di
 endif
endif
        sub     bx,ax                   ;Size program.
        mov     [edi].RealRegsStruc.Real_BX,bx
        mov     [edi].RealRegsStruc.Real_AX,4a00h
        mov     bl,21h
        mov     ErrorNumber,1
        Sys     IntXX
        test    [edi].RealRegsStruc.Real_Flags,1
        jnz     cw1_9
        mov     ErrorNumber,0           ;clear error number.
;
;Force accurate memory values.
;
        or      ecx,-1
        Sys     GetMemLinear32
;
;Enable resource tracking and MCB allocations.
;
        or      ResourceTracking,-1     ;Enable resource tracking.
;
;Run the main program.
;
;        mov     edx,offset MainExec     ;DS:EDX -> name to exec.
        mov     edx,offset DllNameSpace  ;DS:EDX -> name to exec.
        mov     esi,80h                 ;ES:ESI -> cmdline
        mov     es,PSPSegment
        push    cs
        pop     ds
        xor     ecx,ecx                 ;CX = environment
        Sys     cwExec                  ;run the bugger (executes cwAPI_exec).
        mov     ds,cs:apiDSeg16
        jnc     cw1_8
        add     ax,10-1                 ;convert error number.
        mov     ErrorNumber,ax
        jmp     cw1_9
cw1_8:
        cmp     DebugDump,0
        jnz     cw1_9
        mov     ErrorLevel,ax           ;store programs error level.
        mov     ErrorNumber,0           ;clear error number.
cw1_9:
        jmp     cwClose
cwOpen  endp


;-------------------------------------------------------------------------------
;
;Shut everything down.
;
cwClose proc    near

        @dprintf DOPT_EXEC,<"cwClose enter, ss:esp=%X:%lX",10>,ss,esp
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        ;
        mov     ResourceTracking,0
        mov     mcbAllocations,0
        sti

;--- clearing CS of termination handler should NOT be done before DebugDisplay;
;--- an exception may occur when contents of CS:EIP or SS:ESP are displayed.

;        mov     w[TerminationHandler+4],0
        mov     d[TerminationHandler],offset behind_display
        call    DebugDisplay
behind_display:
;
;Remove extension patches.
;
        mov     edi,offset ExtensionEnd - sizeof EXTENSION
cw2_p0:
        cmp     [edi].EXTENSION.wFlgs,-1  ;installed?
        jnz     cw2_p2
        push    edi
        call    [edi].EXTENSION.pExit
        pop     edi
cw2_p2:
        sub     edi,sizeof EXTENSION
        cmp     edi,offset ExtensionStart
        jae     cw2_p0
        @dprintf DOPT_EXEC,<"cwClose: extensions removed",10>
;
;Remove api exception patches.
;
        cmp     apiExcepPatched,0
        jz      cw2_pe0
        mov     grp32Ofs, offset UnPatchExc
        call    [grp32Proc]
cw2_pe0:
        @dprintf DOPT_EXEC,<"cwClose: API removed",10>
;
;Remove the API patch.
;
        mov     es,Group32DS
        assume es:GROUP32
        mov     edx,DWORD PTR [OldInt31+0]
        mov     cx,WORD PTR [OldInt31+4]
        mov     bl,31h
        mov     ax,205h
        int     31h
        mov     DWORD PTR [cwIdentity+0],0
        mov     DWORD PTR [cwIdentity+4],0
;        assume es:nothing
        ;
cw2_noAPI:

; MED, 12/24/99, coalesce free memory by attempting to allocate largest possible
;  with upper memory in the chain
        mov     ax,5800h
        int     21h
        push    ax
        mov     ax,5802h
        int     21h
        push    ax
        mov     bx,1
        mov     ax,5803h
        int     21h
        mov     bx,81h
        mov     ax,5801h
        int     21h

        mov     ah,48h
        mov     bx,-1
        int     21h

        pop     bx
        mov     ax,5803h
        int     21h
        pop     bx
        mov     ax,5801h
        int     21h


        cmp     ProtectedType,PT_DPMI
        jz      cw2_DPMI
;
;Make RAW stuff addressable.
;
        cli                             ;Don't want interrupts interfering.
;        mov     ax,KernalDS             ;Get supervisor data descriptor,
;        mov     ds,ax                   ;DS,ES,FS,GS,SS must be data with 64k limit
;        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax
;
;Switch to RAW exit code.
;
        push    offset cw2_InRealMode
        db 66h,0eah    ;jmp far16 ptr []
        dw offset RawVCPIRealMode, KernalCS
;
;Remove DPMI stuff.
;
cw2_DPMI:
if 0
        cmp     [OldInt21hExec],0
        jz      cw2_d0
        mov     bl,21h
        mov     dx,w[OldInt21hExec+0]
        mov     cx,w[OldInt21hExec+2]
        mov     ax,201h
        int     31h
cw2_d0:
endif
        mov     al,b ErrorNumber
        mov     ah,4ch
        int     21h

_cwMain segment
cw2_InRealMode:
        mov     ax,ErrorNumber
        or      ax,ax
        jnz     cw2_Exit
        mov     ax,ErrorLevel
cw2_Exit:
        mov     ah,4ch
        int     21h
_cwMain ends

        assume ds:nothing
        assume es:nothing

cwClose endp

