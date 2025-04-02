;
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;

ifndef COPYFCBS
COPYFCBS equ 0	;1=copy fcbs in int 21h, ax=4b00h emulation
endif
TRANSLATEREMAPFUNC equ 1	;1=translate function address returned by int 21h, ah=38h

;--- macros
;

Int21h_repmovs  macro
        push    ecx
        shr     ecx,2
        rep     movsd           ;Copy the data.
        pop     ecx
        and     ecx,3
        rep     movsb
endm

;--- clear (hiword) of p1 parameter if DOS/4G mode is on

DOS4GExtend     macro p1
        local __0
        cmp     cs:Int21hDOS4GFlag,0
        jz      __0
        mov     p1,0
__0:
endm

CTRYINFO struct
wDateFmt	dw ?
szCurrency  db 5 dup (?)
szTSep      db 2 dup (?)
szDecSep    db 2 dup (?)
szDateSep   db 2 dup (?)
szTimeSep   db 2 dup (?)
bCurrFmt    db ?	;currency format
bNumDgt     db ?	;number of digits after decimal in currency
bTimeFmt    db ?
pfCaseMap   dd ?	;FAR16 case map routine
szSepDL     db 2 dup (?)
            db 10 dup (?)  ;reserved
CTRYINFO ends

_DATA32 segment

	align 4

OldInt21h       df 0
	align 4
Int21Buffer      RealRegsStruc <>
Int21hCountryTab RealRegsStruc <>
DOS16DummySegment       DD      0
Int21hDOS4GFlag db 0

	align 4
;------------------------------------------------------------------------------
;
;The jump table used to pass control to the right routine.
;
Int21hTable     label dword
        DD 08h - ( $ - Int21hTable ) / 4 dup (0)
        dd Int21hNotOurs        ;08
        dd Int21hPrintString    ;09
        dd Int21hGetString      ;0A
        DD 0Fh - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hInvalid        ;0F
        DD 18h - ( $ - Int21hTable ) / 4 dup (Int21hInvalid)
        DD 1Ah - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hSetDTA         ;1A
        DD 20h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hNotOurs        ;20
        DD 25h - ( $ - Int21hTable ) / 4 dup (Int21hInvalid)

        dd Int21hSetVect        ;25
        DD 2Ah - ( $ - Int21hTable ) / 4 dup (Int21hInvalid)
        DD 2Fh - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hGetDTA         ;2F
        DD 35h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hGetVect        ;35
        DD 38h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hGetSetCountry  ;38
        dd Int21hCreateDIR      ;39
        dd Int21hCreateDIR      ;3A
        dd Int21hCreateDIR      ;3B
        dd Int21hCreateFile     ;3C
        dd Int21hOpenFile       ;3D
        dd Int21hNotOurs        ;3E
        dd Int21hReadFile       ;3F

        dd Int21hWriteFile      ;40
        dd Int21hOpenFile       ;41
        dd Int21hNotOurs        ;42
        dd Int21hCreateFile     ;43
        dd Int21hIOCTLDispatch  ;44
        DD 47h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hGetCurDir      ;47

        dd Int21hAllocMem       ;48
        dd Int21hRelMem         ;49
        dd Int21hResizeMem      ;4A
        dd Int21hExecFile       ;4B
        DD 4Eh - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hFindFirstFile  ;4E
        dd Int21hFindNextFile   ;4F
        dd Int21hSetPSP         ;50
        dd Int21hGetPSP         ;51
        DD 56h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hRenameFile     ;56
        dd Int21hNotOurs        ;57

        dd Int21hNotOurs        ;58
        dd Int21hGetErrorInfo   ;59
        dd Int21hCreateTemp     ;5A
        dd Int21hCreateFile     ;5B
        DD 5Eh - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hMSNet          ;5E
        dd Int21hNotOurs        ;5F
        DD 62h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hGetPSP         ;62
        DD 67h - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hSetHandles     ;67
        DD 6Ch - ( $ - Int21hTable ) / 4 dup (0)

        dd Int21hExtendOpen     ;6C
        DD 70h  - ( $ - Int21hTable ) / 4 dup (0)
_DATA32 ends

;------------------------------------------------------------------------------
;
;DTA size for find first/next seems to be a little volatile so I'll use an EQU
;here incase it needs to change again.
;
DTASize equ     44-1

        assume ds:nothing, es:nothing

;------------------------------------------------------------------------------
;
;The DOS function control code.
;
;All registers are stacked and EBP is setup to give access to the stacked
;values. Carry is cleared, interrupts are enabled if they we're enabled at
;entry. FS is set to the current PSP. Then the function specific handler is
;called.
;
;All this default setup means some handlers have unecesary processing overhead
;but the majority need it and it saves space.
;
;NOTES:
;
;None of the FCB related functions are modified.
;Media ID byte pointer of Get drive default data not yet implemented.
;
Int21h  proc    near

        push    ds
        push    es
        push    fs
        push    gs
        pushad          ;/
        mov     ebp,esp
        movzx   eax,ah          ;Need extended register version.
        cmp     al,-1
        jz      Int21hDOS4GTest
        cmp     al,70h
        jae     Int21hNotOurs
        cmp     cs:[Int21hTable+eax*4],0
        jz      Int21hNotOurs
        mov     esi,IFRM.i32._fl
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT ;/
        jz      int211_32Bit0           ;/
        movzx   ebp,bp          ;/
        mov     esi,IFRM.i16._fl
int211_32Bit0:
        add     esi,ebp
        and     BYTE PTR ss:[esi],not 1
        cld                     ;Default direction.
if 0
        test    WORD PTR ss:[esi],1 shl 9       ;Were interrupts enabled?
        jz      int211_NoInts
        sti                     ;Turn interrupts back on.
int211_NoInts:
endif
        mov     fs,cs:apiDSeg16
        assume fs:GROUP16
        mov     fs,fs:PSPSegment        ;Point to PSP.
        assume fs:nothing
        call    cs:[Int21hTable+eax*4]        ;Pass control to handler.
int21_backtocaller::
        popad
        pop     gs
        pop     fs
        pop     es
        pop     ds
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT
        jz      int211_32Bit1
        iret
int211_32Bit1:
        iretd                   ;Return to caller.
Int21h  endp

;------------------------------------------------------------------------------

Int21hInvalid   proc    near
;       jmp     Int21hNotOurs
Int21hInvalid   endp ; fall thru
;------------------------------------------------------------------------------
;
;Handler for functions that need to be passed to old DOS interrupt.
;
Int21hNotOurs   proc    near
        mov     esp,ebp
        popad                   ;Restore registers.
        pop     gs
        pop     fs
        pop     es
        pop     ds
        jmp     cs:[OldInt21h]  ;pass it onto previous handler.
Int21hNotOurs   endp

;------------------------------------------------------------------------------
;
;Function FF, DOS4G detection.
;
Int21hDOS4GTest proc near
;
;This check only allowed for 32-bit programs which is what DOS4G uses.
;
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT
        jnz     Int21hNotOurs
;
;Check register setup.
;
        cmp     [ebp].IFRM._AL,0    ;AX=0FF00h
        jnz     Int21hNotOurs
        cmp     [ebp].IFRM._DX,78h  ;DX=0078h
        jnz     Int21hNotOurs

        push    offset int21_backtocaller

; changed for dummy segment modification
;       mov     [ebp].IFRM._GS,ds
        mov     [ebp].IFRM._EAX,4734ffffh
;
;Flag DOS4G mode for INT handling.
;
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32

; give Watcom a dummy segment for it to stuff silly __D16SegInfo
; values in without trashing things, MED 12/12/95
        mov     eax,DOS16DummySegment
        test    eax,eax
        jne     setgs

; MED 07/15/97
DUMMYSEGMENTSIZE        EQU     48h     ; must be multiple of four
;       mov     ecx,44h                 ; dummy segment size
        mov     ecx,DUMMYSEGMENTSIZE
        Sys     GetMem32                ; allocate the memory
        jnc     zerodummy               ; no error allocating memory
        mov     ebx,[ebp].IFRM._DS
        jmp     savedummy

; MED 07/15/97, zero out dummy segment
zerodummy:
        push    ds
        mov     ds,ebx
        mov     ecx,DUMMYSEGMENTSIZE-4
dumloop:
        mov     DWORD PTR ds:[ecx],0
        sub     ecx,4
        jns     dumloop
        pop     ds
savedummy:
        mov     DOS16DummySegment,ebx
        mov     eax,ebx                 ; allocated memory or application DS selector to put in GS
setgs:
        mov     [ebp].IFRM._GS,eax
        mov     Int21hDOS4GFlag,-1
        ret
        assume ds:nothing
Int21hDOS4GTest endp


;------------------------------------------------------------------------------
;
;Function 09h emulation.
;
Int21hPrintString proc near
        mov     esi,[ebp].IFRM._EDX       ;Point to source data.
        mov     ds,[ebp].IFRM._DS
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        mov     al,"$"          ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        mov     edx,ecx
;
;Now copy EPSP_Struc.EPSP_TransSize-1 sized chunks into the transfer buffer
;and pass to the real mode handler.
;
int214_0:
        mov     ecx,edx         ;Get current count.
        cmp     ecx,fs:[EPSP_Struc.EPSP_TransSize]
        jc      int214_1
        mov     ecx,fs:[EPSP_Struc.EPSP_TransSize]      ;Use transfer buffer size -1.
        dec     ecx
int214_1:
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]       ;Point to transfer buffer.
        xor     edi,edi
        sub     edx,ecx         ;Update total counter.
        Int21h_repmovs          ;Copy this data.
        mov     BYTE PTR es:[edi],"$"   ;Terminate this string.
        ;
        ;Call the real mode handler to deal with this chunk.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],0900h
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        ;
        ;Keep looping till all data is done.
        ;
        or      edx,edx
        jnz     int214_0
        ;
        ;All done so return to caller.
        ;
        ret
Int21hPrintString endp


;------------------------------------------------------------------------------
;
;Function 0A emulation.
;
Int21hGetString proc near
        mov     esi,[ebp].IFRM._EDX       ;Point to source data.
        mov     ds,[ebp].IFRM._DS
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        mov     al,[esi]                ;Get length byte.
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]       ;Point to transfer buffer.
        mov     BYTE PTR es:[0],al              ;Store length byte.
        ;
        ;Call the real mode handler to read the string.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],0A00h
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        ;
        ;Copy returned string back into callers buffer.
        ;
        mov     edi,[ebp].IFRM._EDX       ;Point to destination buffer.
        mov     es,[ebp].IFRM._DS
        call    Int21hExtend_ES_EDI     ;Extend [E]DI.
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        xor     esi,esi         ;Point to source data.
        movzx   ecx,b[esi+1]            ;get length of string read.
        add     ecx,1+1+1               ;max+len+eol.
        Int21h_repmovs          ;Copy this data.
        ;
        ;All done so return to caller.
        ;
        ret
Int21hGetString endp


;------------------------------------------------------------------------------
;
;Function 1A emulation.
;
Int21hSetDTA    proc    near
        mov     esi,[ebp].IFRM._EDX       ;Point to source data.
        mov     ds,[ebp].IFRM._DS
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        mov     DWORD PTR fs:[EPSP_Struc.EPSP_DTA+0],esi
        mov     WORD PTR fs:[EPSP_Struc.EPSP_DTA+4],ds  ;Store new DTA address.
        ret
Int21hSetDTA    endp


;------------------------------------------------------------------------------
;
;Function 25 emulation.
;
Int21hSetVect   proc    near
        mov     edx,[ebp].IFRM._EDX
        mov     cx,w[ebp].IFRM._DS
        mov     bl,[ebp].IFRM._AL
        Sys     SetVect
        ret
Int21hSetVect   endp


;------------------------------------------------------------------------------
;
;Function 2F emulation.
;
Int21hGetDTA    proc    near
        mov     eax,DWORD PTR fs:[EPSP_Struc.EPSP_DTA+0]  ;Copy current DTA address into
        mov     [ebp].IFRM._EBX,eax       ;return register storage.
        mov     ax,WORD PTR fs:[EPSP_Struc.EPSP_DTA+4]
        mov     w[ebp].IFRM._ES,ax
        ret
Int21hGetDTA    endp


;------------------------------------------------------------------------------
;
;Function 35 emulation.
;
Int21hGetVect   proc    near
        mov     bl,[ebp].IFRM._AL
        xor     edx,edx
        Sys     GetVect
        mov     w[ebp].IFRM._ES,cx
        mov     [ebp].IFRM._EBX,edx
        ret
Int21hGetVect   endp


;------------------------------------------------------------------------------
;
;Function 38 emulation.
;
Int21hGetSetCountry proc near
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT ;16 or 32-bit -1 check?
        jz      int2110_32Bit0
        db 66h                     ;change "cmp edx,-1" to "cmp dx,-1"
int2110_32Bit0:
        cmp     edx,-1
        jz      Int21hNotOurs   ;Go through normal stuff for SET.
        ;
        ;Call the real mode handler to take care of things.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._EBX
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     eax,es:RealRegsStruc.Real_EBX[edi]
        mov     [ebp].IFRM._BX,ax
        DOS4GExtend w[ebp].IFRM._EBX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry status in stack copy of flags
        or      al,al
        jnz     int2110_9
        ;
        ;Copy returned info to callers buffer.
        ;
        mov     edi,[ebp].IFRM._EDX       ;Point to source data.
        mov     es,[ebp].IFRM._DS
        call    Int21hExtend_ES_EDI     ;Extend [E]DI.
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        xor     esi,esi
if TRANSLATEREMAPFUNC
        ;v5.0: this translation didn't work previously due to a bug.
        ;      DOS/4GW translates the remap function address, so CauseWay
        ;      should do as well.
        mov     eax,cs
        shl     eax,16
        mov     ax,lowword offset int2110_RemapCall
        xchg    eax,[esi].CTRYINFO.pfCaseMap
else
        mov     eax,[esi].CTRYINFO.pfCaseMap
endif
        mov     ecx,sizeof CTRYINFO
        cld
        rep     movsb
if TRANSLATEREMAPFUNC
        ;
        ;store real-mode re-map function address in RMCS for country info.
        ;
        mov     es,cs:apiDSeg32
        mov     es:Int21hCountryTab.Real_CSIP,eax
endif
        ;
int2110_9:
        ret

if TRANSLATEREMAPFUNC
;
;The remap handler - a FAR16 proc! Was never called prior to v5.0!
;
int2110_RemapCall:
        push    edi
        push    es
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21hCountryTab
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        push    es:RealRegsStruc.Real_CS[edi]
        push    es:RealRegsStruc.Real_IP[edi]
        Sys     FarCallReal
        pop     es:RealRegsStruc.Real_IP[edi]
        pop     es:RealRegsStruc.Real_CS[edi]
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        pop     es
        pop     edi
        retw
endif
Int21hGetSetCountry endp


;------------------------------------------------------------------------------
;
;Function 39/3A/3B (MKDIR/RMDIR/CHDIR) emulation.
;
Int21hCreateDIR proc near
        ;
        ;Copy string into transfer buffer.
        ;
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        inc     ecx             ;Include terminator.
        rep     movsb
        ;
        ;Pass control to real mode handler.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;Get return code.
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        ret
Int21hCreateDIR endp


;------------------------------------------------------------------------------
;
;Function 3C emulation.
;
Int21hCreateFile proc near
        ;
        ;Copy string into transfer buffer.
        ;
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        inc     ecx             ;Include terminator.
        rep     movsb
        ;
        ;Pass control to real mode handler.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._ECX
        mov     es:RealRegsStruc.Real_ECX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;Get return code.
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     eax,es:RealRegsStruc.Real_ECX[edi]
        mov     [ebp].IFRM._CX,ax
        DOS4GExtend w[ebp].IFRM._ECX+2
        ret
Int21hCreateFile endp


;------------------------------------------------------------------------------
;
;Function 3D emulation.
;
Int21hOpenFile  proc    near
        ;
        ;Copy string into transfer buffer.
        ;
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        inc     ecx             ;Include terminator.
        rep     movsb
        ;
        ;Pass control to real mode handler.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;Get return code.
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        ret
Int21hOpenFile  endp


;------------------------------------------------------------------------------
;
;Function 3F emulation.
;DS:[E]DX -> buffer
;CX = length; ECX = length if DOS/4G mode
;
Int21hReadFile  proc    near
        mov     ds,[ebp].IFRM._DS ;Point to source.
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     edx,[ebp].IFRM._ECX     ;Get length.
        cmp     cs:Int21hDOS4GFlag,0
        jnz     @F
        movzx   edx,dx
@@:
        xor     ebx,ebx         ;Reset length read.
int2114_0:
        mov     ecx,edx
        cmp     ecx,fs:[EPSP_Struc.EPSP_TransSize]
        jc      int2114_1
        mov     ecx,fs:[EPSP_Struc.EPSP_TransSize]
int2114_1:
        mov     es:RealRegsStruc.Real_ECX[edi],ecx      ;Store length.
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     ax,[ebp].IFRM._AX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     ax,[ebp].IFRM._BX ;Set handle.
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        push    ebx
        mov     bl,21h
        Sys     IntXX
        pop     ebx
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        or      al,al           ;Carry set?
        jz      int2114_2
        mov     ebx,es:RealRegsStruc.Real_EAX[edi]      ;get return code.
        jmp     int2114_3
int2114_2:
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get bytes read.
        movzx   eax,ax
        sub     edx,eax         ;Update count remaining.
        add     ebx,eax         ;Update count so far.
        push    ecx
        mov     ecx,eax
        push    edi
        push    es
        push    esi
        push    ds
        pop     es
        pop     edi
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        xor     esi,esi
        Int21h_repmovs          ;Copy this data.
        push    edi
        push    es
        pop     ds
        pop     esi
        pop     es
        pop     edi
        pop     ecx
        cmp     eax,ecx
        jnz     int2114_3
        or      edx,edx
        jnz     int2114_0
int2114_3:
        mov     [ebp].IFRM._AX,bx ;store length or return code.
        cmp     cs:Int21hDOS4GFlag,0
        jz      int2114_5
        mov     [ebp].IFRM._EAX,ebx
int2114_5:
        ret
Int21hReadFile  endp


;------------------------------------------------------------------------------
;
;Function 40 emulation.
;DS:[E]DX -> buffer
;CX = length; ECX = length if DOS/4G mode
;
Int21hWriteFile proc near
        mov     ds,[ebp].IFRM._DS ;Point to source.
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     edx,[ebp].IFRM._ECX     ;Get length.
        cmp     cs:Int21hDOS4GFlag,0
        jnz     @F
        movzx   edx,dx
@@:
        xor     ebx,ebx         ;Reset length read.
int2115_0:
        mov     ecx,edx
        cmp     ecx,fs:[EPSP_Struc.EPSP_TransSize]
        jc      int2115_1
        mov     ecx,fs:[EPSP_Struc.EPSP_TransSize]
int2115_1:
        mov     es:RealRegsStruc.Real_ECX[edi],ecx      ;Store length.
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     ax,[ebp].IFRM._AX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     ax,[ebp].IFRM._BX ;Set handle.
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        push    ecx
        push    edi
        push    es
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        Int21h_repmovs          ;Copy this data.
        pop     es
        pop     edi
        pop     ecx
        push    ebx
        mov     bl,21h
        Sys     IntXX           ;Do the write.
        pop     ebx
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        or      al,al           ;Carry set?
        jz      int2115_2
        mov     ebx,es:RealRegsStruc.Real_EAX[edi]      ;get return code.
        jmp     int2115_3
int2115_2:
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get bytes read.
        movzx   eax,ax
        sub     edx,eax         ;Update count remaining.
        add     ebx,eax         ;Update count so far.
        cmp     eax,ecx         ;Get anything?
        jnz     int2115_3
        or      edx,edx
        jnz     int2115_0
int2115_3:
        mov     [ebp].IFRM._AX,bx ;store length or return code.
        cmp     cs:Int21hDOS4GFlag,0
        jz      int2115_5
        mov     [ebp].IFRM._EAX,ebx
int2115_5:
        ret
Int21hWriteFile endp


;------------------------------------------------------------------------------
;
;Function 44 emulation.
;
Int21hIOCTLDispatch proc near
        mov     al,[ebp].IFRM._AL
        cmp     al,02h          ;IOCTL?
        jz      Int21hReadFile
        cmp     al,03h
        jz      Int21hWriteFile
        cmp     al,04h
        jz      Int21hReadFile
        cmp     al,05h
        jz      Int21hWriteFile
        jmp     Int21hNotOurs
Int21hIOCTLDispatch endp


;------------------------------------------------------------------------------
;
;Function 47 emulation.
;
Int21hGetCurDir proc near
        ;
        ;Call the real mode handler to take care of things.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._EDX
        mov     es:RealRegsStruc.Real_EDX[edi],eax
        mov     w es:RealRegsStruc.Real_ESI[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        or      al,al
        jnz     int2117_9
        ;
        ;Copy returned info to callers buffer.
        ;
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]       ;Point to source data.
        xor     esi,esi
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        inc     ecx
        mov     es,[ebp].IFRM._DS
        mov     edi,[ebp].IFRM._ESI
        call    Int21hExtend_ES_EDI     ;Extend [E]SI.
        rep     movsb
int2117_9:
        ret
Int21hGetCurDir endp


;------------------------------------------------------------------------------
;
;Function 48 emulation.
;non-DOS/4G: BX: size in paragraphs
;DOS/4G:    EBX: size in paragraphs
;this behavior is new for v5.0 - previously, it was always BX, even in DOS/4G mode.
;
Int21hAllocMem  proc    near
        cmp     cs:Int21hDOS4GFlag,0
        jnz     int2118_1
        cmp     bx,-1           ;maximum free check?
        jz      int2118_AllocMuch
        movzx   ebx,bx
        jmp     int2118_2
int2118_1:
        test    ebx,0f0000000h
        jnz     int2118_AllocMuch
int2118_2:
        shl     ebx,4           ;convert paragraphs to bytes.
        mov     dx,bx
        shr     ebx,16
        mov     cx,bx
        Sys     GetMem          ;try to allocate memory.
        jc      int2118_AllocMuch               ;report how much free then.
        mov     [ebp].IFRM._AX,bx ;get the selector allocated.
        DOS4GExtend w[ebp].IFRM._EAX+2
        ret
        ;
int2118_AllocMuch:
        mov     cx,-1
        mov     dx,-1
        Sys     GetMem          ;get free memory size.
        mov     bx,cx
        shl     ebx,16
        mov     bx,dx
        shr     ebx,4           ;convert to paragraphs
        cmp     cs:Int21hDOS4GFlag,0
        jnz     int2118_isdos4g
        cmp     ebx,0fffeh      ;can't report too much.
        jc      @F
        mov     ebx,0fffeh
@@:
        mov     [ebp].IFRM._BX,bx
        mov     [ebp].IFRM._AX,8
        jmp     int2118_done
int2118_isdos4g:
        mov     [ebp].IFRM._EBX,ebx
        mov     [ebp].IFRM._EAX,8
int2118_done:
        mov     al,1
        call    Int21hAL2Carry  ;Set carry.
        ret
Int21hAllocMem  endp


;------------------------------------------------------------------------------
;
;Function 49 emulation.
;
Int21hRelMem    proc    near
        mov     ebx,[ebp].IFRM._ES
;--- segment registers ds,es, gs aren't modified
;        mov     ds,[ebp].IFRM._DS
;        mov     es,[ebp].IFRM._ES
        mov     fs,[ebp].IFRM._FS
;        mov     gs,[ebp].IFRM._GS
        Sys     RelMem
        mov     [ebp].IFRM._DS,ds
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._FS,fs
        mov     [ebp].IFRM._GS,gs
        ret
Int21hRelMem    endp


;------------------------------------------------------------------------------
;
;Function 4A emulation.
;            ES: selector of memory block
;non-DOS/4G: BX: new size in paragraphs
;DOS/4G:    EBX: new size in paragraphs
;
;
Int21hResizeMem    proc    near
        mov     ebx,[ebp].IFRM._EBX
        cmp     cs:Int21hDOS4GFlag,0
        jnz     @F
        movzx   ebx,bx
@@:
        mov     al,1
        test    ebx,0f0000000h
        jnz     int2119_9
        shl     ebx,4           ;convert paragraphs to bytes.
        mov     dx,bx
        shr     ebx,16
        mov     cx,bx
        mov     ebx,[ebp].IFRM._ES
        Sys     ResMem          ;bx=selector, CX:DX=new size
        pushfd
        pop     eax
int2119_9:
        call    Int21hAL2Carry  ;Set carry.
        ret
Int21hResizeMem    endp


;------------------------------------------------------------------------------
;
;Function 4B emulation.
;
Int21hExecFile  proc    near
        cmp     [ebp].IFRM._AL,0 ;We only support sub-function 0.
        jz      int2121_OK
;int2121_Done:
        mov     al,1
        call    Int21hAL2Carry  ;Set carry.
        ret
        ;
int2121_OK:
        ;Copy the file name into transfer buffer.
        ;
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI
        xor     al,al
        call    Int21hStringLen ;Get length of this string
        inc     ecx
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        rep     movsb           ;Copy the file name.
        ;
        ;Copy the command line into transfer buffer.
        ;
        mov     edi,[ebp].IFRM._EBX
        mov     es,[ebp].IFRM._ES
        call    Int21hExtend_ES_EDI     ;Extend [E]DI
        mov     ebx,edi
        push    es
        pop     ds
        cmp     cs:Int21hDOS4GFlag,0
        jz      int2121_0
        lds     esi,fword ptr [ebx+4+2] ; load cmdline into ds:esi
        jmp     int2121_Ef4
int2121_0:
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT
        jz      int2121_Ef3
        xor     esi,esi
        db 66h                          ;prefix 66h changes next instr to LDS SI,dword ptr [ebx+2]
int2121_Ef3:
        lds     esi,fword ptr [ebx+2]   ;Get command line offset & segment.
int2121_Ef4:
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        mov     edi,256
        movzx   ecx,b[esi]              ;get command line length.
        inc     ecx                     ;include length byte.
        rep     movsb
        mov al,0dh
        stosb
        mov al,0
        stosb

if COPYFCBS
        ;
        ;Copy the FCB's - check for DOS/4G added
        ;
        push    ebx
        mov     ds,[ebp].IFRM._ES
        cmp     cs:Int21hDOS4GFlag,0
        jz      int2121_Ef5
        add     ebx,4        ; skip ENV offset
        jmp     int2121_Ef6
int2121_Ef5:
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT
        jz      int2121_Ef6
        xor     eax,eax
        mov     ax,[ebx+(2)+(2+2)+(2+2)+0] ;Get FCB 2 offset
        mov     dx,[ebx+(2)+(2+2)+(2+2)+2] ;Get FCB 2 segment
        xor     esi,esi
        lds     si,[ebx+(2)+(2+2)]  ;Get FCB 1 offset & segment
        jmp     int2121_Ef7
int2121_Ef6:
        mov     eax,[ebx+(2)+(4+2)+(4+2)+0] ;Get FCB 2 offset
        mov     dx, [ebx+(2)+(4+2)+(4+2)+4] ;Get FCB 2 segment
        lds     esi,[ebx+(2)+(4+2)] ;Get FCB 1 offset & segment
int2121_Ef7:
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        mov     di,512
        movsd          ;copy FCB1 - 16 bytes
        movsd
        movsd
        movsd
        ;
        mov     esi,eax
        mov     ds, edx
        movsd          ;copy FCB2 - 16 bytes
        movsd
        movsd
        movsd
        pop     ebx
endif
        ;
        ;Sort out the environment.
        ;
        mov     es,[ebp].IFRM._ES
        xor     esi,esi
        cmp     cs:Int21hDOS4GFlag,0
        jz      int2121_1
        cmp     WORD PTR es:[ebx+4],0
        jz      int2121_2
        lds     esi,es:[ebx+0]
        jmp     int2121_Ef12
int2121_1:
        cmp     WORD PTR es:[ebx+0],0   ;got an environment?
        jnz     int2121_Ef10
int2121_2:
        mov     ds,fs:[PSP_Struc.PSP_Environment]      ;Get current environment.
        jmp     int2121_Ef12
int2121_Ef10:
        mov     ds,es:[ebx+0]           ;get environment segment.
int2121_Ef12:
        push    esi
int2121_3:
        lodsb
        or      al,al
        jnz     int2121_3
        cmp     b[esi],0                ;double zero?
        jnz     int2121_3
        inc     esi
        inc     esi
        pop     eax
        push    eax
        sub     esi,eax
        mov     ecx,esi
        add     esi,256         ;make space for execution path.
        add     esi,15
        shr     esi,4           ;get paragraphs needed.
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_EBX[edi],esi
        mov     es:RealRegsStruc.Real_AX[edi],4800h
        pop     esi
        mov     bl,21h
        Sys     IntXX           ;allocate this memory.
        mov     eax,es:RealRegsStruc.Real_EAX[edi]      ;get segment address.
        test    es:RealRegsStruc.Real_FlagsL[edi],1     ;alloc failed?
        mov     di,ax
        mov     al,1
        jnz     int2121_Ef13                            ;error
        movzx   edi,di
        push    edi
        shl     edi,4
        mov     es,cs:apiDSeg16
        assume es:GROUP16
        mov     es,es:RealSegment
        assume es:nothing
        rep     movsb           ;Copy environment.
        pop     edx             ;EDX=segment address of env. block
;
;Patch INT 21h exec function
;--- this should perhaps be changed - it's needed for DPMI mode only, if at all...
;--- in raw/vcpi mode, the Int 31h API cannot be called by external code, so there's
;--- no need to "deactivate" it.
;
IFNDEF NOI21RMHOOK
        pushad
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        cmp     NoEXECPatchFlag,0
        jne     medex1
        mov     bl,21h
        mov     ax,200h
        int     31h
        mov     w[OldInt21hExec+0],dx
        mov     w[OldInt21hExec+2],cx
        mov     dx,offset Int21hExecPatch
        mov     cx,GROUP16
        mov     bl,21h
        mov     ax,201h
        int     31h
medex1:
        popad
ENDIF
        ;
        ;Setup real mode parameter block.
        ;
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        assume ds:nothing
        mov     esi,512+32
        mov     w[esi+0],dx             ;set environment segment.
        mov     w[esi+2],256            ;command tail offset.
        mov     w[esi+4],ax             ;command tail segment.
        mov     w[esi+6],512            ;FCB 1 offset.
        mov     w[esi+8],ax             ;FCB 1 segment.
        mov     w[esi+10],512+16        ;FCB 2 offset.
        mov     w[esi+12],ax            ;FCB 2 segment.
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],4b00h
        mov     es:RealRegsStruc.Real_EBX[edi],esi
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     bl,21h
        Sys     IntXX
;
;Restore INT 21h patch.
;
IFNDEF NOI21RMHOOK
        pushad
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        cmp     NoEXECPatchFlag,0
        jne     medex2
        mov     bl,21h
        mov     dx,w[OldInt21hExec+0]
        mov     cx,w[OldInt21hExec+2]
        mov     [OldInt21hExec],0
        mov     ax,201h
        int     31h
medex2:
        assume ds:nothing
        popad
ENDIF
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        push    eax
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax ;store errorlevel.
        DOS4GExtend w[ebp].IFRM._EAX+2
        ;
        or      dx,dx
        jz      int2121_Ef15            ;No environment to release.
        mov     es:RealRegsStruc.Real_ES[edi],dx
        mov     es:RealRegsStruc.Real_AX[edi],4900h    ;release dos memory.
        mov     bl,21h
        Sys     IntXX
        ;
int2121_Ef15:
        ;Put the DTA back where it's supposed to be.
        ;
        mov     es:RealRegsStruc.Real_DX[edi],offset DtaBuffer
        mov     es:RealRegsStruc.Real_AX[edi],1a00h
        mov     es:RealRegsStruc.Real_DS[edi],GROUP16
        mov     bl,21h
        Sys     IntXX
        ;
        pop     eax
int2121_Ef13:
        call    Int21hAL2Carry
        ret
Int21hExecFile  endp


;------------------------------------------------------------------------------
;
;Function 4E emulation.
;
Int21hFindFirstFile proc near

        assume fs:_cwEnd

        mov     es,cs:apiDSeg16         ;point to new source.
        mov     edi,offset DtaBuffer    ;use the real DTA buffer.
        lds     esi,fs:[EPSP_Struc.EPSP_DTA]  ;point to user buffer.
        mov     ecx,DTASize             ;length to copy.
        rep     movsb                   ;copy data returned.

        mov     esi,[ebp].IFRM._EDX       ;Point to source data.
        mov     ds,[ebp].IFRM._DS
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al                   ;Character to look for.
        call    Int21hStringLen         ;Get length of this string.
        inc     ecx                     ;Include terminator.
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        rep     movsb
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._ECX
        mov     es:RealRegsStruc.Real_ECX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.

;       or      al,al
;       jnz     @@9

        mov     ds,cs:apiDSeg16        ;point to new source.
        mov     esi,offset DtaBuffer   ;use the real DTA buffer.
        les     edi,fs:[EPSP_Struc.EPSP_DTA]  ;point to user buffer.
        mov     ecx,DTASize            ;length to copy.
        rep     movsb                  ;copy data returned.
int2122_9:
        ret
Int21hFindFirstFile endp


;------------------------------------------------------------------------------
;
;Function 4F emulation.
;
Int21hFindNextFile proc near
        mov     es,cs:apiDSeg16        ;point to new source.
        mov     edi,offset DtaBuffer   ;use the real DTA buffer.
        lds     esi,fs:[EPSP_Struc.EPSP_DTA]  ;point to user buffer.
        mov     ecx,DTASize            ;length to copy.
        rep     movsb                  ;copy data returned.
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry         ;Set carry.
;       or      al,al
;       jnz     @@9
        mov     ds,cs:apiDSeg16        ;point to new source.
        mov     esi,offset DtaBuffer   ;use the real DTA buffer.
        les     edi,fs:[EPSP_Struc.EPSP_DTA]  ;point to user buffer.
        mov     ecx,DTASize            ;length to copy.
        rep     movsb                  ;copy data returned.
int2123_9:
        ret
Int21hFindNextFile endp


;------------------------------------------------------------------------------
;
;Function 50 emulation.
;
Int21hSetPSP    proc    near
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ax,[ebp].IFRM._BX
        mov     PSPSegment,ax   ;Set current PSP.
        assume ds:nothing
        ret
Int21hSetPSP    endp


;------------------------------------------------------------------------------
;
;Function 51 emulation.
;
Int21hGetPSP    proc    near
        mov     [ebp].IFRM._BX,fs
        ret
Int21hGetPSP    endp


;------------------------------------------------------------------------------
;
;Function 56 emulation.
;
Int21hRenameFile proc near
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        inc     ecx
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        push    ecx
        rep     movsb
        pop     edi
        ;
        mov     ds,[ebp].IFRM._ES
        mov     esi,[ebp].IFRM._EDI
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        inc     ecx
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        push    edi
        rep     movsb
        pop     edx
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     es:RealRegsStruc.Real_EDI[edi],edx
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     bl,21h
        Sys     IntXX

; return error status, MED 03/13/96
        mov     ax,es:RealRegsStruc.Real_AX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2

        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.

        ret
Int21hRenameFile endp


;------------------------------------------------------------------------------
;
;Function 5A emulation.
;
Int21hCreateTemp proc near
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._EDX
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        inc     ecx
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        rep     movsb
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._ECX
        mov     es:RealRegsStruc.Real_ECX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        or      al,al
        jnz     int2127_9
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        xor     esi,esi
        xor     al,al           ;Character to look for.
        call    Int21hStringLen ;Get length of this string.
        inc     ecx
        mov     es,[ebp].IFRM._DS
        mov     edi,[ebp].IFRM._EDX
        call    Int21hExtend_ES_EDI     ;Extend [E]SI.
        rep     movsb
int2127_9:
        ret
Int21hCreateTemp endp


;------------------------------------------------------------------------------
;
;Function 5E emulation.
;
Int21hMSNet     proc    near
        cmp     ax,5e00h
        jnz     Int21hNotOurs
        ;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     es:RealRegsStruc.Real_DX[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX           ;read pen values.
        mov     eax,es:RealRegsStruc.Real_ECX[edi]
        mov     [ebp].IFRM._CX,ax
        DOS4GExtend w[ebp].IFRM._ECX+2
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        or      al,al
        jnz     int2128_9
        mov     ds,fs:[EPSP_Struc.EPSP_TransProt]
        xor     esi,esi
        mov     es,[ebp].IFRM._ES
        mov     edi,[ebp].IFRM._EDX
        call    Int21hExtend_ES_EDI     ;Extend [E]SI.
        mov     ecx,16/4
        rep     movsd           ;copy the results.
int2128_9:
        ret
Int21hMSNet     endp


;------------------------------------------------------------------------------
;
;Function 67 emulation.
;
Int21hSetHandles proc near
;
;Set handle count with DOS.
;
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],6700h
        mov     ax,[ebp].IFRM._BX
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     bl,21h
        Sys     IntXX
        test    es:RealRegsStruc.Real_FlagsL[edi],1
        jz      int2129_0
        mov     al,1
        call    Int21hAL2Carry  ;Set carry.
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        ret
;
;Change succeded so modify handle table selector base.
;
int2129_0:
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        movzx   eax,RealPSPSegment
        shl     eax,4
        mov     ds,RealSegment
        assume ds:nothing
        movzx   edx,WORD PTR ds:[PSP_Struc.PSP_HandlePtr+2+eax]
        shl     edx,4
        movzx   ebx,WORD PTR ds:[PSP_Struc.PSP_HandlePtr+eax]
        add     edx,ebx
        movzx   ecx,WORD PTR ds:[PSP_Struc.PSP_Handles+eax]
        mov     bx,WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2]
        Sys     SetSelDet32
;
;Now modify all PSP handle counts.
;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     esi,BasePSPAddress
        mov     ds,RealSegment
        assume ds:nothing
        mov     bx,[ebp].IFRM._BX
int2129_1:
        mov     PSP_Struc.PSP_Handles[esi],bx
        mov     esi,EPSP_Struc.EPSP_NextPSP[esi]
        or      esi,esi
        jnz     int2129_1
        ret

if 0
        mov     bx,[ebp].IFRM._BX
        cmp     bx,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;more or less?
        jz      int2129_sh0
        jnc     int2129_shb0
        ;
        ;Want less handles!
        ;
        mov     ax,fs
        cmp     ax,WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2]      ;table still in PSP?
        jz      int2129_sh0
        ;
        ;Current table not in PSP.
        ;
        cmp     bx,20+1         ;20 is the lower limit.
        jnc     int2129_shs0
        ;
        ;Reduceing to 20 handles so move the table back into the PSP.
        ;
        push    fs
        pop     es
        mov     edi,offset PSP_Struc.PSP_HandleList
        mov     ecx,20
        mov     al,-1
        rep     stosb           ;initialise the table first.
        mov     edi,offset PSP_HandleList
        lds     si,fs:[PSP_Struc.PSP_HandlePtr]       ;point to current list.
        movzx   esi,si
        mov     ecx,20
        rep     movsb           ;copy 1st 20 entries.
        jmp     int2129_shs1
        ;
int2129_shs0:
        ;Still more than 20 handles so just re-size the memory.
        ;
        push    bx
        movzx   ecx,bx          ;1 byte per entry.
        mov     bx,WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2]
        Sys     ResMem32                ;re-size the memory.
        pop     bx
        jc      int2129_sh0             ;not sure if this can happen.
        ;
int2129_shs1:
        ;Now update the the DOS side of things and the PSP.
        ;
        mov     ax,bx
        sub     ax,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;find out how many less than before.
        mov     WORD PTR fs:[PSP_Struc.PSP_Handles],bx  ;set new number of handles.
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        sub     TotalHandles,ax ;update total count needed.
        mov     ax,TotalHandles
        assume ds:nothing
        or      ax,1
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],6700h
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     bl,21h
        Sys     IntXX           ;set new value with DOS.
        jmp     int2129_sh0
        ;
int2129_shb0:
        ;Want more handles.
        ;
        mov     ax,bx
        sub     ax,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;find out how many more.
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        push    ds
        pop     es
        add     ax,TotalHandles ;get total count needed.
        assume ds:nothing
        or      ax,1
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],6700h
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     ecx,ebx
        mov     bl,21h
        Sys     IntXX
        mov     ebx,ecx
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        test    es:RealRegsStruc.Real_FlagsL[edi],1
        jnz     int2129_sh0             ;no can do!
        ;
        ;We managed to set the new number with DOS, now try and get the
        ;memory needed to take care of things in protected mode.
        ;
        mov     ax,fs
        cmp     ax,WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2]      ;table still in PSP?
        jnz     int2129_shb1
        ;
        ;Current table is in the PSP still so we want a fresh block
        ;of memory and a new selector.
        ;
        movzx   ecx,bx          ;1 byte per entry.
        Sys     GetMem32                ;allocate the memory.
        jc      int2129_shb2
        push    es
        mov     es,bx           ;point to new memory.
        xor     edi,edi
        mov     al,-1
        push    ecx
        movzx   ecx,cx
        rep     stosb           ;initialise table to nothing.
        xor     edi,edi
        push    ds
        lds     si,fs:[PSP_Struc.PSP_HandlePtr]
        movzx   esi,si
        mov     cx,WORD PTR fs:[PSP_Struc.PSP_Handles]
        movzx   ecx,cx
        rep     movsb           ;copy existing entries.
        pop     ds
        pop     ecx
        mov     WORD PTR fs:[PSP_Struc.PSP_HandlePtr],0
        mov     WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2],es
        pop     es
        mov     ax,cx
        sub     ax,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;find out how many more.
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        add     TotalHandles,ax
        assume ds:nothing
        mov     WORD PTR fs:[PSP_Struc.PSP_Handles],cx  ;set new number.
        xor     ax,ax
        clc
        jmp     int2129_sh0
        ;
int2129_shb1:
        ;Current table is NOT in the PSP so just do a re-size on the
        ;memory.
        ;
        movzx   ecx,bx          ;1 byte per handle.
        mov     bx,WORD PTR fs:[PSP_Struc.PSP_HandlePtr+2]
        Sys     ResMem32                ;try and get adjusted memory.
        jc      int2129_shb2
        sub     cx,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;get number differance.
        mov     di,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;get original value.
        mov     al,-1
        push    ecx
        push    es
        movzx   edi,di
        movzx   ecx,cx
        mov     es,ebx
        rep     stosb           ;initialise new entries.
        pop     es
        pop     ecx
        sub     ax,WORD PTR fs:[PSP_Struc.PSP_Handles]  ;find out how many more.
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        add     TotalHandles,ax
        assume ds:nothing
        mov     WORD PTR fs:[PSP_Struc.PSP_Handles],cx  ;set new number.
        xor     ax,ax
        clc
        jmp     int2129_sh0
        ;
int2129_shb2:
        ;Couldn't get the memory at the protected mode end of things
        ;so we need to put the DOS version back to its original size.
        ;
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ax,TotalHandles ;get total count needed.
        assume ds:nothing
        or      ax,1
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     es:RealRegsStruc.Real_AX[edi],6700h
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     bl,21h
        Sys     IntXX
        stc
        ;
int2129_sh0:
        ;Finaly we pass our results back to the caller.
        ;
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        pushfd
        pop     eax
        call    Int21hAL2Carry  ;Set carry.
        ret
endif
Int21hSetHandles endp


;------------------------------------------------------------------------------
;
;Function 6C emulation.
;
Int21hExtendOpen proc near
        mov     ds,[ebp].IFRM._DS
        mov     esi,[ebp].IFRM._ESI
        call    Int21hExtend_DS_ESI     ;Extend [E]SI.
        xor     al,al
        call    Int21hStringLen
        inc     ecx
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        xor     edi,edi
        rep     movsb
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._EBX
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     eax,[ebp].IFRM._ECX
        mov     es:RealRegsStruc.Real_ECX[edi],eax
        mov     eax,[ebp].IFRM._EDX
        mov     es:RealRegsStruc.Real_EDX[edi],edx
        mov     w es:RealRegsStruc.Real_ESI[edi],0
        mov     ax,fs:[EPSP_Struc.EPSP_TransReal]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     eax,es:RealRegsStruc.Real_ECX[edi]
        mov     [ebp].IFRM._CX,ax
        DOS4GExtend w[ebp].IFRM._ECX+2
        mov     al,es:RealRegsStruc.Real_FlagsL[edi]
        call    Int21hAL2Carry  ;Set carry.
        ret
Int21hExtendOpen endp


;------------------------------------------------------------------------------
;
;Get extended error information.
;
Int21hGetErrorInfo proc near
        mov     es,cs:apiDSeg32
        mov     edi,offset Int21Buffer
        mov     eax,[ebp].IFRM._EAX
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     eax,[ebp].IFRM._EBX
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     bl,21h
        Sys     IntXX
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     [ebp].IFRM._AX,ax
        DOS4GExtend w[ebp].IFRM._EAX+2
        mov     eax,es:RealRegsStruc.Real_EBX[edi]
        mov     [ebp].IFRM._BX,ax
        DOS4GExtend w[ebp].IFRM._EBX+2
        mov     eax,es:RealRegsStruc.Real_ECX[edi]
        mov     [ebp].IFRM._CX,ax
        DOS4GExtend w[ebp].IFRM._ECX+2
;
;LABEL string?
;
        cmp     [ebp].IFRM._AX,0022h
        jnz     int2132_done
        mov     ds,cs:apiDSeg16
        assume ds:GROUP16
        mov     ds,RealSegment
        assume ds:nothing
        mov     eax,es:RealRegsStruc.Real_EDI[edi]
        movzx   eax,ax
        movzx   esi,es:RealRegsStruc.Real_ES[edi]
        shl     esi,4
        add     esi,eax
        mov     es,fs:[EPSP_Struc.EPSP_TransProt]
        mov     [ebp].IFRM._ES,es
        mov     [ebp].IFRM._DI,0
        DOS4GExtend w[ebp].IFRM._EDI+2
        xor     edi,edi
        mov     ecx,11
        rep     movsb
;
int2132_done:
        ret
Int21hGetErrorInfo endp


;------------------------------------------------------------------------------
;
;Extend SI into ESI if DS's limit is < 64K
;
;On Entry:
;
;DS:[E]SI       - Pointer to convert.
;
;On Exit:
;
;DS:ESI - Valid [extended] pointer.
;
;All other registers preserved.
;
Int21hExtend_DS_ESI proc near
        push    eax
        mov     eax,ds
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int2133_32Bit
        movzx   esi,si
int2133_32Bit:
        pop     eax
        ret
Int21hExtend_DS_ESI endp


;------------------------------------------------------------------------------
;
;Extend DI into EDI if ES's limit is < 64K
;
;On Entry:
;
;ES:[E]DI       - Pointer to convert.
;
;On Exit:
;
;ES:EDI - Valid [extended] pointer.
;
;All other registers preserved.
;
Int21hExtend_ES_EDI proc near
        push    eax
        mov     eax,es
        lsl     eax,eax
        cmp     eax,10000h
        jnc     int2134_32Bit
        movzx   edi,di
int2134_32Bit:
        pop     eax
        ret
Int21hExtend_ES_EDI endp


;------------------------------------------------------------------------------
;
;Fing length of a string.
;
;On Entry:
;
;AL     - Termination value.
;DS:ESI - String to find length of.
;
;On Exit:
;
;ECX    - Length of string EXCLUDEING terminator.
;
;All other registers preserved.
;
Int21hStringLen proc near
        push    edi
        push    es
        push    ds
        pop     es
        mov     edi,esi
        or      ecx,-1
        repne   scasb
        mov     ecx,edi
        sub     ecx,esi
        dec     ecx
        pop     es
        pop     edi
        ret
Int21hStringLen endp


;------------------------------------------------------------------------------
Int21hAL2Carry  proc    near
        and     al,1
        test    BYTE PTR cs:apiSystemFlags,SF_16BIT
        jz      int2136_32Bit
        or      b[ebp].IFRM.i16._fl,al
        ret
int2136_32Bit:
        or      b[ebp].IFRM.i32._fl,al
        ret
Int21hAL2Carry  endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Setup int 21h patch.
;
Int21hOpen      proc    far
        assume ds:GROUP32
        assume es:GROUP16
;        mov     Int21hDSeg,es   ;store GROUP16 data selector.
;        mov     Int21hCSeg,cs   ;store this segment.
;        mov     Int21hDDSeg,ds
        ;
        mov     bl,21h
        Sys     GetVect
        test    BYTE PTR es:SystemFlags,SF_16BIT
        jz      int2137_Use32
        movzx   edx,dx
int2137_Use32:
        mov     d[OldInt21h+0],edx
        mov     w[OldInt21h+4],cx
int2137_Use0:
        mov     edx,offset Int21h
        mov     ecx,cs
        mov     bl,21h
        Sys     SetVect
        ;
        clc
int2137_9:
        ret
Int21hOpen      endp

        assume es:nothing
        assume ds:nothing

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Remove int 21h patch.
;
Int21hClose     proc    far
        push    ds
        mov     ds,cs:apiDSeg32
        assume ds:GROUP32
        cmp     w[OldInt21h+4],0
        jz      int2138_9
        mov     edx,d[OldInt21h+0]
        mov     cx,w[OldInt21h+4]
        mov     bl,21h
        Sys     SetVect
        assume ds:nothing
int2138_9:
        pop     ds
        ;
        ret
Int21hClose     endp

