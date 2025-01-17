;
;The RAW/VCPI specific code.
;
        .386p

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Main (Kernal) section for RAW/VCPI stuff. This holds the mode switch, interrupt
;simulator and low level memory managers etc.
;
_cwRaw  segment para public 'raw kernal code' use16

        assume ds:GROUP16
;
BreakAddress    dd ?
VMMHandle       dw ?
InWindows       db 0
VMMName db '?:\',128 dup (0)
;
	align 2
PageStackSize   equ     32
ProposedPresentFlag db 0
ProposedRecentFlag db 0
	align 4
ProposedPage    dd 0
CompareCount    dd 0
NoneLockedPages dd 0
RecentMapStack  dd PageStackSize dup (0)
;
PageingPointer  dd 0	;see memory.inc
SwapFileLength  dd 0
;
FreePages       dd 1
medAllocPages   dd 0
TotalPages      dd 1
TotalPhysical   dd 0
;
RawSelBuffer    db 16 dup (?)
;
_LM_BlockBase   dd ?	;see memory.inc
_LM_BlockEnd    dd ?
_LM_Needed      dd ?
_LM_Got         dd ?
;
XMSITEMS equ 32
XMSMEMITEM struc
wHdl     dw ?
dwBase   dd ?
dwEnd    dd ?
XMSMEMITEM ends

MemIntBuffer    RealRegsStruc <?>
XMSList         XMSMEMITEM XMSITEMS dup (<0,0,0>)
XMSTempList     dd XMSITEMS dup (0)
XMSTotal        dd 0

CONVITEMS equ 32

DOSMEMITEM struc
wAddr    dw ?
wSize    dw ?
DOSMEMITEM ends

ConventionalList DOSMEMITEM CONVITEMS dup (<0,0>)
ConvTempList    dw CONVITEMS dup (0)
CONVTotal       dd 0
CONVSavePara    dw 0

;--- CAUSEWAY=LOWMEM:xxx default
IFDEF MAXSAVE
CONVSaveSize    dw      -1
ELSE
CONVSaveSize    dw 32768/16
ENDIF

INT15ITEMS      equ 8

Int15Table      dd INT15ITEMS*2 dup (0)
Int15Size       dd 0
INT15hTotal     dd ?
Int15hValue     dd ?
INT15hLevel2    dw ?
;
;--- mode switch routines; default is VCPI, changed to RawProt2Real/RawReal2Prot in raw mode.
Protected2Real  dw VCPIProt2Real
Real2Protected  dw VCPIReal2Prot
;
A20HandlerCall  dw ?          ; A20_PS2/A20_AT
A20Flag         db 0

XMSPresent      db 0
XMSVer3Present  DB 0          ; MED, 09/10/99

XMSControl      dd ?          ; XMS entry; real-mode far16 address
XMSBlockSize    dw ?
;XMSUMBUsed      dw 0

IFDEF PERMBIG1
Big1Flag        DB      1     ; use int 15h, ax=E801h
ELSE
Big1Flag        DB      0     ; use int 15h, ah=88h (64MB only)
ENDIF
	align 4
;
VCPIRM2PM struct
_CR3        dd 0            ;CR3
_pGDT       dd ?            ;Pointer to GDT descriptor.
_pIDT       dd ?            ;Pointer to IDT descriptor.
_LDT        dw 0            ;LDTR
_TR         dw 0            ;TR
_EIP        dd 0            ;CS:EIP client entry point.
_CS         dw 0            ;/
VCPIRM2PM ends

vcpi VCPIRM2PM <0,0,0,0,0,0,0>

VCPI_Entry label fword
        dd ?
        dw VCPI_0
;
VCPI_SP dw ?,?
;
GDTVal          df ?            ;current value GDT
GDTSav          df ?            ;value GDT real-mode (not used)
IDTVal          df ?            ;current value IDT
IDTSav          df ?            ;value IDT real-mode
CR0ProtSav      dd ?            ;CR0 value protected-mode
CR0Sav          dd ?            ;CR0 value real-mode
CR3Sav          dd ?            ;CR3 value real-mode
;
MDTLinear       dd ?,?          ;+0: linear address GDT/LDT; +4: end GDT/LDT (64 kb) -> byte string (8 kB )
;LDTLinear       dd ?
Page1stLinear   dd ?
PageDirLinear   dd ?
PageAliasLinear dd ?
PageDETLinear   dd 0

ifdef KRNLDBG
PhysPageSave    dd 4+MOVEPAGE1STTOEXT dup (0)   ;save max 4(5) VCPI pages to be released in real-mode only
else
PhysPageSave    dd 3+MOVEPAGE1STTOEXT dup (0)   ;save max 3(4) VCPI pages to be released in real-mode only
endif
;
LinearEntry     dd ?            ;current linear page# (address shr 12) for address space management.
LinearBase      dd ?            ;start linear address space
LinearLimit     dd ?            ;end linear address space allocated so far
;
ifdef KRNLDBG
pminit          df ?
endif
PageBufferReal  dw ?            ;real mode segment for swapfile access
PageBufferLinear dd ?           ;linear address for swapfile access
PageInt         RealRegsStruc <>
;
RawSystemFlags  dw 0,0
;

; byte table for 256 interrupts
; it's used to translate IRQs and INT 1C, 23h, 24h to a callback#
Int2CallCheck label byte
        db 8 dup (0)                    ;00-07
        db 8 dup (8)                    ;08-0F (08-0F -> callback 0-7)

;; MED 02/16/96, force INT15h to be treated as hardware interrupt
        db 8 dup (0)                    ;10-17
;       db      0,0,0,0,0,15h-19,0,0    ;10-17

        db 0,0,0,0,1ch-16,0,0,0         ;18-1F (1C: 1Ch-16=12 -> 28 - 12 -> cb 16)
        db 0,0,0,23h-17,24h-18,0,0,0    ;20-27 (23: 23h-17=18 -> 35 - 18 -> cb 17; 24: 24h-18=18 -> 36 - 18 -> cb 18)
        db 70h-28h dup (0)              ;28-6F
        db 8 dup ((70h-8))              ;70-77 (70-77 -> callback 8-15)
        db 100h-78h dup (0)             ;78-FF
;
;LastCallBack    dw ?
CallBackTable   CallBackStruc MaxCallBacks dup (<0,0,0,0,0,0,0>)
;ALLCallBack     dw ?
;ALLICallBack    dw ?
	align 2
CallBackList    label byte
ICallBackList   label byte
;; MED 02/16/96
;;      rept 16+4
        rept 16+3       ; callbacks for 16 IRQs and Int 1Ch, 23h, 24h
        call    RAWICallBack
        endm
        rept 16 ;MaxCallBacks
        call    RAWCallBack
        endm
CallBackSize    equ     ($-CallBackList)/MaxCallBacks
;
VDiskSig        db 'VDISK V'
VDiskSigLen     equ     $-VDiskSig
VDiskSigVec     equ     19h
VDiskSigOffs    equ     12h
VDiskLoOffs     equ     2ch
VDiskHiOffs     equ     2eh
;
	align 4
RawStackPos     dd RawStackTotal	; this var is saved via DPMI save/restore task state
RawStackReal    dw ?
;
RetAdd          dw ?,?
StackAdd        dw ?,?
;
DbgSize2Dr      db 0,0,1,0,3
DbgType2Dr      db 0,1,3
;
Dbg     struc
Dbg_Address     dd ?
Dbg_Size        db ?
Dbg_Type        db ?
Dbg_Flags       db ?
                db ?
Dbg     ends
;
	align 4
DbgTable        Dbg 4 dup (<0,0,0,0>)
;
MaxMemLin       dd 1021 shl 20	; "MAXMEM" environment setting (max virtual memory, default 1021 MB!)
MaxMemPhys      dd -1           ; "HIMEM" environment setting
;
ExtALLSwitch    db 0
NoPassFlag      DB      0       ; nonzero if not passing real mode hardware interrupts up to protect mode
PreAllocSize    DD      0       ; size of VMM disk preallocation
DesiredVMMName  DB      13 DUP (0)      ; VMM name desired by user via CAUSEWAY e-var
FirstUninitPage DW      0

	align 2

;-------------------------------------------------------------------------------

;--- final switch back to real-mode
;--- this proc expects a real-mode SSSS:OOOO at [SS:SP+0]
;--- ES: flat 4G
;--- DS: KernalDS

RawVCPIRealMode proc far
;
;Disable hardware INT call-backs.
;
        pop     [rv1_RetAdd]   ; pop real-mode return address
        ;
        mov     bx,offset CallBackTable ;list of call backs.
;; MED 02/16/96
;       mov     cx,16+4         ;number of entries to scan.
        mov     cx,16+3         ;number of entries to scan.

rv1_6:  test    CallBackStruc.CallBackFlags[bx],1       ;in use?
        jz      rv1_7
        test    CallBackStruc.CallBackFlags[bx],2       ;interupt?
        jz      rv1_7
        mov     CallBackStruc.CallBackFlags[bx],0
        push    bx
        mov     eax,[bx].CallBackStruc.CallBackReal     ;get original vector value.
        mov     bl,[bx].CallBackStruc.CallBackNum       ;get vector number.
        xor     bh,bh
        shl     bx,2
        mov     d es:[bx+0],eax
        pop     bx
rv1_7:  add     bx,size CallBackStruc   ;next entry.
        loop    rv1_6
;
;Release VCPI memory.
;
        call    VCPIRelExtended ;release VCPI memory.

ife RELXMSINRM

;Release XMS memory while in protected-mode

        call    RAWRelXMS       ;release XMS memory.
endif
;
;Release INT 15h memory.
;
        call    Int15Rel        ;release int 15 vectors.
;
;Restore A20 state.
;
        mov     ax,2
        call    A20Handler
;
;Switch back to real mode.
;
        mov     dx, offset _cwStackEnd
        mov     cx, _cwStack
        call    [Protected2Real]
;
;We're back in real mode so remove any patches.
;
        cmp     w[OldInt2Fr+2],0
        jz      rv1_ir0
        push    ds
        mov     ax,252fh
        lds     dx,OldInt2Fr
        int     21h
        pop     ds
rv1_ir0:

ifdef KRNLDBG
        cmp w[pminit+4],0
        jz nokdbg
D386_Real_Mode_Init equ 45h
        mov ah,D386_Real_Mode_Init
        int 68h
nokdbg:
endif

;--- release some VCPI pages in real-mode only,
;--- so system tables remain valid until final switch.
        mov cx, lengthof PhysPageSave
        mov si, offset PhysPageSave
nextpg:
        lodsd
        and eax, eax
        jz skiprelpg
        mov edx, eax
        and dx, 0f000h
        mov ax, 0DE05h
        int 67h
        loop nextpg
skiprelpg:

if RELXMSINRM
        call    RAWRelXMS               ;release XMS memory.
endif

        cmp     VMMHandle,0             ;Any VMM file?
        jz      rv1_v0
        mov     bx,VMMHandle            ;close VMM file.
        mov     ah,3eh
        int     21h
        mov     dx,offset VMMName       ;delete VMM file.
        mov     ah,41h
        int     21h
rv1_v0:
;
;Go back to _cwMain/Init caller.
;
         jmp    [rv1_RetAdd]

	align 4
rv1_RetAdd      dd 0

; MED 10/31/96
;rv1_StackAdd    df ?

RawVCPIRealMode endp

;-------------------------------------------------------------------------------
;ax=offset of PL0 proc to call

CallPL0Proc     proc near
        push    es
        push    GDTData
        pop     es
        mov     word ptr es:[KernalPL3toPL0 and 0fff8h], ax	; adjust offset in call gate
        pop     es
        db 9ah
        dw 0, KernalPL3toPL0
        ret
CallPL0Proc     endp

;-------------------------------------------------------------------------------
_fRawSimulateInt proc far
        call    RawSimulateInt
        ret
_fRawSimulateInt endp


;-------------------------------------------------------------------------------
_fRawSimulateInt2 proc far
        call    RawSimulateInt2  ; expects params (# in CX) to copy at SS:[EBP]
        ret
_fRawSimulateInt2 endp


;-------------------------------------------------------------------------------
_fRawSimulateFCall proc far
        call    RawSimulateFCall
        ret
_fRawSimulateFCall endp


;-------------------------------------------------------------------------------
_fRawSimulateFCall2 proc far
        call    RawSimulateFCall2
        ret
_fRawSimulateFCall2 endp


;-------------------------------------------------------------------------------
_fRawSimulateFCallI proc far
        call    RawSimulateFCallI
        ret
_fRawSimulateFCallI endp

;-------------------------------------------------------------------------------
;
;Allocate a page of physical memory.
;
fPhysicalGetPage proc near
        db 66h,9ah
        dd offset _ffPhysicalGetPage
        dw DpmiEmuCS
        ret
fPhysicalGetPage endp

;-------------------------------------------------------------------------------
;
;Find out how many physical page of memory are free.
;
fPhysicalGetPages proc near
        db 66h,9ah
        dd offset _ffPhysicalGetPages
        dw DpmiEmuCS
        ret
fPhysicalGetPages endp

;-------------------------------------------------------------------------------
;
;Switch 2 PL0, flush CR3 value then switch back to PL3.
;DS=GROUP16
;
CR3Flush        proc    near
        pushf
        cli
        push    eax
        mov     ax,offset rv10_pl0
        call    CallPL0Proc
        pop     eax
        popf
        ret
        ;
rv10_pl0:
        mov     eax,vcpi._CR3
        mov     cr3,eax         ;flush page cache.
        retd
CR3Flush        endp

	align 2

;-------------------------------------------------------------------------------
Int15PatchTable dw Int15Patch0,Int15Patch1,Int15Patch2,Int15Patch3
        dw Int15Patch4,Int15Patch5,Int15Patch6,Int15Patch7
Int15Patch0:
        push    esi
        mov     si,offset ITable+0*8
        jmp     Int15Patch
Int15Patch1:
        push    esi
        mov     si,offset ITable+1*8
        jmp     Int15Patch
Int15Patch2:
        push    esi
        mov     si,offset ITable+2*8
        jmp     Int15Patch
Int15Patch3:
        push    esi
        mov     si,offset ITable+3*8
        jmp     Int15Patch
Int15Patch4:
        push    esi
        mov     si,offset ITable+4*8
        jmp     Int15Patch
Int15Patch5:
        push    esi
        mov     si,offset ITable+5*8
        jmp     Int15Patch
Int15Patch6:
        push    esi
        mov     si,offset ITable+6*8
        jmp     Int15Patch
Int15Patch7:
        push    esi
        mov     si,offset ITable+7*8
        jmp     Int15Patch
        ;
        assume ds:nothing
Int15Patch:
        cmp     ah,88h          ;get memory size?
        jnz     Checke801h
        mov     eax,cs:4[si]

Int15PatchRet:
        pop     esi
        iret

Checke801h:
        cmp     ax,0e801h
        jnz     Int15Old
        xor     bx,bx
        mov     eax,cs:4[si]    ; get 32-bit memory size 1K pages
        cmp     eax,3c00h
        jbe     E801Done                ; <16M memory, ax/cx hold proper return value
        mov     ebx,eax
        mov     eax,3c00h               ; ax holds 1K memory between 1M and 16M
        sub     ebx,eax                 ; ebx holds 1K pages of high memory
        shr     ebx,6                   ; convert 1K to 64K

E801Done:
        mov     cx,ax
        mov     dx,bx
        jmp     Int15PatchRet

        ;
Int15Old:
        mov     esi,cs:[si]
        mov     DWORD PTR cs:[Int15Jump],esi
        pop     esi
        jmp     DWORD PTR cs:[Int15Jump]        ;pass to old handler.
        assume ds:GROUP16
	align 4
Int15Jump       dd ?
ILevel  dw 0
ITable  dd INT15ITEMS*2 dup (0)


;-------------------------------------------------------------------------------
;
;Release any XMS we claimed.
;
RAWRelXMS       proc    near
        cmp     XMSPresent,0
        jz      rv11_Done
        ;
ife RELXMSINRM
        push    ds
        pop     es
        mov     edi,offset MemIntBuffer
endif        
        ;
        mov     si,offset XMSList       ;+2             ;list of handles.
        mov     cx,XMSITEMS	; max 32 xms handles
rv11_0:
        cmp     [si].XMSMEMITEM.dwBase,0
        jz      rv11_1
        push    cx
        push    si
        mov     dx,[si].XMSMEMITEM.wHdl
ife RELXMSINRM
        push    dx
        mov     eax,[XMSControl]
        mov     [edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,0dh
        mov     w [edi].RealRegsStruc.Real_EAX,ax
        mov     w [edi].RealRegsStruc.Real_EDX,dx
        mov     [edi].RealRegsStruc.Real_SSSP,0
        call    RawSimulateFCall        ;un-lock it first.
        pop     dx
        mov     eax,[XMSControl]
        mov     [edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,0ah
        mov     w [edi].RealRegsStruc.Real_EAX,ax
        mov     w [edi].RealRegsStruc.Real_EDX,dx
        mov     [edi].RealRegsStruc.Real_SSSP,0
        call    RawSimulateFCall
else
        mov ah, 0dh
        call [XMSControl]
        mov ah, 0ah
        call [XMSControl]
endif
        pop     si
        pop     cx
rv11_1:
        add     si,sizeof XMSMEMITEM
        dec     cx
        jnz     rv11_0
        ;
rv11_Done:
        ret
RAWRelXMS       endp

if VCPIPMCALL
freevcpi proc
        mov ax, 0DE05h
        jmp [VCPI_Entry]
freevcpi endp
else

;--- int 67h is preferably called with SimulateFCall than with SimulateInt

Int67h  proc    far
        int     67h
        ret
Int67h  endp

FreeVCPIPage proc        
        push    cx
        push    esi
        push    edi
        push    ds
        push    es

        push    ds
        pop     es
        mov     ax,0DE05h               ;free 4k page.
        mov     edi,offset MemIntBuffer
        mov     w RealRegsStruc.Real_EAX[edi],ax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     RealRegsStruc.Real_IP[edi],offset Int67h
        mov     RealRegsStruc.Real_CS[edi],GROUP16
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    RawSimulateFCall

        pop     es
        pop     ds
        pop     edi
        pop     esi
        pop     cx
        ret
FreeVCPIPage endp        
endif

;-------------------------------------------------------------------------------
;
;Release any VCPI memory aquired.
;
VCPIRelExtended proc near

        cmp     ProtectedType,PT_VCPI
        jnz     rv12_9
        ;
        cmp     PageDETLinear,0
        jz      rv12_NoDET
        ;
        ;Release DET pages.
        ;
        mov     ax,KernalZero
        mov     es,ax
        mov     cx,1024
        mov     esi,PageDETLinear
rv12_d0:
        btr     w es:[esi],0
        jnc     rv12_d1
        mov     edx,es:[esi]
        test    dx,1 shl 10                     ;VCPI bit set?
        jz      rv12_d1
        and     dx,0F000h
if VCPIPMCALL
        mov     ax,offset freevcpi
        call    CallPL0Proc
else
        call    FreeVCPIPage
endif
rv12_d1:
        add     esi,4
        loop    rv12_d0
rv12_NoDET:
        ;
        ;Release normal memory.
        ;
        mov     ax,KernalZero
        mov     es,ax
        mov     esi,PageDirLinear
        mov     cx,1024-2               ;don't scan the last 2 regions (page table mappings)
        mov     edi,PTMAPADDR           ;base of page alias's.
rv12_0:                                 ;<--- next page table
        test    b es:[esi],1            ;Page table present?
        jz      rv12_1

        push    cx
        push    edi
        mov     cx,1024
        cmp     edi,PTMAPADDR           ; see if 0th page table
        jne     rv12_2                  ; no
        movzx   eax,FirstUninitPage     ; don't scan pages reserved for VCPI host
        add     edi,eax
        shr     ax,2
        sub     cx,ax
rv12_2:                                 ;<--- next PTE
        mov     edx,es:[edi]
        test    dl,1                    ;Page present?
        jz      rv12_3
        test    dx,1 shl 10             ;VCPI bit set?
        jz      rv12_3
        and     b es:[edi],not 1        ;mark as no longer present.
        and     dx,0F000h
if VCPIPMCALL
        mov     ax, offset freevcpi
        call    CallPL0Proc
else
        call    FreeVCPIPage
endif
rv12_3:
        add     edi,4
        loop    rv12_2
        pop     edi
        pop     cx

        mov     edx,es:[esi]
        test    dx,1 shl 10             ;VCPI bit set?
        jz      rv12_1
        and     b es:[esi],not 1        ;mark as no longer present.
        and     dx,0F000h
if VCPIPMCALL
        mov     ax, offset freevcpi
        call    CallPL0Proc
else
        call    FreeVCPIPage
endif
rv12_1:
        add     edi,4096        ;next page table alias.
        add     esi,4           ;next page dir entry.
        loop     rv12_0
        call    CR3Flush
rv12_9:
        ret
VCPIRelExtended endp

;-------------------------------------------------------------------------------
;
;Release real mode int 15 patches.
;
Int15Rel        proc    near
        cmp     ProtectedType,PT_VCPI
        jnc     rv15_9      ; skip if VCPI or DPMI
        cmp     XMSPresent,0
        jnz     rv15_9
        ;
        mov     esi,offset ITable+((4+4)*(INT15ITEMS-1))
        mov     ecx,INT15ITEMS
rv15_0:
        push    ecx
        push    esi
        cmp     d[esi],0
        jz      rv15_1
        mov     dx,0[esi]
        mov     cx,2[esi]
        mov     bl,15h
        push    ax
        push    ebx
        push    es
        mov     ax,KernalZero
        mov     es,ax
        movzx   ebx,bl
        shl     ebx,2
        pushf
        cli
        mov     es:0[ebx],dx
        mov     es:2[ebx],cx
        popf
        pop     es
        pop     ebx
        pop     ax
rv15_1:
        pop     esi
        pop     ecx
        sub     esi,4+4
        dec     ecx
        jnz     rv15_0
rv15_9:
        ret
Int15Rel        endp


;-------------------------------------------------------------------------------
;
;Real to protected mode PL3 switch. Used if no VCPI available.
;
;On Entry: [CR0ProtSav],[GDTVal],[IDTVal] must be initialized!
;
;CX:EDX - target stack.
;modifies EAX, SS:ESP, segment registers
;
RawReal2Prot    proc    near
        pop     [rv16_Return]
;        mov     d[rv16_ReturnStack+0],edx ;store return stack.
;        mov     w[rv16_ReturnStack+4],cx
        ;
        sidt    IDTSav          ;save old IDT value for switch back.
        lgdt    GDTVal          ;Setup GDT &
        lidt    IDTVal          ;IDT.
;       pushfd
;       pop     eax
;       mov     EFlagsSav,eax
        mov     eax,cr3
        mov     CR3Sav,eax
        mov     eax,cr0
        mov     CR0Sav,eax
        mov     eax,vcpi._CR3   ;PageDirLinear
        mov     cr3,eax         ;set page dir address.

; MED 10/15/96
;       mov     eax,cr0         ;Get machine status &
;       or      eax,080000001h  ;set PM+PG bits.
        mov     eax,CR0ProtSav  ; restore protected mode cr0 status

        mov     cr0,eax         ;/
        db 0eah                 ;Absolute 16-bit jump, to clear
        dw rv16_0,KernalCS0     ;instruction pre-fetch & load CS.
rv16_0:
        mov     ax,KernalSS0
        mov     ss,ax           ;/
        mov     esp,tPL0StackSize

        mov     ax,GDTData
        mov     ds,ax
        mov     byte ptr ds:[(KernalTS and 0fff8h) + Desc.Access],DescPresent+DescPL3+Desc386Tss

        mov     ax,KernalLDT    ;set empty LDT selector
        lldt    ax
        mov     si,KernalTS     ;Set task register
        ltr     si
        ;
        ;Make our stuff addresable.
        ;
        mov     ax,KernalDS     ;Get data descriptor.
        mov     ds,ax
        mov     es,ax
        xor     ax,ax
        mov     gs,ax
        mov     fs,ax
        ;
        cld
        clts
        ;
;        mov     edx,d[rv16_ReturnStack+0]
;        mov     cx,w[rv16_ReturnStack+4]
        call    RawPL0toPL3
        ;
        jmp     [rv16_Return]
;        push    [rv16_Return]
;        ret
        ;
	align 2
rv16_Return      dw ?
;rv16_ReturnStack df ?

RawReal2Prot    endp


;-------------------------------------------------------------------------------
;
;Protected PL3 to real mode switch. Used if no VCPI available
;
;On Entry:-
;
;CX:DX  - target stack.
;
RawProt2Real    proc    near
        pop     [rv17_Return]
;        mov     [rv17_ReturnSP],dx
;        mov     [rv17_ReturnSS],cx
        ;
        call    RawPL3toPL0
        mov     ax,KernalSS0Switch ;SS must be PL0, 16-bit and 64k limit
        mov     ss,ax
        mov     ax,KernalDS
        mov     ds,ax           ;DS,ES,FS,GS,SS must be data with 64k limit
        mov     es,ax           ;expand up, read/write for switch back to real
        mov     fs,ax           ;mode.
        mov     gs,ax           ;/
        ;

; MED 10/15/96
        mov     eax,cr0
        mov     CR0ProtSav,eax  ; save protected mode cr0 status

        mov     eax,CR0Sav              ;Get machine control &

; MED 10/15/96, don't clear emulate math coprocessor bit
;       and     eax,07FFFFFF2h  ;clear PM bit.
        and     eax,07FFFFFF6h  ;clear PM bit.

        mov     cr0,eax         ;/
        db 0eah                 ;Absolute 16-bit jump, to clear
        dw rv17_Resume,GROUP16  ;instruction pre-fetch & load CS.
rv17_Resume:
        mov     ax,cs
        mov     ds,ax
;        mov     ss,[rv17_ReturnSS]
;        movzx   esp,[rv17_ReturnSP]
        mov     ss,cx
        movzx   esp,dx
        mov     eax,CR3Sav
        mov     cr3,eax
        lidt    IDTSav          ;restore old IDT 0(3ff)
;       lgdt    GDTSav
;       push    EFlagsSav
;       popfd
        ;
        jmp     [rv17_Return]
;        push    [rv17_Return]
;        ret
        ;
	align 2
rv17_Return        dw ?

RawProt2Real    endp


;-------------------------------------------------------------------------------
;
;Real to protected mode PL3 switch, VCPI.
;
;On Entry:-
;
;--- CX:EDX - target stack.
;--- modifies AX,ESI
;
VCPIReal2Prot   proc    near
        pop     [rv18_Return]
        mov     vcpi._CS,KernalCS0
        mov     vcpi._EIP,offset rv18_Resume486
        mov     ax,0de0ch
        mov     esi,GROUP16
        shl     esi,4
        add     esi,offset vcpi
        int     67h             ; VCPI call 0de0ch may alter EAX and ESI
        ;
rv18_Resume486:
        ;Make our stuff addresable.
        ;
        mov     ax,KernalSS0
        mov     ss,ax           ;/
        mov     esp,tPL0StackSize

        mov     ax,KernalDS     ;Get data descriptor.
        mov     ds,ax
        mov     es,ax
        xor     ax,ax
        mov     gs,ax
        mov     fs,ax
        ;
        call    RawPL0toPL3
        ;
        jmp     [rv18_Return]
;
	align 2
rv18_Return      dw ?
;
VCPIReal2Prot   endp


;-------------------------------------------------------------------------------
;
;Protected PL3 to real mode switch.
;
;On Entry:-
;
;--- CX:DX  - Target stack.
;--- modifies AX
;--- DS changed to Flat!
;
VCPIProt2Real   proc    near
        pop     [rv19_Return]
;        mov     [rv19_ReturnSP],dx
;        mov     [rv19_ReturnSS],cx
        ;
        call    RawPL3toPL0
        mov     ax,KernalSS0Switch
        mov     ss,ax
if 0
;DS,ES,FS,GS,SS must be data with 64k limit
;expand up, read/write for switch back to real mode.
;--- yes, but here we switch back to v86-mode, that is,
;--- protected-mode isn't left and the segment attributes
;--- will be automatically set to v86-compatible values.
        mov     ax,KernalDS
        mov     ds,ax
        mov     es,ax
        mov     fs,ax
        mov     gs,ax
endif        
        ;
        movzx   edx,dx
        xor     eax,eax
        push    eax     ;gs
        push    eax     ;fs
        mov     ax,GROUP16
        push    eax     ;ds
        push    eax     ;es
        push    ecx     ;ss
        push    edx     ;esp
        pushfd  ;EFLags (reserved for)
        push    eax     ;cs
        mov     ax,[rv19_Return]
        push    eax     ;eip
        mov     ax,KernalZero
        mov     ds,ax
        mov     ax,0DE0Ch
        call    cs:[VCPI_Entry]       ;switch back.
        ;
;
	align 2
rv19_Return     dw ?

VCPIProt2Real   endp

        assume ds:GROUP16

;-------------------------------------------------------------------------------
;--- used by RawReal2Prot & VCPIReal2Prot
;--- CX:EDX=SS:ESP

RawPL0toPL3      proc    near
        pop     [rv20_RetAdd]
        push    0
        push    cx              ;SS
        push    edx             ;ESP
        pushfd                  ;Hiword(EFlags)
        pop     ax
        and     ah,10001111b    ;clear NT & IOPL.
        or      ah,00110000b    ;set IOPL 3
        push    ax
        popf
        push    ax              ;Loword(EFlags)
        push    0
        push    KernalCS        ;CS
        push    0               ;Hiword(EIP)
        push    [rv20_RetAdd]   ;Loword(EIP)
        iretd
        ;
	align 2
rv20_RetAdd     dw ?

RawPL0toPL3      endp

;-------------------------------------------------------------------------------
;--- switch to PL0 - used by RawProt2Real & VCPIProt2Real
;--- DS:GROUP16
;--- ax, es, SS:ESP modified.

RawPL3toPL0      proc    near
        mov     ax,GDTData
        mov     es,ax
        pop     w es:[KernalPL3toPL0 and 0fff8h]	; adjust offset in call gate
        db 9ah                      ; call FAR16 call gate
        dw 0,KernalPL3toPL0         ; the call gate is 32-bit, though!
        ;

RawPL3toPL0      endp

;--- protected-mode save state proc

StateSavePM   proc    far
        push    ds
        push    KernalDS
        pop     ds
        push    edi
        test    BYTE PTR RawSystemFlags,1
        jz      @F
        movzx   edi,di
@@:
        push    eax
        cmp     al,0
        jz      sspm_Save
        cmp     al,1
        jz      sspm_Restore
        jmp     sspm_Done
        ;
sspm_Save:
        mov     eax,[RawStackPos]
        mov     es:[edi],eax
        jmp     sspm_Done
sspm_Restore:
        mov     eax,es:[edi]
        mov     [RawStackPos],eax
sspm_Done:
        pop     eax
        pop     edi
        pop     ds
        test    BYTE PTR cs:RawSystemFlags,1
        jz      @F
        retw
@@:
        retd
        ;
StateSavePM   endp

StateSaveRM   proc    far
        ret
StateSaveRM   endp

;--- raw switch to real-mode
;--- ax=ds, cx=es, dx:bx=ss:sp, si:di=cs:ip

SwitchProcPM   proc    far
        cli
        push KernalDS
        pop ds
        mov [dsreg],ax
        mov [esreg],cx
;--- [Protected2Real] expects new ss:sp in CX:DX
        mov cx,dx
        mov dx,bx
        call [Protected2Real]
        mov es,[esreg]
        mov ds,[dsreg]
        push si
        push di
        retf
SwitchProcPM   endp

;--- raw switch to protected-mode
;--- ax=ds, cx=es, dx:(e)bx=ss:(e)sp, si:(e)di=cs:(e)ip

SwitchProcRM   proc    far
        cli
        push GROUP16
        pop ds
        mov [dsreg],ax
        mov [esreg],cx
        mov [csreg],si
;--- [Real2Protected] expects new ss:esp in CX:EDX
        mov cx,dx
        mov edx,ebx
        test BYTE PTR cs:RawSystemFlags,1
        jz @F
        movzx edx,dx
@@:
        call [Real2Protected]
        mov es,[esreg]
        movzx esi,[csreg]
        mov ds,[dsreg]
        test BYTE PTR cs:RawSystemFlags,1
        jz @F
        push si
        push di
        retw
@@:
        push esi
        push edi
        retd
	align 2
dsreg dw ?
esreg dw ?
csreg dw ?
SwitchProcRM   endp

;-------------------------------------------------------------------------------
;
;Release INT 2F patch.
;
ReleaseINT2F    proc    near
        push    ds
        mov     ax,252fh
        lds     dx,OldInt2Fr
        int     21h
        pop     ds
        ret
ReleaseINT2F    endp


;-------------------------------------------------------------------------------
;
;Intercept for windows init broadcast.
;
Int2FPatch      proc    near
;
;check if it's an init broadcast that's being allowed.
;
        cmp     ax,1605h
        jnz     rv23_exit
        or      bx,bx
        jnz     rv23_ret
        or      si,si
        jnz     rv23_ret
        or      cx,cx
        jnz     rv23_ret
        test    dx,1
        jnz     rv23_ret
        ;
        push    ds
        push    cs
        pop     ds
        inc     InWindows
        pop     ds
        jmp     rv23_ret
;
;check if it's an exit broadcast.
;
rv23_exit:
        cmp     ax,1606h
        jnz     rv23_ret
        test    dx,1
        jnz     rv23_ret
        ;
        push    ds
        push    cs
        pop     ds
        dec     InWindows
        pop     ds
;
;Let previous handlers have a go at it.
;
rv23_ret:
        jmp     cs:[OldInt2Fr]

if 0
        push    ax
        push    dx
        push    ds
        push    cs
        pop     ds
        mov     dx,offset WinMessage
        mov     ah,9
        int     21h
        pop     ds
        mov     ax,4cffh
        int     21h

rv23_Old:
        jmp     cs:[OldInt2Fr]
        assume ds:GROUP16

WinMessage      db 'Cannot run Windows in enhanced mode while a CauseWay application is active.',13,10
        db 'Run Windows in standard mode or remove the CauseWay application.',13,10,'$'
endif
	align 2
OldInt2Fr       dd 0
Int2FPatch      endp


;-------------------------------------------------------------------------------
;
;Simulate real mode interupt.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;BL     - Interupt number.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateInt proc     near
        push    cx
        push    bx
        mov     bh,0
        mov     cx,0
        jmp     RAWSimulate
RawSimulateInt  endp


;-------------------------------------------------------------------------------
;
;Simulate real mode interupt.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;BL     - Interupt number.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateInt2 proc    near
        push    cx
        push    bx
        mov     bh,0
        jmp     RAWSimulate
RawSimulateInt2 endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCall proc near
        push    cx
        push    bx
        mov     bh,1
        mov     cx,0
        jmp     RAWSimulate
RawSimulateFCall endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCall2 proc near
        push    cx
        push    bx
        mov     bh,1
        jmp     RAWSimulate
RawSimulateFCall2 endp


;-------------------------------------------------------------------------------
;
;Simulate real mode far call with IRET stack frame.
;
;On Entry:-
;
;ES:EDI - Parameter table.
;SS:EBP - Stacked parameters.
;CX     - stacked word count.
;
;On Exit:-
;
;Parameter table updated.
;
RawSimulateFCallI proc near
        push    cx
        push    bx
        mov     bh,2
        jmp     RAWSimulate
RawSimulateFCallI endp


;-------------------------------------------------------------------------------
;
; Simulate either a real mode INT or far call.
; BH: 0=INT, 1=far call, 2=far call with IRET frame
; BL: int# if BH==0
; CX: word params to copy to real-mode stack
; this "proc" is never called, only jumped to, with
; registers BX & CX on stack.
;

;--- stack frame
RV29FR struct
    dw ?    ; rv29_ ...
    dw ?
    dd ?
    dd ?
    dw ?,?,?,?	;seg regs
    dd ?,?,?,?,?,?,?,?	;pushad
wFlgs dw ?
RV29FR ends

RAWSimulate     proc    near
        pushf                   ;Preserve IF state.
        cli                     ;Stop INTs interfering.
        cld                     ;make sure direction is right.
        pushad
        push    ds
        push    es
        push    fs
        push    gs
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,ax           ;/
        mov     ax,KernalZero
        mov     fs,ax
        push    [rv29_tVCPI_SP]
        push    [rv29_IntAdd]
        push    [rv29_CallAdd]
        push    [rv29_ourstack]

        mov     [rv29_ourstack],0
;
;setup the real mode stack.
;
        mov     eax,es:RealRegsStruc.Real_SSSP[edi]     ;check if stack is being
        or      eax, eax
        jnz     rv29_GotStack
;
;Caller isn't supplying a stack so we will.
;
        mov     eax,RawStackPos
        sub     RawStackPos,RawStackDif ;update for re-entry.
        mov     es:RealRegsStruc.Real_SP[edi],ax
        mov     si,RawStackReal
        mov     es:RealRegsStruc.Real_SS[edi],si
        or      [rv29_ourstack],-1
;
;Point to the real mode stack.
;
rv29_GotStack:
        movzx   eax,es:RealRegsStruc.Real_SP[edi]
        movzx   esi,es:RealRegsStruc.Real_SS[edi]
        sub     eax,(4+4)+(4+4)		; make room for pm ss:esp and rmcs es:edi
        mov     w[rv29_tVCPI_SP+0],ax
        mov     w[rv29_tVCPI_SP+2],si
        mov     edx,esi
        shl     esi,4
        add     esi,eax
;
;Store current stack pointer on v86 stack.
;
        xor     eax,eax
        mov     ax,ss
        mov     fs:[esi+4],eax
        mov     eax,esp
        test    BYTE PTR RawSystemFlags,1
        jz      rv29_noextendstack
        movzx   eax,ax
rv29_noextendstack:
        mov     fs:[esi+0],eax
;
;Store table address on v86 stack.
;
        xor     eax,eax
        mov     ax,es
        mov     fs:[esi+12],eax
        mov     fs:[esi+8],edi
;
;Copy stacked parameters.
;
        or      cx,cx
        jz      rv29_NoStacked
        movzx   eax,cx
        shl     eax,1
        add     ebp,eax
rv29_copystack0:
        sub     ebp,2
        sub     esi,2
        mov     ax,[ebp]
        mov     fs:[esi],ax
        dec     cx
        jnz     rv29_copystack0
;
;Put flags onto the real mode stack.
;
rv29_NoStacked:
        mov     ebp,esp
        test    BYTE PTR RawSystemFlags,1
        jz      rv29_Its32
        movzx   ebp,bp
rv29_Its32:
        mov     ax,[ebp].RV29FR.wFlgs
        or      bh,bh           ;int or far?
        jnz     rv29_NoIF
        and     ah,11111100b    ;clear TF and IF
rv29_NoIF:
        sub     esi,2
        mov     fs:[esi],ax
        ;
        ;See if the CS:IP is supplied or needs fetching.
        ;
        or      bh,bh
        jz      rv29_IsInt
        ;
        mov     ecx,es:RealRegsStruc.Real_CSIP[edi]
        mov     [rv29_CallAdd],offset rv29_fcall
        cmp     bh,2
        jnz     rv29_NotInt
        mov     [rv29_CallAdd],offset rv29_fcalli
        jmp     rv29_NotInt
        ;
rv29_IsInt:
        ;See if this is a busy interrupt call back.
        ;
        xor     bh,bh
        mov     bp,bx
        shl     bp,2
        mov     al,[Int2CallCheck+bx]
        or      al,al
        jz      rv29_c3
        sub     bl,al           ;gives callback# in bx
        shl     bx,3            ;*8
        mov     ax,bx
        shl     bx,1            ;*16
        add     bx,ax           ;*24 (=size CallBackStruc)
        add     bx,offset CallBackTable
        test    CallBackStruc.CallBackFlags[bx],128     ;this entry in use?
        jz      rv29_c3
        mov     ecx,CallBackStruc.CallBackReal[bx]
        jmp     rv29_c2
        ;
rv29_c3:
        ;Get interupt address to put on stack.
        ;
        mov     ecx,DWORD PTR fs:[bp]
        ;
rv29_c2:
        mov     [rv29_CallAdd],offset rv29_int
        ;
rv29_NotInt:
        sub     esi,4
        mov     fs:[esi],ecx
;
;Copy register values onto real mode stack.
;8*4 = PUSHAD
;2   = Flags
;4*2 = seg regs DS,ES,FS,GS

        sub     esi,8*4+4*2+2+2 ;extra +2 for movsD
        push    esi
        push    edi
        push    ds
        push    es
        xchg    esi,edi
        push    es
        push    fs
        pop     es
        pop     ds
        mov     ecx,(8*4+4*2+2+2)/4
        rep     movsd [edi],[esi]
        pop     es
        pop     ds
        pop     edi
        pop     esi
;
;Get ss:sp values again.
;
        mov     ecx,edx
        mov     eax,ecx
        shl     eax,4
        mov     edx,esi
        sub     edx,eax
;
;Switch back to v86 mode.
;
        call    [Protected2Real]
;
;Fetch registers off the stack.
;
        assume ds:nothing
        popad
        pop     WORD PTR cs:[rv29_IntAdd]       ;lose dummy.
        pop     es
        pop     ds
        pop     fs
        pop     gs
        pop     WORD PTR cs:[rv29_IntAdd]       ;lose dummy.
        ;
        pop     cs:[rv29_IntAdd]
        jmp     cs:[rv29_CallAdd]
        ;
rv29_fcall:
        popf
        call    cs:[rv29_IntAdd]
        jmp     rv29_Back
        ;
rv29_fcalli:
rv29_int:
        call    cs:[rv29_IntAdd]
rv29_Back:
        pushf
        cli
        cld
        pop     WORD PTR cs:[rv29_IntAdd]
        and     WORD PTR cs:[rv29_IntAdd],0000110011010101b
;
;Switch back to old stack.
;
;        mov     ss,WORD PTR cs:[rv29_tVCPI_SP+2]
;        mov     sp,WORD PTR cs:[rv29_tVCPI_SP+0]
         lss     sp,cs:[rv29_tVCPI_SP]
;
;Save all registers.
;
        push    WORD PTR cs:[rv29_IntAdd]       ;save dummy.
        push    gs
        push    fs
        push    ds
        push    es
        push    WORD PTR cs:[rv29_IntAdd]       ;save flags.
        pushad
;
;Make our data addresable again and store stack values.
;
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16
        mov     w[rv29_tVCPI_SP+0],sp
        mov     w[rv29_tVCPI_SP+2],ss
        mov     bp,sp
;
;Retrieve protected mode stack address.
;
        mov     edx,d[bp+(8*4)+(2+2+2+2+2)+(2)]
        mov     cx,w[bp+4+(8*4)+(2+2+2+2+2)+(2)]
;
;switch back to protected mode.
;
        call    [Real2Protected]
;
        mov     ax,KernalZero   ;/
        mov     fs,ax           ;/
;
;Retreive v86 stack address.
;
        movzx   esi,w[rv29_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv29_tVCPI_SP+0]
        add     esi,eax
;
;Retrieve table address.
;
        les     edi,fs:[esi+(8*4)+(2+2+2+2+2)+(2)+(4+4)]
;
;Copy new register values into table.
;

        push    esi
        push    edi
        push    ds
        push    fs
        pop     ds
        mov     ecx,(4+4+4+4+4+4+4+4+2+2+2+2+2+2)/4
        rep     movsd [edi],[esi]
        pop     ds
        pop     edi
        pop     esi
        mov     bx,[esp].RV29FR.wFlgs
        and     bx,1111001100101010b
        or      es:RealRegsStruc.Real_Flags[edi],bx
        ;
        cmp     [rv29_ourstack],0
        jz      rv29_nostackadjust
        add     RawStackPos,RawStackDif ;update for re-entry.
rv29_nostackadjust:
        pop     [rv29_ourstack]
        pop     [rv29_CallAdd]
        pop     [rv29_IntAdd]
        pop     [rv29_tVCPI_SP]
        ;
        pop     gs
        pop     fs
        pop     es
        pop     ds
        popad
        popf
        clc
        ;
        pop     bx
        pop     cx
        ret
        ;
	align 4
rv29_IntAdd   dd ?
rv29_CallAdd  dw ?
rv29_tVCPI_SP dd ?
rv29_ourstack dw 0

RAWSimulate     endp


;-------------------------------------------------------------------------------

RAWCallBack     proc    near
        pushf
        cli
        ;
        ;Check if Windows enhanced mode has been started.
        ;
        cmp     cs:InWindows,0
        jz      rv30_Normal
        popf
        retf
        ;
rv30_Normal:
        push    ax
        push    bp
        push    ds
        mov     ax,GROUP16
        mov     ds,ax
        assume ds:GROUP16
        mov     bp,sp
        mov     ax,[bp+2+2+2+2] ;get return address
        mov     RetAdd,ax
        mov     ax,[bp+2+2+2]   ;get flags
        mov     [bp+2+2+2+2],ax ;ovewrite return address.
        mov     StackAdd+0,bp
        add     StackAdd+0,2+2+2+2+2      ;correct for stacked registers.
        mov     StackAdd+2,ss
        pop     ds
        pop     bp
        pop     ax
        add     sp,2            ;remove local return address.
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
        push    gs
        mov     ax,GROUP16
        mov     ds,ax           ;make our data addresable.
        mov     es,ax
        mov     fs,ax
        mov     gs,ax
        mov     [VCPI_SP+0],sp
        mov     [VCPI_SP+2],ss
        ;
        ;Check if this call back is int &| busy.
        ;
        mov     ax,RetAdd               ;get return address.
        sub     ax,CallBackSize ;back to start of call back entry.
        sub     ax,offset CallBackList  ;offset from start of list.
        xor     dx,dx
        mov     bx,CallBackSize
        div     bx              ;entry number.
        mov     bx,size CallBackStruc
        mul     bx              ;get offset into table.
        mov     bx,offset CallBackTable
        add     bx,ax           ;point to this entry.
        ;
        ;switch to protected mode.
        ;
        mov     cx,KernalSS
        mov     edx,RawStackPos
        sub     RawStackPos,RawStackDif
        call    [Real2Protected]
        ;
        mov     ax,RetAdd               ;get return address.
        sub     ax,CallBackSize ;back to start of call back entry.
        sub     ax,offset CallBackList  ;offset from start of list.
        jz      rv30_zero
        xor     dx,dx
        mov     bx,CallBackSize
        div     bx              ;entry number.
rv30_zero:
        mov     bx,size CallBackStruc
        mul     bx              ;get offset into table.
        mov     bx,offset CallBackTable
        add     bx,ax           ;point to this entry.
        movzx   esi,[VCPI_SP+2] ;point to stacked registers.
        shl     esi,4
        movzx   eax,[VCPI_SP+0]
        add     esi,eax
        mov     ax,KernalZero   ;/
        mov     fs,ax           ;/
        ;
        les     edi,CallBackStruc.CallBackRegs[bx]      ;get register structure.
        mov     ax,fs:[esi+0]
        mov     es:RealRegsStruc.Real_GS[edi],ax
        mov     ax,fs:[esi+2]
        mov     es:RealRegsStruc.Real_FS[edi],ax
        mov     ax,fs:[esi+4]
        mov     es:RealRegsStruc.Real_ES[edi],ax
        mov     ax,fs:[esi+6]
        mov     es:RealRegsStruc.Real_DS[edi],ax
        mov     eax,fs:[esi+8]
        mov     es:RealRegsStruc.Real_EBP[edi],eax
        mov     eax,fs:[esi+12]
        mov     es:RealRegsStruc.Real_EDI[edi],eax
        mov     eax,fs:[esi+16]
        mov     es:RealRegsStruc.Real_ESI[edi],eax
        mov     eax,fs:[esi+20]
        mov     es:RealRegsStruc.Real_EDX[edi],eax
        mov     eax,fs:[esi+24]
        mov     es:RealRegsStruc.Real_ECX[edi],eax
        mov     eax,fs:[esi+28]
        mov     es:RealRegsStruc.Real_EBX[edi],eax
        mov     eax,fs:[esi+32]
        mov     es:RealRegsStruc.Real_EAX[edi],eax
        mov     ax,fs:[esi+36]
        mov     es:RealRegsStruc.Real_Flags[edi],ax
        mov     ax,RetAdd
        mov     es:RealRegsStruc.Real_IP[edi],ax
        mov     ax,GROUP16
        mov     es:RealRegsStruc.Real_CS[edi],ax
        mov     ax,StackAdd
        mov     es:RealRegsStruc.Real_SP[edi],ax
        mov     ax,StackAdd+2
        mov     es:RealRegsStruc.Real_SS[edi],ax
        ;
        test    BYTE PTR RawSystemFlags,1
        jz      rv30_Use32Bit12
        mov     ax,w[bx].CallBackStruc.CallBackProt+0
        mov     w[rv30_CallB016+0],ax
        mov     ax,w[bx].CallBackStruc.CallBackProt+4
        mov     w[rv30_CallB016+2],ax
        jmp     rv30_Use16Bit12
rv30_Use32Bit12:
        mov     eax,d[bx].CallBackStruc.CallBackProt+0
        mov     d[rv30_CallB032+0],eax
        mov     ax,w[bx].CallBackStruc.CallBackProt+4
        mov     w[rv30_CallB032+4],ax
rv30_Use16Bit12:
        push    bx              ;save call back structure pointer.
        ;
rv30_oops:
        ;
        ;Setup stack referance.
        ;
        push    eax
        push    ebx
        push    esi
        push    edi
        push    es
        movzx   esi,es:RealRegsStruc.Real_SS[edi]
        shl     esi,4
        mov     ax,KernalZero
        mov     es,ax
        movzx   eax,w[CallBackStruc.CallBackStackSel+bx]
        and     ax,not 7
;        mov     edi,GDTLinear
        mov     edi,d GDTVal+2
        add     edi,eax
        mov     es:[edi+2],si   ;store low word of linear base.
        shr     esi,16
        mov     bx,si
        mov     es:[edi+4],bl   ;store mid byte of linear base.
        mov     es:[edi+7],bh   ;store high byte of linear base.
        pop     es
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        mov     ds,w[CallBackStruc.CallBackStackSel+bx]
        movzx   esi,es:RealRegsStruc.Real_SP[edi]
        ;
        assume ds:nothing
        test    BYTE PTR cs:RawSystemFlags,1
        jz      rv30_Use32Bit13
        pushf
        call    cs:[rv30_CallB016]
        jmp     rv30_Use16Bit13
rv30_Use32Bit13:
        pushfd
        call    cs:[rv30_CallB032]
rv30_Use16Bit13:
        cli
        mov     ax,KernalDS             ;make our data addresable.
        mov     ds,ax
        assume ds:GROUP16
        pop     bx              ;restore call back structure.
        ;
        movzx   esi,es:RealRegsStruc.Real_SS[edi]       ;point to stacked registers.
        mov     [VCPI_SP+2],si
        shl     esi,4
        movzx   eax,es:RealRegsStruc.Real_SP[edi]
        sub     eax,(2)+(4+4+4+4+4+4+4)+(2+2+2+2)+(2+2)
        mov     [VCPI_SP+0],ax
        add     esi,eax
        mov     ax,KernalZero   ;/
        mov     fs,ax           ;/
        mov     ax,es:RealRegsStruc.Real_GS[edi]
        mov     fs:[esi],ax
        mov     ax,es:RealRegsStruc.Real_FS[edi]
        mov     fs:[esi+2],ax
        mov     ax,es:RealRegsStruc.Real_ES[edi]
        mov     fs:[esi+4],ax
        mov     ax,es:RealRegsStruc.Real_DS[edi]
        mov     fs:[esi+6],ax
        mov     eax,es:RealRegsStruc.Real_EBP[edi]
        mov     fs:[esi+8],eax
        mov     eax,es:RealRegsStruc.Real_EDI[edi]
        mov     fs:[esi+12],eax
        mov     eax,es:RealRegsStruc.Real_ESI[edi]
        mov     fs:[esi+16],eax
        mov     eax,es:RealRegsStruc.Real_EDX[edi]
        mov     fs:[esi+20],eax
        mov     eax,es:RealRegsStruc.Real_ECX[edi]
        mov     fs:[esi+24],eax
        mov     eax,es:RealRegsStruc.Real_EBX[edi]
        mov     fs:[esi+28],eax
        mov     eax,es:RealRegsStruc.Real_EAX[edi]
        mov     fs:[esi+32],eax
        mov     ax,es:RealRegsStruc.Real_Flags[edi]
        mov     fs:[esi+36],ax
        mov     ax,es:RealRegsStruc.Real_IP[edi]
        mov     fs:[esi+38],ax
        mov     ax,es:RealRegsStruc.Real_CS[edi]
        mov     fs:[esi+40],ax
        ;
        ;Switch back to v86 mode.
        ;
        mov     dx,[VCPI_SP+0]
        mov     cx,[VCPI_SP+2]
        call    [Protected2Real]
        add     RawStackPos,RawStackDif
        pop     gs
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
        popf
        retf
        ;
	align 4
rv30_CallB016 label dword
rv30_CallB032 label fword
	df ?

RAWCallBack     endp

	assume ds:nothing

;-------------------------------------------------------------------------------
RAWICallBack    proc    near
        cli
        pop     cs:RetAdd               ;get return address.
        ;
        ;Check if this call back is busy.
        ;
        push    ax
        push    bx
        push    dx
        mov     ax,cs:RetAdd            ;get return address.
        sub     ax,CallBackSize ;back to start of call back entry.
        sub     ax,offset ICallBackList ;offset from start of list.
        xor     dx,dx
        mov     bx,CallBackSize
        div     bx              ;entry number.
        mov     bx,size CallBackStruc
        mul     bx              ;get offset into table.
        mov     bx,offset CallBackTable
        add     bx,ax           ;point to this entry.
        mov     cs:[rv31_CallTab],bx
        ;
        cmp     cs:InWindows,0
        jnz     rv31_ForceOld
        ;
        test    cs:CallBackStruc.CallBackFlags[bx],128  ;call back busy?
        jz      rv31_NotBusy
        ;
        ;This is a busy int call back so pass control to old real mode vector.
        ;
rv31_ForceOld:
        mov     ax,WORD PTR cs:[bx].CallBackStruc.CallBackReal+0
        mov     WORD PTR cs:[rv31_tVCPI_SP+0],ax
        mov     ax,WORD PTR cs:[bx].CallBackStruc.CallBackReal+2        ;fetch old real mode vector address.
        mov     WORD PTR cs:[rv31_tVCPI_SP+2],ax
        pop     dx
        pop     bx
        pop     ax
        jmp     DWORD PTR cs:[rv31_tVCPI_SP]    ;pass onto old handler.
        ;
        ;
rv31_NotBusy:
        or      cs:CallBackStruc.CallBackFlags[bx],128  ;mark it as busy.
        mov     bx,sp
        mov     ax,ss:[bx+(2+2+2)+(2+2)]
        and     ah,00001100b               ; clear TF,IF,IOPL,NT
        mov     WORD PTR cs:[rv31_FlagsStore+0],ax
        pop     dx
        pop     bx
        pop     ax
        ;
        ;Switch to new stack.
        ;
        mov     w cs:[rv31_tVCPI_SP+0],sp  ;store current stack.
        mov     w cs:[rv31_tVCPI_SP+2],ss
        mov     ss,cs:RawStackReal         ;use new stack.
        mov     esp,cs:RawStackPos
        sub     cs:RawStackPos,RawStackDif
        push    cs:[rv31_tVCPI_SP]         ;put old stack onto new one.
        ;
        ;Preserve registers.
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
        push    gs
        ;
        ;Make our data addresable.
        ;
        mov     ax,GROUP16
        mov     ds,ax           ;make our data addresable.
        assume ds:GROUP16
        mov     w[rv31_tVCPI_SP+0],sp
        mov     w[rv31_tVCPI_SP+2],ss
        ;
        ;Switch to protected mode.
        ;
        mov     cx,KernalSS
        movzx   edx,sp
;       mov     edx,RawStackPos
;       sub     RawStackPos,RawStackDif
        call    [Real2Protected]
        ;
        ;Get protected mode code address.
        ;
        mov     bx,[rv31_CallTab]
        mov     bl,CallBackStruc.CallBackNum[bx]        ;get int number.
        push    ax
        push    ebx
        movzx   ebx,bl
        shl     ebx,1           ;*2
        lea     ebx,[ebx*2+ebx] ;*6
        add     ebx,offset InterruptTable
        push    ds
        mov     ax,DpmiEmuDS
        mov     ds,ax
        assume ds:_cwDPMIEMU
        mov     edx,[ebx+0]             ;get offset.
        mov     cx,[ebx+4]              ;get segment selector.
        pop     ds
        assume ds:GROUP16
        pop     ebx
        pop     ax
        test    BYTE PTR RawSystemFlags,1
        jz      rv31_Use32Bit12
        mov     w[rv31_CallB016+0],dx
        mov     w[rv31_CallB016+2],cx
        jmp     rv31_Use16Bit12
rv31_Use32Bit12:
        mov     d[rv31_CallB032+0],edx
        mov     w[rv31_CallB032+4],cx
rv31_Use16Bit12:
        ;
        ;Retrieve register values.
        ;
        mov     ax,KernalZero
        mov     es,ax
        movzx   esi,w[rv31_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv31_tVCPI_SP+0]
        add     esi,eax         ;Point to stacked registers.
        pushfd
        pop     eax
        shr     eax,16
        mov     w[rv31_FlagsStore+2],ax
        mov     eax,es:[esi+32]
        mov     ebx,es:[esi+28]
        mov     ecx,es:[esi+24]
        mov     edx,es:[esi+20]
        mov     edi,es:[esi+12]
        mov     ebp,es:[esi+8]
        mov     esi,es:[esi+16]
        push    [rv31_tVCPI_SP]
        push    [rv31_CallTab]
        push    ds

        test    BYTE PTR RawSystemFlags,1
        jz      rv31_Use32Bit13

        push    w[rv31_FlagsStore]
;       pushf                   ;dummy return flags.
        push    cs              ;dummy return address.
        push    w[rv31_zero]            ;/

        push    w[rv31_FlagsStore]
        call    [rv31_CallB016]

        lea     esp,[esp+(2*3)]

;       pushf
;       add     sp,2*3
;       popf

        jmp     rv31_Use16Bit13
rv31_Use32Bit13:
;       pushfd          ;dummy return flags.
        push    d[rv31_FlagsStore]
        push    0               ;\
        push    cs              ;dummy return address.
        push    d[rv31_zero]

        push    d[rv31_FlagsStore]
        call    [rv31_CallB032]

        lea     esp,[esp+(4*3)]

rv31_Use16Bit13:
        pop     ds
        pushfd
        cli
        pop     d[rv31_FlagsStore]
        pop     [rv31_CallTab]
        pop     [rv31_tVCPI_SP]
        push    esi
        push    eax
        mov     ax,KernalZero
        mov     es,ax
        movzx   esi,w[rv31_tVCPI_SP+2]
        shl     esi,4
        movzx   eax,w[rv31_tVCPI_SP+0]
        add     esi,eax         ;Point to stacked registers.
        ;
        ;Set new register values.
        ;
        pop     eax
        mov     es:[esi+32],eax
        mov     es:[esi+28],ebx
        mov     es:[esi+24],ecx
        mov     es:[esi+20],edx
        pop     eax
        mov     es:[esi+16],eax
        mov     es:[esi+12],edi
        mov     es:[esi+8],ebp
        ;
        ;Update flags.
        ;
        movzx   eax,WORD PTR es:[(2+2+2+2)+(4+4+4+4+4+4+4)+esi]
        movzx   esi,WORD PTR es:[(2+2+2+2)+(4+4+4+4+4+4+4)+(2)+esi]
        shl     esi,4
        add     esi,eax
        mov     ax,w[rv31_FlagsStore]
        and     ah,11111000b           ;clear TF, IF & DF
        and     WORD PTR es:[(2+2)+esi],0000011100000000b
        or      es:[(2+2)+esi],ax
        ;
        mov     bx,[rv31_CallTab]      ;restore call back structure.
        and     CallBackStruc.CallBackFlags[bx],255-128 ;clear busy flag.
        ;
        ;Switch back to v86 mode.
        ;
        mov     dx,w[rv31_tVCPI_SP+0]
        mov     cx,w[rv31_tVCPI_SP+2]
        call    [Protected2Real]
;       add     RawStackPos,RawStackDif
        ;
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
        pop     eax
        lss     sp,[esp]                ;restore original stack.
        add     cs:RawStackPos,RawStackDif
        iret
;
	align 4
rv31_CallB016 label dword
rv31_CallB032 label fword
                 df ?
rv31_CallTab     dw ?
rv31_FlagsStore  dd 0
rv31_tVCPI_SP    dd ?
rv31_zero        dd 0

RAWICallBack    endp

        assume ds:GROUP16

;-------------------------------------------------------------------------------
;
;Install relevant A20 handler for this machine.
;
InstallA20      proc near
        ;
rv32_IAChkPS2:
        ; Are we on a PS/2?
        ;
        call    IsPS2Machine
        cmp     ax,1
        jne     rv32_IAOnAT
        mov     A20HandlerCall,offset A20_PS2
        jmp     rv32_0
        ;
rv32_IAOnAT:
        ;Assume we're on an AT.
        ;
        mov     A20HandlerCall,offset A20_AT
        ;
rv32_0: ret
InstallA20      endp


;-------------------------------------------------------------------------------
IsPS2Machine    proc   near
        mov     ah,0C0h         ; Get System Description Vector
        stc
        int     15h
        jc      rv33_IPMNoPS2           ; Error?  Not a PS/2.
        ;
        ; Do we have a "Micro Channel" computer?
        ;
        mov     al,byte ptr es:[bx+5]   ; Get "Feature Information Byte 1"
        test    al,00000010b            ; Test the "Micro Channel Implemented" bit
        jz      rv33_IPMNoPS2
        ;
rv33_IPMFoundIt:
        xor     ax,ax           ; Disable A20. Fixes PS2 Ctl-Alt-Del bug
        call    A20_PS2
        mov     ax,1
        ret
        ;
rv33_IPMNoPS2:
        xor     ax,ax
        ret
IsPS2Machine    endp


;-------------------------------------------------------------------------------
;
;Control A20 with whatever method is apropriate.
; AX=0: disable A20
; AX=1: enable A20
; AX=2: restore A20
;
A20Handler      proc    near
        cmp     ProtectedType,PT_RAWXMS
        jnz     rv34_A20DoneOther
        cmp     XMSPresent,0
        jz      rv34_A20Raw
        cmp     ax,2            ;restore?
        jz      rv34_XMSA20OFF
        or      ax,ax
        jnz     rv34_XMSA20ON
rv34_XMSA20OFF:
        sub     esp,size RealRegsStruc
        mov     ax,ss
        mov     es,ax
        mov     edi,esp
        mov     eax,[XMSControl]
        mov     es:[edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,6       ;disable A20 locally
        mov     w es:[edi].RealRegsStruc.Real_EAX,ax
        mov     es:[edi].RealRegsStruc.Real_SSSP,0
        call    RawSimulateFCall
        add     esp,size RealRegsStruc
        ret
        ;
rv34_XMSA20ON:
        sub     esp,size RealRegsStruc
        mov     ax,ss
        mov     es,ax
        mov     edi,esp
        mov     eax,[XMSControl]
        mov     es:[edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,5       ;enable A20 locally
        mov     w es:[edi].RealRegsStruc.Real_EAX,ax
        mov     es:[edi].RealRegsStruc.Real_SSSP,0
        call    RawSimulateFCall
        mov     eax,[XMSControl]
        mov     es:[edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,7       ;get A20 state
        mov     w es:[edi].RealRegsStruc.Real_EAX,ax
        mov     es:[edi].RealRegsStruc.Real_SSSP,0
        call    RawSimulateFCall
        mov     ax,w es:[edi].RealRegsStruc.Real_EAX
        xor     ax,1
        add     esp,size RealRegsStruc
        or      ax,ax
        ret
        ;
rv34_A20Raw:
        call    [A20HandlerCall]
        ;
rv34_A20Done:
        ret
rv34_A20DoneOther:
        xor  ax,ax
        ret
A20Handler      endp


;-------------------------------------------------------------------------------
A20_AT  proc    near
        or      ax,ax
        jz      rv35_AAHDisable
        cmp     ax,2
        jnz     rv35_AAHEnable
        cmp     A20Flag,0
        jz      rv35_AAHDisable
rv35_AAHEnable:
        call    rv35_Sync8042           ; Make sure the Keyboard Controller is Ready
        jnz     rv35_AAHErr
        mov     al,0D1h         ; Send D1h
        out     64h,al
        call    rv35_Sync8042
        jnz     rv35_AAHErr
        mov     al,0DFh         ; Send DFh
        out     60h,al
        call    rv35_Sync8042
        jnz     rv35_AAHErr
        ;
        ; Wait for the A20 line to settle down (up to 20usecs)
        ;
        mov     al,0FFh         ; Send FFh (Pulse Output Port NULL)
        out     64h,al
        call    rv35_Sync8042
        jnz     rv35_AAHErr
        jmp     rv35_AAHExit
        ;
rv35_AAHDisable:
        call    rv35_Sync8042           ; Make sure the Keyboard Controller is Ready
        jnz     rv35_AAHErr
        mov     al,0D1h         ; Send D1h
        out     64h,al
        call    rv35_Sync8042
        jnz     rv35_AAHErr
        mov     al,0DDh         ; Send DDh
        out     60h,al
        call    rv35_Sync8042
        jnz     rv35_AAHErr
        ;
        ; Wait for the A20 line to settle down (up to 20usecs)
        ;
        mov     al,0FFh         ; Send FFh (Pulse Output Port NULL)
        out     64h,al
        call    rv35_Sync8042
rv35_AAHExit:
        xor     ax,ax
        ret
rv35_AAHErr:
        mov     ax,1
        or      ax,ax
        ret
        ;
rv35_Sync8042:
        xor     cx,cx
rv35_S8InSync:
        in      al,64h
        and     al,2
        loopnz rv35_S8InSync
        ret
A20_AT  endp


;-------------------------------------------------------------------------------
A20_PS2 proc    near
PS2_PORTA       equ     0092h
PS2_A20BIT      equ     00000010b
        or      ax,ax
        jz      rv36_PAHDisable
        cmp     ax,2
        jnz     rv36_PAHEnable
        cmp     A20Flag,0
        jz      rv36_PAHDisable
        ;
rv36_PAHEnable:
        in      al,PS2_PORTA            ; Get the current A20 state
        test    al,PS2_A20BIT   ; Is A20 already on?
        jnz     rv36_PAHErr
        ;
        or      al,PS2_A20BIT   ; Turn on the A20 line
        out     PS2_PORTA,al
        ;
        xor     cx,cx           ; Make sure we loop for awhile
rv36_PAHIsItOn:
        in      al,PS2_PORTA            ; Loop until the A20 line comes on
        test    al,PS2_A20BIT
        loopz   rv36_PAHIsItOn
        jz      rv36_PAHErr             ; Unable to turn on the A20 line
        jmp     rv36_PAHExit
        ;
rv36_PAHDisable:
        in      al,PS2_PORTA            ; Get the current A20 state
        and     al,NOT PS2_A20BIT       ; Turn off the A20 line
        out     PS2_PORTA,al
        ;
        xor     cx,cx           ; Make sure we loop for awhile
rv36_PAHIsItOff:
        in      al,PS2_PORTA            ; Loop until the A20 line goes off
        test    al,PS2_A20BIT
        loopnz rv36_PAHIsItOff
        jnz     rv36_PAHErr             ; Unable to turn off the A20 line
        ;
rv36_PAHExit:
        xor     ax,ax
        ret
        ;
rv36_PAHErr:
        mov     ax,1
        or      ax,ax
        ret
A20_PS2 endp

_cwRaw  ends

