
;
;The RAW/VCPI specific 16-bit code, remains resident in conv. memory.
;
        .386P

PAGESTACKSIZE   equ 32

_cwRaw  segment para public 'raw kernal code' use16

_cwRaw$1 segment dword public 'raw kernal data' use16
;
SDAaddress      dd ?      ; lin. address of DOS SDA; set during Setup (int 21h, ax=5d06)
OldInt2Fr       dd 0      ; saved IVT vector 2Fh
VMMHandle       dw ?      ; file handle for swapfile
InWindows       db 0      ; 1 while "enhanced Windows" is running
VMMName db '?:\',128 dup (0)
;
	align 2
ProposedPresentFlag db 0	;memory.inc - used by UnmapPhysical()
ProposedRecentFlag db 0		;memory.inc - used by UnmapPhysical()
	align 4
ProposedPage    dd 0	;memory.inc - used by UnmapPhysical()
CompareCount    dd 0	;memory.inc - used by UnmapPhysical()
PageingPointer  dd 0	;memory.inc - used by UnmapPhysical()

FreePages       dd 1	;memory.inc - free virtual pages
AllocedPages    dd 0	;memory.inc - allocated virtual pages
UnLockedPages   dd 0	;memory.inc - unlocked physical pages
TotalPages      dd 1	;memory.inc - physical pages used for virt. backup

RecentMapStack  dd PAGESTACKSIZE dup (0)	;memory.inc
;_LM_BlockBase   dd ?	;memory.inc - used by Raw[Un]LockMemory
;_LM_BlockEnd    dd ?	;memory.inc
;_LM_Needed      dd ?	;memory.inc
_LM_Got         dd ?	;memory.inc
TotalPhysical   dd 0    ;free phys. pages reported by VCPI/XMS/INT15/DOS
;
SwapFileLength  dd 0
;
RawSelBuffer    Desc <>
;
;
XMSMEMITEM struc
wHdl     dw ?
dwBase   dd ?
dwEnd    dd ?
XMSMEMITEM ends

MemIntBuffer    RealRegsStruc <>
XMSList         XMSMEMITEM XMSITEMS dup (<0,0,0>)
XMSTempList     dd XMSITEMS dup (0)
XMSTotal        dd 0

;--- CAUSEWAY=LOWMEM:xxx default
;--- the stored value is in paragraph units, while the xxx in LOWMEM:xxx are kB
IFNDEF DEFLOWMEM
DEFLOWMEM equ 32   ;default 32kB
endif

if DEFLOWMEM LT 640

CONVITEMS equ 32

DOSMEMITEM struc
wAddr    dw ?
wSize    dw ?
DOSMEMITEM ends

ConventionalList DOSMEMITEM CONVITEMS dup (<0,0>)
ConvTempList    dw CONVITEMS dup (0)
ConvTotal       dd 0
ConvSavePara    dw 0

ConvSaveSize    dw DEFLOWMEM*64 ;*64 -> convert kB to paragraphs
endif

INT15ITEMS      equ 8

Int15Table      dd INT15ITEMS*2 dup (0)
Int15Size       dd 0
INT15hTotal     dd ?
Int15hValue     dd ?
INT15hLevel2    dw ?
;
;--- mode switch routines; default is VCPI, changed to RawProt2Real/RawReal2Prot in raw mode.
;--- near proc Protected2Real is called by 1) RawVCPIRealMode, 2) SwitchProcPM, 3) Raw[I]CallBack

fProtected2Real  label PF16
Protected2Real	dw offset VCPIProt2Real, KernalCS
Real2Protected	dw offset VCPIReal2Prot
;
A20HandlerCall  dw ?          ; offset A20 handler (PS2/AT KBC); used if no XMS host found
A20Flag         db 0          ; initial state A20 (enabled/disabled)
XMSPresent      db 0
XMSControl      dd ?          ; XMS entry; real-mode far16 address
XMSBlockSize    dw ?          ; size in KB to alloc XMS ext. memory blocks
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

    align 4
VCPI_Entry label fword      ;VCPI entry protected-mode FAR32 address
        dd ?
        dw VCPI_0
;
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
Page1stLinear   dd ?            ;linear address page table 0
PageDirLinear   dd ?            ;linear address page dir
PageAliasLinear dd ?            ;linear address page table for page mappings (FFC00000h)
PageDETLinear   dd 0            ;linear address page table for page detail region (FF800000h)

ifdef KRNLDBG
PhysPageSave    dd 4+MOVEPT0TOEXT dup (0)   ;save max 4(5) VCPI pages to be released in real-mode only
else
PhysPageSave    dd 3+MOVEPT0TOEXT dup (0)   ;save max 3(4) VCPI pages to be released in real-mode only
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
	align 4
;--- callback tables
;--- ICallxx = for internal callbacks: IRQs, Int 1Ch/23h/24h
;
ICallBackUsed   dd 0
ICallBackBusy   dd 0
ICallBackOldVec dd 16+3 dup (0)
ICallBackIntNo  db 16+3 dup (-1)

	align 4

;--- CallBackStruc is for external callbacks; 2 are allocated before the app gets control:
;--- 1. mouse event (always allocated, even if no mouse driver installed)
;--- 2. Int 1Bh (always allocated)

CallBackStruc struc
CallBackFlags	db ?	; [0]:1=in use
                db ?    ; not used
CallBackProt	df ?    ; FAR32 protected-mode CS:EIP to call
CallBackRegs	df ?    ; FAR32 address of RMCS
CallBackStackSel dw ?   ; stack selector
CallBackStruc ends
;
CallBackTable   CallBackStruc MaxCallBacks dup (<0,0,0,0,0>)
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
	align 4
MaxMemLin       dd 1021 shl 20	; "MAXMEM" environment setting (max virtual memory, default 1021 MB!)
MaxMemPhys      dd -1           ; "HIMEM" environment setting
;
PreAllocSize    DD 0            ; size of VMM disk preallocation
ExtALLSwitch    db 0
NoPassFlag      DB 0            ; nonzero if not passing real mode hardware interrupts up to protect mode
DesiredVMMName  DB 13 DUP (0)   ; VMM name desired by user via CAUSEWAY e-var
	align 2
FirstUninitPage DW 0
	align 4
rv16_Return   dd ?
rv18_Return   dd ?
rv29_rmsp     dw ?,?    ;for RAWSimulate(): store real-mode SP in first word
rv1_RetAdd    dw ?
rv17_Return   dw ?
rv19_Return   dw ?
rv20_Return   dw ?
rv30_CallTab  dw ?      ;RawCallBack
rv30_rmsp     dw ?,?    ;RawCallBack
	align 4
rv31_CallB016 label PF16
rv31_CallB032 label fword
                 df ?
rv31_CallTab     dw ?   ;RawIntCallBack
rv31_FlagsStore  dd 0   ;RawIntCallBack: saved efl
rv31_rmsp        dd ?   ;RawIntCallBack: saved rm stack
;rv31_zero        dd 0   ;RawIntCallBack: ???

rvsp_dsreg    dw ?      ;raw mode switch
rvsp_esreg    dw ?      ;raw mode switch
rvsp_csreg    dw ?      ;raw mode switch
wP2LIndex     dw DETINDEX   ;current bottom PD index for Phys2Linear mappings

ITABSTRUC struct
dwOldVec dd ?
dwMemSiz dd ?
ITABSTRUC ends

	align 4
Int15Jump     PF16 ?
ITable  ITABSTRUC INT15ITEMS dup (<0,0>)
ILevel  dw 0
_cwRaw$1 ends

        assume ds:GROUP16

;-------------------------------------------------------------------------------

;--- final switch back to real-mode
;--- ES: flat 4G
;--- DS: KernalDS

RawVCPIRealMode proc near
        pop     [rv1_RetAdd]   ; pop return address
;
;Disable hardware INT call-backs.
;
        mov     edx,ICallBackUsed ;list of call backs.
        mov     bx,offset ICallBackIntNo
        mov     si,offset ICallBackOldVec
rv1_6:
        lodsd
        shr     edx,1
        jnc     rv1_7
        push    bx
        movzx   bx,b [bx]    ;get Int#
        shl     bx,2
        mov     es:[bx],eax
        pop     bx
rv1_7:
        inc     bx
        and     edx,edx
        jnz     rv1_6
;
;Release VCPI memory.
;
        call    VCPIRelExtended ;release VCPI memory.

ife RELXMSINRM

;Release XMS memory while in protected-mode

        call    RawRelXMS       ;release XMS memory.
endif
;
;Release INT 15h memory.
;
        call    Int15Rel        ;release int 15 vectors.
;
;Switch back to real mode.
;
        mov     dx, offset _cwStackEnd
        mov     cx, _cwStack
        call    [Protected2Real]
;
;Restore A20 state.
;
        mov     ax,A20_RESTORE
        call    A20Handler
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
        call    RawRelXMS               ;release XMS memory.
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
;
;Simulate a real-mode far call
;called by RawRelXMS if RELXMSINRM==0
;called by FreeVCPIPage if VCPIPMCALL==0
;
if (RELXMSINRM eq 0) or (VCPIPMCALL eq 0)
fRawSimulateFCall proc near
        db 66h,9ah
        dd offset _ffRawSimulateFCall
        dw DpmiEmuCS
        ret
fRawSimulateFCall endp
endif
;-------------------------------------------------------------------------------
;
;Allocate a page of physical memory.
;called by _cwInit() only!
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
;called by _cwInit() only!
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

_cwRaw$1 segment
	align 2
Int15PatchTable label word
        dw Int15Patch0,Int15Patch1,Int15Patch2,Int15Patch3
        dw Int15Patch4,Int15Patch5,Int15Patch6,Int15Patch7
_cwRaw$1 ends

;-------------------------------------------------------------------------------
;--- these are Int 15h real-mode entries for AH=88h/AX=E801h

Int15Patch0:
        push    esi
        mov     si,offset ITable+0*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch1:
        push    esi
        mov     si,offset ITable+1*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch2:
        push    esi
        mov     si,offset ITable+2*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch3:
        push    esi
        mov     si,offset ITable+3*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch4:
        push    esi
        mov     si,offset ITable+4*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch5:
        push    esi
        mov     si,offset ITable+5*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch6:
        push    esi
        mov     si,offset ITable+6*sizeof ITABSTRUC
        jmp     Int15Patch
Int15Patch7:
        push    esi
        mov     si,offset ITable+7*sizeof ITABSTRUC
        jmp     Int15Patch
        ;
        assume ds:nothing
Int15Patch:
        cmp     ah,88h          ;get memory size?
        jnz     Checke801h
        mov     eax,cs:[si].ITABSTRUC.dwMemSiz   ;IIRC only AX does return a value (extended memory in kB)

Int15PatchRet:
        pop     esi
        iret                    ;IIRC carry flag should be cleared on success

Checke801h:
        cmp     ax,0e801h
        jnz     Int15Old
        xor     bx,bx
        mov     eax,cs:[si].ITABSTRUC.dwMemSiz   ; get 32-bit memory size 1K pages
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
        mov     esi,cs:[si].ITABSTRUC.dwOldVec
        mov     cs:[Int15Jump],esi
        pop     esi
        jmp     cs:[Int15Jump]          ;pass to old handler.

        assume ds:GROUP16

;-------------------------------------------------------------------------------
;
;Release any XMS we claimed.
;
RawRelXMS       proc    near
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
        mov     [edi].RealRegsStruc.Real_AX,ax
        mov     [edi].RealRegsStruc.Real_DX,dx
        mov     [edi].RealRegsStruc.Real_SSSP,0
        call    fRawSimulateFCall        ;un-lock EMB (if RELXMSINRM=0)
        pop     dx
        mov     eax,[XMSControl]
        mov     [edi].RealRegsStruc.Real_CSIP,eax
        mov     ah,0ah
        mov     [edi].RealRegsStruc.Real_AX,ax
        mov     [edi].RealRegsStruc.Real_DX,dx
        mov     [edi].RealRegsStruc.Real_SSSP,0
        call    fRawSimulateFCall        ;free EMB (if RELXMSINRM=0)
else
        mov     ah, 0dh
        call    [XMSControl]
        mov     ah, 0ah
        call    [XMSControl]
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
RawRelXMS       endp

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
        mov     RealRegsStruc.Real_AX[edi],ax
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     RealRegsStruc.Real_IP[edi],offset Int67h
        mov     RealRegsStruc.Real_CS[edi],GROUP16
        mov     RealRegsStruc.Real_SSSP[edi],0
        call    fRawSimulateFCall    ; free VCPI page (if VCPIPMCALL=0)

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
;Still in protected mode.
;DS=GROUP16
;
VCPIRelExtended proc near

        cmp     ProtectedType,PTYP_VCPI
        jnz     rv12_9
        ;
        mov     ax,KernalZero
        mov     es,ax
        cmp     PageDETLinear,0
        jz      rv12_NoDET
        ;
        ;Release DET pages.
        ;
        mov     cx,1024
        mov     esi,PageDETLinear
rv12_d0:
        btr     w es:[esi],PT_PRESBIT
        jnc     rv12_d1
        mov     edx,es:[esi]
        test    dx,1 shl PT_VCPIBIT    ;VCPI bit set?
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
        mov     esi,PageDirLinear
        mov     cx,1024-2               ;don't check the last 2 PDEs (page table mappings, page details)
        mov     edi,PTMAPADDR           ;base of page alias's.
rv12_0:                                 ;<--- next PDE
        mov     eax,es:[esi]
        test    al,1 shl PT_PRESBIT     ;Page table present?
        jz      rv12_4

        push    cx
        push    edi
        mov     cx,1024
        cmp     edi,PTMAPADDR           ; see if 0th page table
        jne     rv12_2                  ; no
        movzx   eax,FirstUninitPage     ; don't scan pages reserved by VCPI host
        add     edi,eax
        shr     ax,2
        sub     cx,ax
rv12_2:                                 ;<--- next PTE
        mov     edx,es:[edi]
        test    dl,1 shl PT_PRESBIT     ;Page present?
        jz      rv12_3
        test    dx,1 shl PT_VCPIBIT     ;VCPI bit set?
        jz      rv12_3
        and     b es:[edi],not (1 shl PT_PRESBIT)  ;reset present bit
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
rv12_4:
        mov     edx,es:[esi]
        test    dx,1 shl PT_VCPIBIT     ;VCPI bit set?
        jz      rv12_1
        and     b es:[esi],not (1 shl PT_PRESBIT)       
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
;ES=zero-based flat
;
Int15Rel        proc    near
        cmp     ProtectedType,PTYP_VCPI
        jnc     rv15_9      ; skip if VCPI or DPMI
        cmp     XMSPresent,0
        jnz     rv15_9
        ;
        mov     si,offset ITable + sizeof ITABSTRUC * (INT15ITEMS - 1)
        mov     cx,INT15ITEMS
rv15_0:
        mov     eax,[si].ITABSTRUC.dwOldVec
        cmp     eax,0
        jz      rv15_1
        mov     es:[15h*4],eax
rv15_1:
        sub     si,sizeof ITABSTRUC
        loop    rv15_0
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
        mov     eax,vcpi._CR3   ;physical address page directory
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
        ;
        call    RawPL3toPL0

        mov     eax,cr0
        mov     CR0ProtSav,eax  ; save protected mode cr0 status

        mov     ax,KernalSS0Switch ;SS must be PL0, 16-bit and 64k limit
        mov     ss,ax
        mov     ax,KernalDS
        mov     ds,ax           ;DS,ES,FS,GS,SS must be data with 64k limit
        mov     es,ax           ;expand up, read/write for switch back to real
        mov     fs,ax           ;mode.
        mov     gs,ax
        ;
        mov     eax,CR0Sav              ;Get machine control &
; MED 10/15/96, don't clear emulate math coprocessor bit
;       and     eax,07FFFFFF2h  ;clear PM,EM,TS & PG bits.
        and     eax,07FFFFFF6h  ;clear PM,TS & PG bits.
        mov     cr0,eax

        db 0eah                 ;immediate 16-bit far jump, to clear
        dw rv17_Resume,GROUP16  ;instruction pre-fetch & load CS.
rv17_Resume:

        mov     ax,cs
        mov     ds,ax
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
        ;
RawProt2Real    endp


;-------------------------------------------------------------------------------
;
;Real to protected mode PL3 switch, VCPI.
;
;On Entry:-
;
;--- CX:EDX - target stack.
;--- ss:[sp] - far16 protected-mode address protected-mode
;--- modifies AX,ESI
;
VCPIReal2Prot   proc    near
        pop     [rv18_Return]
;        mov     vcpi._CS,KernalCS0
;        mov     vcpi._EIP,offset VCPI_ResumeProt
        mov     ax,0de0ch
        mov     esi,GROUP16
        shl     esi,4
        add     esi,offset vcpi
        int     67h             ; VCPI call 0de0ch may alter EAX and ESI
        ;
VCPI_ResumeProt::
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
VCPIProt2Real   endp

        assume ds:GROUP16

;-------------------------------------------------------------------------------
;--- used by RawReal2Prot & VCPIReal2Prot
;--- CX:EDX=SS:ESP

RawPL0toPL3      proc    near
        pop     [rv20_Return]
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
        push    [rv20_Return]   ;Loword(EIP)
        iretd
        ;

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
        assume ds:GROUP16
        test    BYTE PTR SystemFlags,SF_16BIT
        jz      @F
        push    edi
        movzx   edi,di
        call    ssproc
        pop     edi
        pop     ds
        retw
@@:
        call    ssproc
        pop     ds
        retd
        ;
StateSavePM   endp

StateSaveRM   proc    far
        push    ds
        push    edi
        movzx   edi,di
        push    cs
        pop     ds
        call    ssproc
        pop     edi
        pop     ds
        ret
StateSaveRM   endp

        assume ds:GROUP16

ssproc proc        
        cmp     al,0
        jz      sspm_Save
        cmp     al,1
        jz      sspm_Restore
        ret
sspm_Save:
        push    [RawStackPos]
        pop     d es:[edi]
        ret
sspm_Restore:
        push    d es:[edi]
        pop     [RawStackPos]
        ret
ssproc endp

;--- raw switch to real-mode
;--- ax=ds, cx=es, dx:bx=ss:sp, si:di=cs:ip

SwitchProcPM   proc    far
        pushf
        cli
        push    KernalDS
        pop     ds
        mov     [rvsp_dsreg],ax
        mov     [rvsp_esreg],cx
;--- Protected2Real expects new ss:sp in CX:DX
        mov     cx,dx
        mov     dx,bx
        pop     bx
        call    [Protected2Real]
        mov     es,[rvsp_esreg]
        mov     ds,[rvsp_dsreg]
        push    bx
        push    si
        push    di
        iret
SwitchProcPM   endp

;--- raw switch to protected-mode
;--- ax=ds, cx=es, dx:(e)bx=ss:(e)sp, si:(e)di=cs:(e)ip

SwitchProcRM   proc    far
        pushf
        cli
        push    cs
        pop     ds
        mov     [rvsp_dsreg],ax
        mov     [rvsp_esreg],cx
        mov     [rvsp_csreg],si
;--- Real2Protected expects new ss:esp in CX:EDX
        mov     cx,dx
        mov     edx,ebx
        test    BYTE PTR SystemFlags,SF_16BIT
        jz @F
        movzx   edx,dx
        movzx   edi,di
@@:
        pop     bx
        push    KernalCS
        call    [Real2Protected]
        mov     es,[rvsp_esreg]
        movzx   esi,[rvsp_csreg]
        mov     ds,[rvsp_dsreg]
        push    esi
        push    edi
        test    bh,2
        jz      @F
        sti
@@:
        retd
SwitchProcRM   endp

if 0 ; not used, it's done in RawVCPIRealMode()
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
endif

;-------------------------------------------------------------------------------
;
;Intercept for windows init broadcast (int 2Fh real-mode)
;
Int2FPatch      proc    near
;
;check if it's an init broadcast that's being allowed.
;
        cmp     ax,1605h
        jnz     no_1605
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
        assume ds:GROUP16
        inc     InWindows
        pop     ds
        jmp     rv23_ret
;
;check if it's an exit broadcast.
;
no_1605:
        cmp     ax,1606h
        jnz     no_1606
        test    dx,1
        jnz     rv23_ret
        ;
        push    ds
        push    cs
        pop     ds
        dec     InWindows
        pop     ds
no_1606:
if 0;DPMIDBG
        cmp     ax,1687h
        jnz     rv23_ret
Patch2f1687 equ $-2
        mov     es,cs
        mov     di,offset initjmp2pm
        clc
        retf 2
endif
rv23_ret:
;
;Let previous handlers have a go at it.
;
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

        jmp     cs:[OldInt2Fr]

WinMessage      db 'Cannot run Windows in enhanced mode while a CauseWay application is active.',13,10
        db 'Run Windows in standard mode or remove the CauseWay application.',13,10,'$'
endif

Int2FPatch      endp

;-------------------------------------------------------------------------------
;
;Control A20 with whatever method is apropriate.
; AX=0: disable A20
; AX=1: enable A20
; AX=2: restore A20
;
A20Handler      proc    near
        assume ds:GROUP16
        cmp     ProtectedType,PTYP_RAWXMS
        jnz     rv34_A20DoneOther
        cmp     XMSPresent,0
        jz      rv34_A20Raw
        cmp     ax,2            ;restore?
        jz      rv34_XMSA20OFF
        or      ax,ax
        jnz     rv34_XMSA20ON
rv34_XMSA20OFF:
        mov     ah,6       ;disable A20 locally
        call    [XMSControl]
        ret
        ;
rv34_XMSA20ON:
        mov     ah,5       ;enable A20 locally
        call    [XMSControl]
        mov     ah,7       ;get A20 state
        call    [XMSControl]
        xor     ax,1
        or      ax,ax
        ret
        ;
rv34_A20Raw:
        call    [A20HandlerCall]
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
        cmp     A20Flag,0       ; initially disabled?
        jz      rv35_AAHDisable
rv35_AAHEnable:
        call    rv35_Sync8042   ; Make sure the Keyboard Controller is Ready
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
        call    rv35_Sync8042   ; Make sure the Keyboard Controller is Ready
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
        cmp     A20Flag,0          ;initially disabled?
        jz      rv36_PAHDisable
        ;
rv36_PAHEnable:
        in      al,PS2_PORTA    ; Get the current A20 state
        test    al,PS2_A20BIT   ; Is A20 already on?
        jnz     rv36_PAHErr
        ;
        or      al,PS2_A20BIT   ; Turn on the A20 line
        out     PS2_PORTA,al
        ;
        xor     cx,cx           ; Make sure we loop for awhile
rv36_PAHIsItOn:
        in      al,PS2_PORTA    ; Loop until the A20 line comes on
        test    al,PS2_A20BIT
        loopz   rv36_PAHIsItOn
        jz      rv36_PAHErr     ; Unable to turn on the A20 line
        jmp     rv36_PAHExit
        ;
rv36_PAHDisable:
        in      al,PS2_PORTA    ; Get the current A20 state
        and     al,NOT PS2_A20BIT; Turn off the A20 line
        out     PS2_PORTA,al
        ;
        xor     cx,cx           ; Make sure we loop for awhile
rv36_PAHIsItOff:
        in      al,PS2_PORTA    ; Loop until the A20 line goes off
        test    al,PS2_A20BIT
        loopnz rv36_PAHIsItOff
        jnz     rv36_PAHErr     ; Unable to turn off the A20 line
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

