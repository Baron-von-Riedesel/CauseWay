;------------------------------------------------------------------------------
;
;Some equates to make stacked register access simpler.
;--- it's for a) DPMI API ( file api.inc, proc cwAPIpatch )
;---          b) DOS API ( file int21h.inc, proc Int21h )
;
Int_EDI	equ	0
Int_DI	equ	0
Int_ESI	equ	4
Int_SI	equ	4
Int_EBP	equ	8
Int_BP	equ	8
Int_ESP	equ	12
Int_SP	equ	12
Int_EBX	equ	16
Int_BX	equ	16
Int_BL	equ	16
Int_BH	equ	17
Int_EDX	equ	20
Int_DX	equ	20
Int_DL	equ	20
Int_DH	equ	21
Int_ECX	equ	24
Int_CX	equ	24
Int_CL	equ	24
Int_CH	equ	25
Int_EAX	equ	28
Int_AX	equ	28
Int_AL	equ	28
Int_AH	equ	29
Int_GS	equ	32
Int_FS	equ	36
Int_ES	equ	40
Int_DS	equ	44
Int_Off	equ	48
Int_Seg16	equ	50
Int_Seg32	equ	52
Int_Flags16	equ	52
Int_Flags32	equ	56

;; MED 02/16/96
MaxCallBacks	equ	32+3	;maximum number of call back table entries.
;MaxCallBacks	equ	32+4	;maximum number of call back table entries.

;; MED increased 512 to 1024, 12/05/95
IFDEF BIGSTACK
RawStackDif		equ	5*1024	;size of interrupt stacks.
ELSE
;RawStackDif	equ	1280	;size of interrupt stacks.
RawStackDif		equ	1024	;size of interrupt stacks.
;RawStackDif	equ	512		;size of interupt stacks.
ENDIF

;--- RawStackTotal must be a multiple of 16
IFDEF BIGSTACK
RawStackTotal	equ	(10*RawStackDif)+RawStackDif
ELSE
RawStackTotal	equ	8192+RawStackDif
ENDIF

tPL2StackSize	equ	0		;other priveledge level stacks. never used.
tPL1StackSize	equ	0
tPL0StackSize	equ	256		;Interupt catcher stack.


GDTNul		equ	00h			;Null entry.
;
VCPI_0		equ	08h			;blank entries for VCPI to fill in.
VCPI_1		equ	10h			;/
VCPI_2		equ	18h			;/
;
;KernalTS		equ	20h+3	;Kernal TSS
KernalTS		equ	20h		;Kernal TSS
KernalCS0		equ	28h		;Kernal PL0 code seg.
KernalSS0		equ	30h		;Kernal PL0 stack (in TSSFields)
KernalSS0Switch	equ	38h		;Mode switch PL0 stack (in _cwStack; 64k limit).
Kernal40h		equ	40h+3	;DOS/BIOS data area access.
KernalPL3toPL0	equ	48h+3	;PL3 to PL0 call gate.
;
KernalLDT		equ	50h+3
KernalCS		equ	58h+3	;Kernal CS
KernalDS		equ	60h+3	;Kernal DS
KernalSS		equ	68h+3	;Kernal SS
KernalZero		equ	70h+3	;Kernal 0-4G data referance.
;
MainSS			equ	78h+3	;Main stack seg (_cwStack).
MainPSP			equ	80h+3	;PSP segment.
MainEnv			equ	88h+3	;Enviroment segment.
;MainCS			equ	78h+3	;Main code seg.
;MainDS			equ	80h+3	;Main data seg.
;MainCS0			equ	0a0h	;code seg at PL0.
;MainPL3toPL0	equ	0a8h+3	;call gate
MainCS			equ	KernalCS
MainDS			equ	KernalDS
MainCS0			equ	KernalCS0
MainPL3toPL0	equ	KernalPL3toPL0
;
DpmiEmuCS		equ	090h+3	;DPMI emulator code seg.
DpmiEmuCS0		equ	098h	;DPMI emulator code seg at PL0.
DpmiEmuDS		equ	0a0h+3	;DPMI emulator data seg.
DpmiEmuPL3toPL0	equ	0a8h+3	;call gate
;
GDTData			equ	0b0h+3	; GDT selector r/w
;
ifdef KRNLDBG
KDbgCS			equ	0b8h
GDT_Entries		equ	0d0h/8
else
GDT_Entries		equ	0b8h/8	;GDT entries to allocate.
endif
;
;
CallBackStruc struc
CallBackFlags	db ?	; b[0]:1=in use;b[7]:1=busy
CallBackNum		db ?	; interrupt#
CallBackOff		dw ?
CallBackProt	df ?
CallBackRegs	df ?
CallBackReal	dd ?
CallBackStackSel	dw ?
				dw ?
CallBackStruc ends
;