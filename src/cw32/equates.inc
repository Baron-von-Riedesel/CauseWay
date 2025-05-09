
;------------------------------------------------------------------------------

MaxCallBacks	equ	16	;maximum number of call back table entries.

;; MED increased 512 to 1024, 12/05/95
IFDEF BIGSTACK
RawStackDif		equ	5*1024	;size of interrupt stacks.
ELSE
;RawStackDif	equ	1280	;size of interrupt stacks.
RawStackDif		equ	1024	;size of interrupt stacks.
;RawStackDif	equ	512		;size of interrupt stacks.
ENDIF

;--- RawStackTotal must be a multiple of 16
IFDEF BIGSTACK
RawStackTotal	equ	(10*RawStackDif)+RawStackDif
ELSE
RawStackTotal	equ	8192+RawStackDif
ENDIF

tPL2StackSize	equ	0		;other priveledge level stacks. never used.
tPL1StackSize	equ	0
tPL0StackSize	equ	256		;Interrupt catcher stack.


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
KernalSS		equ	68h+3	;Kernal SS (size RawStackTotal in raw/vcpi)
KernalZero		equ	70h+3	;Kernal 0-4G data referance.
;
MainSS			equ	78h+3	;Main stack seg (_cwStack).
MainPSP			equ	80h+3	;PSP segment.
MainEnv			equ	88h+3	;Enviroment segment.
;MainCS			equ	78h+3	;Main code seg.
;MainDS			equ	80h+3	;Main data seg.
MainCS			equ	KernalCS
MainDS			equ	KernalDS
MainPL3toPL0	equ	KernalPL3toPL0
;
DpmiEmuCS		equ	090h+3	;DPMI emulator code seg.
DpmiEmuDS		equ	098h+3	;DPMI emulator data seg.
DpmiEmuCS0		equ	0A0h	;DPMI emulator code seg at PL0.
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

if VIDEOGDTSELS
KernalB000		equ	0b000h+3
KernalB800		equ	0b800h+3
KernalA000		equ	0a000h+3
endif
;
