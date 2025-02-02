;------------------------------------------------------------------------------
;

ResHead struc
ResHead_Prev    dd 0
ResHead_Next    dd 0
ResHead_Handle  dd 0
ResHead ends

ResSize	equ	4096		;Size of resource block.
;ResHead	equ	16		;Size of main header.
ResHeadSize	equ	16		;Size of main header.
ResNum	equ	((((4096-ResHeadSize)/5) shr 2) shl 2)	;Number of entries.
ResCount	equ	ResNum-4		;count for free entry checking.

Res_NULL	equ	0	;MUST be zero.
Res_PSP	equ	1	;single entry.
Res_SEL	equ	2	;single entry.
Res_MEM	equ	3	;3 entries.
Res_LOCK	equ	4	;2 entries.
Res_DOSMEM	equ	5	;?
Res_CALLBACK	equ	6	;3 entries.
Res_NOTHING	equ	254
Res_CHAIN	equ	255	;single entry.



MEM_FREE	equ	0	;Free page.
MEM_START	equ	1	;Used page, start of block.
MEM_END	equ	2	;Used page, end of block.
MEM_MASK	equ	3
;
MEM_LOCK	equ	4	;Start of lock count.
MEM_LOCK_MASK	equ	15	;4 bit lock count.
MEM_LOCK_SHIFT	equ	2
;
MEM_PHYS	equ	32	;physical mapping.
;
MEM_FILL	equ	(MEM_LOCK_MASK shl MEM_LOCK_SHIFT)+MEM_START

;
KernalB000		equ	0b000h+3
KernalB800		equ	0b800h+3
KernalA000		equ	0a000h+3

;
Desc		struc
Limit		dw 0		;Offset of last byte.
Base_l		dw 0		;Low 16 bits of 32-bit address.
Base_m		db 0		;Bits 16-23 of base.
Access		db 0		;Access privaledge level.
Gran		db 0		;Granularity and limit.
Base_H		db 0		;bits 24-31 of base.
Desc		ends

DescPresent		equ	1 shl 7	;Segment is present.
Desc32Bit		equ	1 shl 6	;Segment is 32 bit (not 16!).
DescAvailable	equ	1 shl 4	;Available for our use.
;
DescPL0		equ	0 shl 5	;PL0
DescPL1		equ	1 shl 5	;PL1
DescPL2		equ	2 shl 5	;PL2
DescPL3		equ	3 shl 5	;PL3
;
DescMemory		equ	1 shl 4	;Segment is memory (not system gate etc.)
;
DescRData		equ	0	;Read only data segment. (eg, ROM)
DescRWData		equ	2	;Read/Write data.
DescRDataDown	equ	4	;Read only expand down data segment.
DescRWDataDown	equ	6	;Read/write expand down data segment.
DescECode		equ	8	;Execute only code.
DescERCode		equ	10	;Execute/read code.
DescECCode		equ	12	;Execute only conforming code.
DescERCCode		equ	14	;Execute/read conforming code.
;
Desc286Tss		equ	1	;Available 286 TSS.
DescLDT		equ	2	;LDT.
Desc286TssB		equ	3	;Busy 286 TSS.
Desc286Call		equ	4	;286 call gate.
DescTssGate		equ	5	;TSS gate.
Desc286Int		equ	6	;286 interupt gate.
Desc286Trap		equ	7	;286 trap gate.
Desc386Tss		equ	9	;Available 386 TSS.
Desc386TssB		equ	11	;Busy 386 TSS.
Desc386Call		equ	12	;386 call gate.
Desc386Int		equ	14	;386 interupt gate.
Desc386Trap		equ	15	;386 trap gate.

GATE struc
OfsLow	dw ?
sel		dw ?
attr	dw ?
OfsHigh	dw ?
GATE ends

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
TSSFields	struc
;
;Task segment structure.
;
Back	dw ?,?  	;Back link to any previous TSS.
ESP0	dd ?		;SP for level 0 interupt.
SS0		dw ?,?
ESP1	dd ?		;Level 1 interupt handlers stack.
SS1		dw ?,?
ESP2	dd ?		;Level 2 interupt handlers stack.
SS2		dw ?,?
tCR3	dd ?		;CR3 value.
tEIP	dd ?		;Instruction pointer.
tEFlags	dd ?		;EFlags.
tEAX	dd ?		;AX for init.
tECX	dd ?		;CX for init.
tEDX	dd ?		;DX for init.
tEBX	dd ?
tESP	dd ?
tEBP	dd ?
tESI	dd ?
tEDI	dd ?
tES		dd ?
tCS		dd ?
tSS		dd ?
tDS		dd ?
tFS		dd ?
tGS		dd ?
tLDT	dw ?
		dw ?		;reserved.
		dw ?		;reserved.
IOMap	dw ?		;displacement to I/O map.
;
ifdef tPL2StackSize
if tPL2StackSize
tPL3Stack	db tPL2StackSize dup (?)
endif
endif
ifdef tPL1StackSize
if tPL1StackSize
tPL2Stack	db tPL1StackSize dup (?)
endif
endif
ifdef tPL0StackSize
tPL1Stack	db tPL0StackSize dup (?)
endif
;tPL0Stack	dw ?
TSSFields	ends

NewHeaderStruc	struc
NewID		db '3P'	;+0 identifier.
NewSize		dd ?		;+2 byte size of 3P section of file.
NewLength		dd ?		;+6  byte size of exe image data.
NewAlloc		dd ?		;+10 byte size of program.
NewSegments		dw ?		;+14 number of segment definitions.
NewRelocs		dd ?		;+16 number of relocation table entries.
NewEntryEIP		dd ?		;+20 entry offset.
NewEntryCS		dw ?		;+24 segment list entry number for entry CS.
NewEntryESP		dd ?		;+26 ESP offset.
NewEntrySS		dw ?		;+30 segment list entry number for SS.
NewFlags		dd ?		;+32 Control flags.
NewAutoStack		dd ?		;+36 Auto stack size.
NewAutoDS		dw ?		;+40 Auto DS segment number +1
NewExports		dd ?		;+42 Length of EXPORT section.
NewImports		dd ?		;+46 Length of IMPORT section.
NewImportModCnt	dd ?		;+50 Number of IMPORT modules.
;NewReserved		db 64-NewReserved dup (?)
NewReserved		db 64-$ dup (?)
NewHeaderStruc	ends
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

ifndef CW3
LE_OBJ_Flags_Read       equ 1   ;Readable.
LE_OBJ_Flags_Write      equ 2   ;Writeable.
LE_OBJ_Flags_Exec       equ 4   ;Executable.
LE_OBJ_Flags_Res        equ 8   ;Resource.
LE_OBJ_Flags_Discard    equ 16  ;Discardable.
LE_OBJ_Flags_Shared     equ 32  ;Shared.
LE_OBJ_Flags_PreLoad    equ 64  ;Preload.
LE_OBJ_Flags_Invalid    equ 128 ;Invalid.
LE_OBJ_Flags_FillMsk    equ 256+512     ;Mask for fill type bits.
LE_OBJ_Flags_Normal     equ 0   ;Normal fill type.
LE_OBJ_Flags_Zero       equ 256 ;Zero filled.
LE_OBJ_Flags_Res1       equ 512 ;resident.
LE_OBJ_Flags_Res2       equ 256+512     ;resident/contiguous.
LE_OBJ_Flags_LongLoc    equ 1024        ;long lockable.
LE_OBJ_Flags_16Alias    equ 4096        ;16:16_ALIAS
LE_OBJ_Flags_Big        equ 8192        ;"BIG" (Huge: 32-bit)
LE_OBJ_Flags_Conform    equ 16384       ;Conforming.
LE_OBJ_Flags_IOPriv     equ 32768       ;"OBJECT_I/O_PRIVILEGE_LEVEL
endif

;
;LE header format.
;
LE_Header               struc
LE_ID           dw ?    ;"LE" text identifier.
LE_ByteOrder            db ?    ;byte order, 0=little-endian, none-zero=big.
LE_WordOrder            db ?    ;word order.
LE_Format               dd ?    ;format level.
;
LE_CPU          dw ?    ;CPU type.
LE_CPU_286              equ 1
LE_CPU_386              equ 2
LE_CPU_486              equ 3
LE_CPU_586              equ 4
LE_CPU_i860             equ 20h
LE_CPU_N11              equ 21h
LE_CPU_R2000            equ 40h
LE_CPU_R6000            equ 41h
LE_CPU_R4000            equ 42h
;
LE_OS           dw ?    ;Target operating system.
LE_OS_OS2               equ 1
LE_OS_Windows           equ 2
LE_OS_DOS4              equ 3
LE_OS_Win386            equ 4
;
LE_Version              dd ?    ;Module version.
;
LE_Type         dd ?    ;Module type.
LE_Type_InitPer equ 1 shl 2     ;initialise per process.
LE_Type_IntFixup        equ 1 shl 4     ;no internal fixups.
LE_Type_ExtFixup        equ 1 shl 5     ;no external fixups.
LE_Type_NoLoad          equ 1 shl 13    ;module not loadable.
LE_Type_DLL             equ 1 shl 15    ;DLL
;
LE_Pages                dd ?    ;number of memory pages.
LE_EntryCS              dd ?    ;Entry CS object.
LE_EntryEIP             dd ?    ;Entry EIP.
LE_EntrySS              dd ?    ;Entry SS object.
LE_EntryESP             dd ?    ;Entry ESP.
LE_PageSize             dd ?    ;Page size.
LE_LastBytes            dd ?    ;Bytes on last page.
LE_FixupSize            dd ?    ;fixup section size.
LE_FixupChk             dd ?    ;fixup section check sum.
LE_LoaderSize           dd ?    ;loader section size.
LE_LoaderChk            dd ?    ;loader section check sum.
LE_ObjOffset            dd ?    ;offset of object table.
LE_ObjNum               dd ?    ;object table entries
LE_PageMap              dd ?    ;object page map table offset.
LE_IterateMap           dd ?    ;object iterate data map offset.
LE_Resource             dd ?    ;resource table offset
LE_ResourceNum          dd ?    ;resource table entries.
LE_ResidentNames        dd ?    ;resident names table offset.
LE_EntryTable           dd ?    ;entry table offset.
LE_Directives           dd ?    ;module directives table offset.
LE_DirectivesNum        dd ?    ;module directives entries.
LE_Fixups               dd ?    ;fixup page table offset.
LE_FixupsRec            dd ?    ;fixup record table offset.
LE_ImportModNames       dd ?    ;imported module name table offset.
LE_ImportModNum dd ?    ;imported modules count.
LE_ImportNames          dd ?    ;imported procedures name table offset.
LE_PageChk              dd ?    ;per-page checksum table offset.
LE_Data         dd ?    ;data pages offset.
LE_PreLoadNum           dd ?    ;pre-load page count.
LE_NoneRes              dd ?    ;non-resident names table offset.
LE_NoneResSize          dd ?    ;non-resident names table length.
LE_NoneResChk           dd ?    ;non-resident names checksum.
LE_AutoDS               dd ?    ;automatic data object.
LE_Debug                dd ?    ;debug information offset.
LE_DebugSize            dd ?    ;debug information size.
LE_PreLoadInstNum       dd ?    ;pre-load instance pages number.
LE_DemandInstNum        dd ?    ;demand instance pages number.
LE_HeapExtra            dd ?    ;extra heap alloction.
LE_Reserved             db 20 dup (?) ;reserved.
LE_DeviceID             dw ?    ;device ID (Windows VxD only).
LE_DDK          dw ?    ;DDK version number.
LE_Header               ends


PSP_Struc		struc
;
;First the standard DOS PSP structure. Won't use any of existing entries for
;extended info incase currently un-used entries get used at a later data.
;
 PSP_INT20		dw ?		;INT 20 instruction for CP/M CALL 0 program
				;termination the CDh 20h here is often used as a
				;signature for a valid PSP
 PSP_MemTop		dw ?		;segment of first byte beyond memory allocated to
				;program. N/A
 PSP_Filler1		db ?		;unused filler

 PSP_CPM		db ?		;CP/M CALL 5 service request (FAR JMP to 000C0h)
				;BUG: (DOS 2+) PSPs created by INT 21/AH=4Bh
				;point at 000BEh
		dw ?		;CP/M compatibility--size of first segment for
				;.COM files
		dw ?		;remainder of FAR JMP at 05h
 PSP_INT22		dd ?		;stored INT 22 termination address
 PSP_INT23		dd ?		;stored INT 23 control-Break handler address
 PSP_INT24		dd ?		;DOS 1.1+ stored INT 24 critical error handler
				;address
 PSP_Parent		dw ?		;segment of parent PSP
 PSP_HandleList	db 20 dup (?) ;DOS 2+ Job File Table, one byte per file
				;handle, FFh = closed
 PSP_Environment	dw ?		;segment of environment for process
 PSP_INT21SSSP	dd ?		;N/A
 PSP_Handles		dw ?		;DOS 3+ number of entries in JFT (default 20)
 PSP_HandlePtr	dd ?		;DOS 3+ pointer to JFT (default PSP:0018h)
 PSP_Previous	dd ?		;DOS 3+ pointer to previous PSP (default
				;FFFFFFFFh in 3.x) used by SHARE in DOS 3.3.
 PSP_Filler2		dd ?		;unused by DOS versions <= 6.00
				;reportedly used by Novell NetWare shell 3.x
 PSP_DOSVersion	dw ?		;DOS 5+ version to return on INT 21/AH=30h
 PSP_NextPSP		dw ?		;(MSWin3) selector of next PSP (PDB) in linked
				;list Windows keeps a linked list of Windows
				;programs only
 PSP_Filler3		dd ?		;unused by DOS versions <= 6.00
 PSP_WinOldAp	db ?		;(MSWindows3) bit 0 set if non-Windows
				;application (WINOLDAP)
 PSP_Filler4		db 7 dup (?) ;unused by DOS versions <= 6.00
 PSP_INT21		db 3 dup (?) ;DOS 2+ service request (INT 21/RETF instructions)
 PSP_Filler5		db 9 dup (?) ;unused in DOS versions <= 6.00
 PSP_FCB1		db 16 dup (?) ;first default FCB, filled in from first
				;commandline argument overwrites second FCB if
				;opened
 PSP_FCB2		db 16 dup (?) ;second default FCB, filled in from second
				;commandline argument overwrites beginning of
				;commandline if opened
 PSP_Filler6		dd ?		;unused
 PSP_CommandTail	db 128 dup (?) ;commandline / default DTA
				;command tail is BYTE for length of tail, N BYTEs
				;for the tail, followed by a BYTE containing 0Dh
PSP_Struc		ends




