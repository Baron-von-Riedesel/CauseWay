
;--- CW converts LE to P3
;--- if image is P3 already, it displays current format with /i
;--- or sets various parameters for CW dos extender.

;--- this version has been adjusted and needs to find cwstub.exe!

	.386
	.model flat
	.dosseg		;WL32 needs .dosseg to ensure that stack is last
	.stack 4096

	include general.inc
	include ..\strucs.inc

_Seg_	struc
Seg_BaseAddress dd ?
Seg_Type	dd ?
Seg_Length	dd ?
Seg_Memory	dd ?
_Seg_	ends

if @Model ne 7
DGROUP group _TEXT, _DATA, _BSS, STACK
endif

	.const

ifdef DEBUG
@dbgmsg macro text:vararg
local sym
	.const
sym db text,'$'
	.code
    pushad
    mov edx, offset sym
    mov ah,9
    int 21h
    popad
endm
else
@dbgmsg textequ <;>
endif

	.data

psp dd ?		; PSP linear address

ERR_NOFILES equ 5
ERR_OPENFIL equ 6
ERR_HDRFMT  equ 7
ERR_IO      equ 8
ERR_ALREADY3P equ 26

ErrorNumber	dw 0
;
ErrorMessages	dd Errorm0,Errorm1,Errorm2,Errorm3,Errorm4,Errorm5,Errorm6,Errorm7
	dd Errorm8,ErrorM9,ErrorM10,ErrorM11,ErrorM12,ErrorM13,ErrorM14,ErrorM15
	dd ErrorM16,ErrorM17,ErrorM18,ErrorM19,ErrorM20,ErrorM21,ErrorM22,ErrorM23
	dd ErrorM24,ErrorM25,ErrorM26,ErrorM27,ErrorM0,ErrorM0
	dd ErrorM0,ErrorM0,ErrorM32,ErrorM33,ErrorM34,ErrorM35,ErrorM36,ErrorM37
	dd ErrorM38,ErrorM39,ErrorM40,ErrorM41,ErrorM42,ErrorM43
;
Errorm0	db 'Operation completed successfully...',13,10,13,10,'$'
Errorm1	db 'Unable to resize memory block, this should never happen...',10,13,'$'
Errorm2	db 'Insufficient memory available...',13,10,'$'
Errorm3	db ' Syntax is:- CW Options FileName Options',13,10
	db 13,10
	db '   Options:-',13,10
	db 13,10
	db '   3  - Enable 32 bit code segments/interupts.',13,10
	db '  +b  - Bind stub loader.',13,10
	db '   c  - Config file name, eg, /c[:]example.cfg',13,10
	db '   d  - Enable dual mode operation.',13,10
	db '   f  - Sort fixups to enable fast-load.',13,10
	db '  +g  - Enable GROUP type segments (data limits set to end of program).',13,10
	db '   i  - Display 3P header information. use i, is, ir or irs.',13,10
	db "        eg, /isr to see everything (s/r order not important).",13,10
	db '   l  - Enable LDT usage.',13,10
	db '   n  - Enable NEAR mode (code/data in same segment).',13,10
	db '   s  - Set auto-stack size, eg, /s2048.',13,10
	db '   u  - Update copy of CauseWay in a CauseWay file.',13,10
	db '  +z  - Append auto-stack segment to NEAR segment in NEAR model.',13,10
	db 13,10
	db ' The options above that have a + before them are ON by default.',13,10
	db ' Switches are ON with + or /, OFF with -.',13,10
	db ' The command line can be in any order.',13,10
	db ' If no extension is specified then .EXE is used (.MAP for map files).',13,10
	db 13,10,13,10,'$'
Errorm4	db 'No operation type specified...',13,10,'$'
Errorm5	db 'No files found to alter...',13,10,'$'
Errorm6	db 'Unable to open EXE file...',13,10,'$'
Errorm7	db 'Incorrect header type...',13,10,'$'
Errorm8	db 'I/O error while accessing file...',13,10,'$'
Errorm9	db 'Could not find CauseWay stub file CWSTUB.EXE.',13,10,'$'
ErrorM10	db 'Could not find .MAP file.',13,10,'$'
ErrorM11	db 'Could not identify segment list header.',13,10,'$'
ErrorM12	db 'Bad segment definition.',13,10,'$'
ErrorM13	db 'No segment definitions found.',13,10,'$'
ErrorM14	db 'Segment does not have recognized class.',13,10,'$'
ErrorM15	db 'Unrecognized variable in configuration file.',13,10,'$'
ErrorM16	db 'Syntax error in configuration file.',13,10,'$'
ErrorM17	db 'To many segment declarations.',13,10,'$'
ErrorM18	db 'Maximum line length exceeded.',13,10,'$'
ErrorM19	db 'Unable to create output file.',13,10,'$'
ErrorM20	db 'Entry point is not in a code segment.',13,10,'$'
ErrorM21	db 'No stack defined.',13,10,'$'
ErrorM22	db 'Stack cannot be in a code segment / No stack.',13,10,'$'
ErrorM23	db 'Invalid relocation entry.',13,10,'$'
ErrorM24	db 'Only 1 segment definition allowed in NEAR mode.',13,10,'$'
ErrorM25	db 'Could not identify stack segment in segment list.',13,10,'$'
ErrorM26	db ' is already in CauseWay format.',13,10,'$'
ErrorM27	db 'Invalid auto-stack value.',13,10,'$'
;
ErrorM32	db "Could not open specified LE file.",13,10,0
ErrorM33	db "Error reading LE offset.",13,10,0
ErrorM34	db "Not enough memory to load LE file.",13,10,0
ErrorM35	db "Error reading LE file.",13,10,0
ErrorM36	db "Not enough memory to build 3P image.",13,10,0
ErrorM37	db "Could not create output file.",13,10,0
ErrorM38	db "Error occured while writeing output file.",13,10,0
ErrorM39	db "Multiple fixup records not supported.",13,10,0
ErrorM40	db "Only internal fixups supported.",13,10,0
ErrorM41	db "Invalid fixup type, only Seg16 and Offset32 supported.",13,10,0
ErrorM42	db "Error reading extender stub loader.",13,10,0
ErrorM43	db "Unknown fixup flag settings.",13,10,0
;
ErrorText	db 'ERROR: ',0
	if	0
SegSamet	db 'WARNING: Segments '
SegSamet0	db '0000 and '
SegSamet1	db '0000 have the same paragraph address.',13,10,0
	endif
SegSamet	db 'WARNING: Segments found with same paragraph base address.',13,10,0
;
InternalConfig	db 'Using internal segment configuration details.',13,10,0
ProcessStubText db 'Linking CauseWay stub loader.',13,10,0
ProcessMapText	db 'Processing map file.',13,10,0
ProcessExeText	db 'Processing EXE relocation table.',13,10,0
GenerateExeText db 'Generating new executable file.',13,10,0
CarriageReturn	db 13,10,0
CarriageReturn2 db 13,10,"$"
;
ConfigName	db 'cw.cfg',0
CWStubName	db "CWSTUB.EXE",0
ConfigHandle	dw 0
SHELLHandle	dw 0
EXEextension	db 'EXE',0
EXEHandle	dw 0                  ; file handle of .EXE to patch
MAPextension	db 'MAP',0
MAPHandle	dw 0                  ; file handle of .MAP file
TempFileName	db 'ckanetdy.ckq',0
TempHandle	dw 0
;
SegmentList	dd 0
SegCurrent	dd ?
SegmentTotal	dd ?
ExeSegment	dd ?
;
	.data?

ConfigPath	db 128 dup (?)
SHELLFileName	db 128 dup (?)    ; name of CW.EXE binary, gotten from environment
EXEFileName	db 64 dup (?)
MAPFileName	db 64 dup (?)
LineBuffer	db 1024 dup (?)

	.data
;
RelocSegment	dd ?
;
MZHdr struct
Signature	dw ?	;00 Identifier text 'MZ', '3P'.
_Length		dw ?	;02 Length of file MOD 512
			dw ?	;04 Length of file in 512 byte blocks.
RelocNum	dw ?	;06 Number of relocation items.
HeaderSize	dw ?	;08 Length of header in 16 byte paragraphs.
MinAlloc	dw ?	;0A Minimum number of para's needed above program.
MaxAlloc	dw ?	;0C Maximum number of para's needed above program.
StackSeg	dw ?	;0E Segment displacement of stack module.
EntrySP		dw ?	;10 value for SP at entry.
CheckSum	dw ?	;12 Check sum...
EntryIP	dw ?	;14 Contents of IP at entry.
EntryCS	dw ?	;16 Segment displacement of CS at entry.
RelocFirst	dw ?	;18 First relocation item offset.
OverlayNum	db ?	;1A Overlay number.
MZHdr ends

exehdr MZHdr <>
;
ExeSize1	dw ?,?	;Header size
ExeSize2	dw ?,?	;Real file size.
;
NewHeader	NewHeaderStruc <>	;make space for a header.
;
;List of variables for the config file.
;
VariableList	dd SegHeaderVAR,WhiteSpaceString,SegHeaderText,0
	dd SegFormatVAR,SegFormatCode,SegLayout,0
	dd CodeClassVAR,WhiteSpaceString,SegClassCODE,0
	dd DataClassVAR,WhiteSpaceString,SegClassDATA,0
	dd StackClassVAR,WhiteSpaceString,SegClassSTACK,0
	dd ConstClassVAR,WhiteSpaceString,SegClassCONST,0
	dd NearClassVAR,WhiteSpaceString,SegClassNEAR,0
	dd -1
;
SegHeaderVAR	db 'SegmentHeader',0
SegFormatVAR	db 'SegmentFormat',0
CodeClassVAR	db 'CodeClass',0
DataClassVAR	db 'DataClass',0
StackClassVAR	db 'StackClass',0
ConstClassVAR	db 'ConstClass',0
NearClassVAR	db 'NearClass',0
;
;
;The text looked for to identify start of segment list. Any amount of white space can
;be replaced with a single space here. search is NOT case sensitive.
;
	db -1
SegHeaderText	db ' start stop length name class',0
	db 128 dup (0)
;
SegFormatTexts	dd NullText,StartText,LengthText,ClassText,-1
NullText	db 'null',0
StartText	db 'start',0
LengthText	db 'length',0
ClassText	db 'class',0
SegFormatSlots	db 0,0,0,0,0
;
;Layout of each line.
;
;0	- Ignore.
;1	- Start.
;2	- Length.
;3	- Class.
;
SegLayout	db 1,0,2,0,3,-1
	db 256 dup (-1)

;
;List of different class's recognised.
;
SegClassList	dd SegClassCODE,SegClassDATA,SegClassSTACK,SegClassCONST,SegClassNEAR,-1


;
;list of code segment classes seperated by a space.
;
	db -1
SegClassCODE	db 'code',0
	db 2048 dup (0)

;
;List of data segment classes seperated by a space.
;
	db -1
SegClassDATA	db 'data',0
	db 2048 dup (0)

;
;List of stack segment classes seperated by a space.
;
	db -1
SegClassSTACK	db 'stack',0
	db 2048 dup (0)

;
;List of const segment classes seperated by a space.
;
	db -1
SegClassCONST	db 'const',0
	db 2048 dup (0)

;
;List of flat segment classes seperated by space.
;
	db -1
SegClassNEAR	db 'NEAR',0
	db 2048 dup (0)

;
Bit16Text	db '16 bit application.',13,10,'$'
Bit32Text	db '32 bit application.',13,10,'$'
BitDualText	db 'Dual mode application.',13,10,'$'
SpeedLoadText	db "Speed-Load enabled.",13,10,13,10,"$"
;
NewSizeT	db '    3P size: ',0	;byte size of 3P file.
NewLengthT	db ' Image size: ',0	;byte size of exe image data.
NewAllocT	db 'Memory size: ',0	;byte size of program.
NewSegmentsT	db '   Segments: ',0	;number of segment definitions.
NewRelocsT	db 'Relocations: ',0	;number of relocation table entries.
NewEntryEIPt	db '  Entry EIP: ',0	;entry offset.
NewEntryCSt	db '   Entry CS: ',0	;segment list entry number for entry CS.
NewEntryESPt	db '  Entry ESP: ',0	;ESP offset.
NewEntrySSt	db '   Entry SS: ',0	;segment list entry number for SS.
NewEntryAutoSS	db ' Auto-Stack: ',0	;Auto stack size.
;
ReadingMainText db 13,10,"Reading segments, relocations and image data.",13,10,0
;
SegmentStuff	db 13,10," Segment definition details"
	db 13,10,"============================",13,10
	db 13,10,' No.   Base     Limit  Class'
	db 13,10,'-----------------------------',13,10,0
;
RelocStuff	db 13,10," Relocation entry details"
	db 13,10,"==========================",13,10
	db 13,10,' No.  Offset   Segment'
	db 13,10,'-----------------------',13,10,0
;
InvalidText	db "INVALID "
;
Real3POffset	dd ?
;
ReadingLEText	db "Reading specified LE file.",13,10,0
BuildImageText	db "Building EXE and segment image.",13,10,0
BuildRelocsText db "Building relocation table.",13,10,0
BuildHeaderText db "Building 3P format header.",13,10,0
Write3PText	db "Writeing 3P file.",13,10,0
;
LEAddress	dd 0
LELength	dd 0
LEOffset	dd 0
;
ObjectCount	dd 0
ObjectBase	dd 0
ObjectList	dd 0
;
RelocationList	dd 0
;
SegmentBase	dd 0
;
PageCount	dd 0,0
;
RealHeader	NewHeaderStruc <>
;
StubMem	dd 0
RelocationCount dd 0
;
if @Model ne 7
DataLinearBase	dd 0
endif
;
RelocMem	dd 0,0
P3Offset	dd 0

	db -1			;just to make sure end of segment is initialised.

	.code

HiThere	 db 'CauseWay EXE maker v2.02.',13,10,'$'

ifndef CWAPP
 if @Model ne 7
start32:
 else
start:
 endif
else
start:
endif
Entry	proc	far

ifndef CWAPP
	mov [psp], ebx		;set by LOADPE.BIN
else
	mov ebx, es
	mov ax, 6
	int 31h
	mov ax, cx
	shl eax, 16
	mov ax, dx
	mov [psp], eax
	push ds
	pop es
endif
if @Model ne 7
	mov	ebx,cs
	mov ax, 6
	int 31h
	push cx
	push dx
	pop edx
	mov	DataLinearBase,edx
endif
	;
	mov	edx,offset HiThere
	mov	ah,9
	int	21h
	mov	ErrorNumber,3
	;
	mov	edi,offset NewHeader+2
	mov	ecx,size NewHeaderStruc-2
	xor	al,al
	rep	stosb
	;
	mov	OptionTable+'G','+'	;default to groups.
	mov	OptionTable+'B','+'	;default to bind loader.
	mov	OptionTable+'Z','+'	;default to auto stack on near.
	;
	call	ReadCommand		;read the command line.
	cmp	ax,1
	jc	System		;need at least 2 names.
	mov	ErrorNumber,5
	@dbgmsg "debug messages ON",13,10
	cmp	w[OptionTable+128],0	;get file name mask.
	jz	System		;must have a name.
	cmp	OptionTable+'N',0
	jz	@@NoGroupDel
	mov	OptionTable+'G',0	;turn group stuff off.
@@NoGroupDel:	;
	;Read config file.
	;
	call	ReadConfig
	jnz	System
	;
	;Get the .EXE file name and add .EXE if needed.
	;
	mov	esi,d[OptionTable+128]	;get file name mask.
	mov	edi,offset EXEFileName
	cld
	xor	al,al
@@0:	movsb
	cmp	b[esi-1],'.'
	jnz	@@1
	mov	al,1
@@1:	cmp	b[esi-1],0
	jnz	@@0
	or	al,al
	jnz	@@2
	mov	b[edi-1],'.'
	mov	esi,offset EXEextension
@@4:	movsb
	cmp	b[esi-1],0
	jnz	@@4
	;
@@2:	;Generate .MAP file name.
	;
	mov	esi,offset EXEFileName
	mov	edi,offset MapFileName
@@3:	movsb
	cmp	b[esi-1],'.'
	jnz	@@3
	mov	esi,offset MAPextension
@@5:	movsb
	cmp	b[esi-1],0
	jnz	@@5
	;
	;Generate shell file name.
	;
	mov	ebx, psp
	mov	bx,[ebx+2ch]		;Get enviroment string selector
	mov ax,6
	int 31h
	push cx
	push dx
	pop esi			;Point at the enviroment string.
@@10a:
	mov	al,[esi]		;Get a byte.
	inc	esi		;/
	or	al,al		;End of a string?
	jnz	@@10a		;keep looking.
	mov	al,[esi]		;Double zero?
	or	al,al		;/
	jnz	@@10a		;keep looking.
	add	esi,3		;Skip last 0 and word count.
	cld			;/
	mov	edi,offset SHELLFileName	;Where we're gonna put it.
    mov	edx,edi
@@70:
	lodsb
    stosb
    cmp al,'\'
    jnz noslash
    mov edx,edi
noslash:
	cmp	al,0		;copy till end.
	jnz	@@70
	mov edi, edx
    mov esi, offset CWStubName
nextchar:
    lodsb
    stosb
    and al,al
    jnz nextchar
	;
	;Check file type.
	;
;	mov	bx,EXEHandle
;	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	System
	mov	EXEHandle,ax
	mov	bx,ax
	;
	;See if EXE is LE format.
	;
	mov edx, offset exehdr
	mov ecx, sizeof MZHdr
	mov	ah,3fh
	int	21h		;read the .EXE header.
	cmp	ax,sizeof MZHdr
	jnz	@@LEReset
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@LEReset
	cmp	exehdr.HeaderSize, 4	; at least 40h byte header?
	jb	@@LEReset
    
	mov	dx,3ch
	xor	cx,cx
	mov	ax,4200h
	int	21h
	push	edx
	mov	edx,esp
	mov	ecx,4
	mov	ah,3fh
	int	21h		;Fetch LE offset.
	pop	ebp
	jc	@@LEReset
	cmp	ax,cx
	jnz	@@LEReset
	or	ebp,ebp		;We get anything?
	jz	@@LEReset
	mov	dx,bp
	shr	ebp,16
	mov	cx,bp
	mov	ax,4200h
	int	21h		;Move to LE section.
	push	edx
	mov	edx,esp
	mov	ecx,2
	mov	ah,3fh
	int	21h
	pop	edx
	jc	@@LEReset
	cmp	ax,cx
	jnz	@@LEReset
	cmp	dx,"EL"		;LE?
	jnz	@@LEReset

	mov	ah,3eh
	int	21h		;close .EXE file.
	;
	call	LEProcess		;convert LE file.
	jmp	System
	;
@@LEReset:	;Reset to start of file again.
	;
	xor	dx,dx
	xor	cx,cx
	mov	ax,4200h
	int	21h
	;
	;See if EXE is already 3P.
	;
if 0 ; exehdr was already read
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,EXEHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	System
	mov	ErrorNumber,ERR_HDRFMT	;stub file error.
	cmp	ax,sizeof MZHdr
	jnz	System
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jz	@@DoEXE
	mov	bx,EXEHandle
	mov	ah,3eh                  ;close .EXE
	int	21h
	mov	ErrorNumber,0
	jmp	@@Info                  ;and just display INFO
else
	cmp exehdr.Signature,'P3'   ;is .EXE a plain 3P file without MZ header?
	jnz @@DoExe
	mov	ErrorNumber,0
	call NewExeInfo
	jmp	System
endif
	;
@@DoEXE:
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe2		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe2:
	movzx eax, ax
	imul eax, 200h
	movzx edx,exehdr._Length	;add length mod 512
	add eax, edx
	push eax
	pop dx
	pop cx
	mov	ax,4200h
	mov	bx,EXEHandle
	int	21h
	;
	mov	edx,offset exehdr
	mov	ecx,2
	mov	bx,EXEHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	jc	@@Reset
	mov	ErrorNumber,ERR_HDRFMT	;stub file error.
	cmp	ax,2
	jnz	@@Reset
	mov	ErrorNumber,ERR_ALREADY3P
	cmp	exehdr.Signature,'P3'	;Correct ID '3P'?
	jz	@@Info
	;
@@Reset:
	mov	bx,EXEHandle
	mov	ax,3e00h
	int	21h
	mov	ErrorNumber,0
	;
	;Now do some real processing.
	;
	call	ExeAndMap2NewExe
	jnz	System
	jmp	@@InfoDump
	;
@@Info:	cmp	OptionTable+"P",0	;Post-mortem?
	jz	@@update
	call	PostMortem
	jmp	@@InfoDump
	;
@@update:	cmp	OptionTable+'U',0	;Update?
	jz	@@fastload
	call	NewCauseWay
	jmp	@@InfoDump

@@fastload:
	cmp	OptionTable+"F",0	;fast load?
	jz	@@DualMode
	call	FastLoad3P
	jmp	@@InfoDump

@@DualMode:
;	cmp	OptionTable+'D',0	; dual mode?
;	jz	@@InfoDump
;	call	EXEDualMode

@@InfoDump:
	cmp	OptionTable+'I',0	;Info?
	jz	System
	push	ErrorNumber
	call	NewExeInfo
	cmp	ErrorNumber,0
	jnz	System
	pop	ErrorNumber
	cmp	ErrorNumber,26
	jnz	System
	mov	ErrorNumber,0
	jmp	System

	;
@@9:	mov	ax,4c00h
	int	21h
Entry	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
System	proc	near
	;
	;Make sure temp file is deleted.
	;
	mov	edx,offset TempFileName	;get file name.
	mov	ah,41h
	int	21h
	;
	cmp	ErrorNumber,0
	jz	@@NoError
	cmp	ErrorNumber,3
	jz	@@NoError
	mov	edx,offset ErrorText
	call	StringPRint
	jmp	@@DoneError
@@NoError:	;
	mov	edx,offset CarriageReturn
	call	StringPrint
@@DoneError:	;
	cmp	ErrorNumber,26
	jnz	@@NoName
	mov	edx,offset EXEFileName
	call	StringPrint
@@NoName:
	movzx	ebx,ErrorNumber
	shl	ebx,2
	add	ebx,offset ErrorMessages	;get error message.
	mov	edx,[ebx]
	mov	ah,9
	int	21h		;print it.
	;
	mov	ax,4c00h
	int	21h
System	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Add post-mortem dump code and SYM file to 3P EXE.
;
PostMortem	proc	near
	ret
PostMortem	endp


COMMENT !
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Set the EXE dual mode flag
;
EXEDualMode	proc	near
	;Open the source EXE
	;
	mov	bx,EXEHandle
	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	@@9
	mov	EXEHandle,ax
	;
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,EXEHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	@@9
	mov	ErrorNumber,9	;stub file error.
	cmp	ax,sizeof MZHdr
	jnz	@@9
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@9
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe10	; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe10:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	xchg	cx,dx
	shl	ecx,16
	mov	cx,dx
	movzx	ebx,EXEHandle
	xor	al,al
	call	LESetFilePointer	;point to 3P bit of the file.
	mov	P3Offset,ecx
	;
	;Read the 3P header.
	;
	mov	edx,offset RealHeader
	mov	ecx,size NewHeaderStruc
	movzx	ebx,EXEHandle
	call	LEReadFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	call	SetSystemConfig	; update with new info
	;put the pointer back to the header.
	;
	mov	ecx,P3Offset
	xor	al,al
	call	LESetFilePointer
	;
	;Update the header
	;
	mov	edx,offset RealHeader
	mov	ecx,size NewHeaderStruc
	movzx	ebx,EXEHandle
	call	LEWriteFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9

@@8:
	movzx	ebx,EXEHandle
	call	LECloseFile

	mov	ErrorNumber,0
	clc

@@9:
	ret

EXEDualMode	ENDP
END COMMENT !

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Sort 3P file fixups into order and set fast load flag.
;
FastLoad3P	proc	near
	local @@SymbolCount:dword, @@iValue:dword, @@vValue:dword
	;
	;Open the source EXE
	;
	mov	bx,EXEHandle
	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	@@9
	mov	EXEHandle,ax
	;
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,EXEHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	@@9
	mov	ErrorNumber,9	;stub file error.
	cmp	ax,sizeof MZHdr
	jnz	@@9
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@9
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe3		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe3:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	xchg	cx,dx
	shl	ecx,16
	mov	cx,dx
	movzx	ebx,EXEHandle
	xor	al,al
	call	LESetFilePointer	;point to 3P bit of the file.
	mov	P3Offset,ecx
	;
	;Read the 3P header.
	;
	mov	edx,offset RealHeader
	mov	ecx,size NewHeaderStruc
	movzx	ebx,EXEHandle
	call	LEReadFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Check it is a 3P section.
	;
	cmp	w[RealHeader.NewID],"P3"
	jnz	@@9
	;
	;Get some memory for the relocation table.
	;
	mov	ecx,d[RealHeader.NewRelocs]
	cmp	ecx,1+1
	jc	@@8
	shl	ecx,2
	call	LEMalloc
	jc	@@9
	mov	RelocMem,esi
	call	LEMalloc
	jc	@@9
	mov	RelocMem+4,esi
	;
	;Move to the relocations.
	;
	movzx	ecx,[RealHeader.NewSegments]
	shl	ecx,3
	mov	al,1
	movzx	ebx,EXEHandle
	call	LESetFilePointer
	;
	;Read the relocations.
	;
	mov	ecx,d[RealHeader.NewRelocs]
	shl	ecx,2
	mov	edx,RelocMem
	movzx	ebx,EXEHandle
	call	LEReadFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9

	;
	;Make the table.
	;
	mov	edi,RelocMem+4
	mov	eax,RelocMem
	mov	ecx,d[RealHeader.NewRelocs]
	push	ds
	pop	es
	cld
@@00:	stosd
	add	eax,4
	dec	ecx
	jnz	@@00


;
;Thingy sort the relocation pointers.
;
	mov	ecx,d[RealHeader.NewRelocs]
	mov	@@SymbolCount,ecx
	;
	;Now do the sort.
	;
	mov	eax,@@SymbolCount
	cmp	eax,1
	jbe	@@sortend		; sorted by definition
	xor	ebx,ebx
	mov	edx,ebx
	mov	bl,9
	div	ebx
	mov	ecx,eax		; ecx == quotient, N/9
;
; for (h=1;h<=N/9;h=3*h+1);
;
	mov	eax,ebx		; eax==9
	mov	al,1		; eax==1, h
	mov	bl,3		; ebx==3
	xor	edx,edx		; zero for multiply loop
@@sethloop:	;
	cmp	eax,ecx		; h<=N/9
	ja	@@sort2
	mul	ebx		; 3*h, assume 32-bit result (pretty safe bet)
	inc	eax		; 3*h+1
	jmp	@@sethloop
;
; ebx will play role of j, edx will play role of h
;
@@sort2:	mov	edx,eax		; edx == h
;
; for (;h>0;...
;
@@hloop:	or	edx,edx		; h>0
	je	@@sortend
;
; for(i=h+1...
;
	mov	eax,edx
	inc	eax
	mov	@@iValue,eax
;
; for(...;i<=N;...){
;
@@iloop:	mov	eax,@@iValue
	cmp	eax,@@SymbolCount
	ja	@@nexth
	;
	mov	ecx,RelocMem+4
	mov	ecx,[ecx-4+eax*4]
	mov	@@vValue,ecx		; v=a[i]
	mov	ebx,eax		; j=i
;
; while(j>h && a[j-h]>v){
;
@@whileloop:	cmp	ebx,edx		; j>h
	jbe	@@whilefail
	;
	mov	eax,ebx
	sub	eax,edx		; eax==j-h
	mov	esi,RelocMem+4
	mov	esi,[esi-4+eax*4]	; esi==a[j-h]
	mov	edi,@@vValue		; edi==v
	xor	ecx,ecx		; zero high bytes of register for following repe
;
; a[j-h] > v
;
	pushm	esi,edi
	mov	esi,[esi]
	mov	edi,[edi]
	and	esi,0FFFFFFFh
	and	edi,0FFFFFFFh
	cmp	esi,edi
	popm	esi,edi
	jbe	@@whilefail		; first < second, a[j-h]<v

@@dochange:	;
	mov	eax,ebx
	sub	eax,edx		; eax==j-h
	lea	eax,[eax*4]
	add	eax,RelocMem+4
	mov	eax,[eax-4]		; eax==a[j-h]
	push	ebx
	lea	ebx,[ebx*4]
	add	ebx,RelocMem+4
	mov	[ebx-4],eax		; a[j]=a[j-h]
	pop	ebx
	sub	ebx,edx		; j-=h
	jmp	@@whileloop
@@whilefail:	;
	mov	eax,@@vValue
	push	ebx
	lea	ebx,[ebx*4]
	add	ebx,RelocMem+4
	mov	[ebx-4],eax		; a[j]=v
	pop	ebx
;
; for(...;i++){
;
	inc	@@iValue
	jmp	@@iloop
;
; for (...;h/=3){
;
@@nexth:	mov	eax,edx
	xor	edx,edx
	mov	ecx,edx
	mov	cl,3
	div	ecx
	mov	edx,eax
	jmp	@@hloop
@@sortend:	;


;
;Use list to order output.
;
	mov	ecx,d[RealHeader.NewRelocs]
	mov	edi,RelocMem+4
@@01:	mov	esi,[edi]
	movsd
	dec	ecx
	jnz	@@01

;
;Copy second table back to first.
;
	mov	ecx,d[RealHeader.NewRelocs]
	mov	esi,RelocMem+4
	mov	edi,RelocMem
	rep	movsd


;
;Set speed load flag in the header.
;
	or	d[RealHeader.NewFlags],1 shl 30

	;
	;Put the pointer back to start of relocations.
	;
	mov	ecx,d[RealHeader.NewRelocs]
	shl	ecx,2
	neg	ecx
	mov	al,1
	movzx	ebx,EXEHandle
	call	LESetFilePointer
	;
	;Write new relocations.
	;
	mov	ecx,d[RealHeader.NewRelocs]
	shl	ecx,2
	mov	edx,RelocMem
	movzx	ebx,EXEHandle
	call	LEWriteFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9

	;
	;put the pointer back to the header.
	;
	mov	ecx,P3Offset
	xor	al,al
	call	LESetFilePointer
	;
	;Update the header
	;
	mov	edx,offset RealHeader
	mov	ecx,size NewHeaderStruc
	movzx	ebx,EXEHandle
	call	LEWriteFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9


@@8:	movzx	ebx,EXEHandle
	call	LECloseFile

	mov	ErrorNumber,0
	clc


@@9:	ret
FastLoad3P	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
;Convert an LE format file into 3P format.
;
LEProcess	proc	near
	call	FetchLEFile		;Read the LE file.
	jc	@@9
	call	Create3PFile		;Create the 3P image.
	jc	@@9
	call	CreateRelocations	;Build relocation list.
	jc	@@9
	call	Create3PHeader	;Build the header.
	jc	@@9
	call	Write3PFile		;Now write the 3P file.
	jc	@@9
	mov	ErrorNumber,0
	clc
@@9:	ret
LEProcess	endp


;------------------------------------------------------------------------------
;
;Write the file at last.
;
Write3PFile	proc	near
	mov	esi,offset Write3PText
	call	LEPrintString
	;
	;Load the extender stub CWSTUB.EXE
	;
	mov	edx,offset SHELLFileName	; get extender stub CWSTUB.EXE
	call	LEOpenFile
	mov	ErrorNumber,42
	jc	@@9
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	call	LEReadFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe4		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe4:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	xchg	cx,dx
	shl	ecx,16
	mov	cx,dx
	mov	ErrorNumber,36
	add	ecx,4
	call	LEMalloc
	jc	@@9
	sub	ecx,4
	mov	StubMem,esi
	mov	d[esi],ecx
	add	esi,4
	pushm	ecx,esi
	xor	ecx,ecx
	xor	al,al
	call	LESetFilePointer
	popm	ecx,esi
	mov	ErrorNumber,42
	mov	edx,esi
	call	LEReadFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Create the output file.
	;
	mov	edx,offset TEMPFileName
	call	LECreateFile
	mov	ErrorNumber,37
	jc	@@9
	;
	;Write the stub.
	;
	mov	ErrorNumber,38
	mov	edx,StubMem
	mov	ecx,[edx]
	add	edx,4
	call	LEWriteFile
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Write the main header.
	;
	mov	edx,offset RealHeader
	mov	ecx,size NewHeaderStruc
	call	LEWriteFile
	mov	ErrorNumber,38
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Write the segment definitions.
	;
	mov	esi,ObjectList	;Point to the segment list.
	mov	ebp,[esi]		;Get number of entries.
	add	esi,4
@@0:	mov	edx,esi		;Point to segment defintion.
	mov	ecx,4+4
	call	LEWriteFile		;Write this entry.
	mov	ErrorNumber,38
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	add	esi,size _Seg_	;Next entry.
	dec	ebp
	jnz	@@0		;Do all segments.
	;
	;Write the relocation entries.
	;
	mov	esi,RelocationList
	or	esi,esi
	jz	@@1
	mov	ecx,[esi]		;Get number of entries.
	shl	ecx,2		;Dword per entry.
	add	esi,4
	mov	edx,esi		;Point to data.
	call	LEWriteFile
	mov	ErrorNumber,38
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
@@1:	;Write the EXE image.
	;
	mov	esi,ObjectList	;Point to object definitions.
	mov	ebp,[esi]		;Get number of entries.
	add	esi,4		;Point to real data.
@@2:	mov	ecx,_Seg_.Seg_Length[esi]	;Get segments length.
	mov	edx,[esi]._Seg_.Seg_Memory	;point to segments image.
	call	LEWriteFile
	mov	ErrorNumber,38
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	add	esi,size _Seg_	;point to next entry.
	dec	ebp
	jnz	@@2		;Do all entries.
	;
	;Close the output file.
	;
	call	LECloseFile
	;
	;Delete origional .EXE
	;
	mov	edx,offset EXEFileName	;get file name mask.
	mov	ah,41h
	int	21h
	mov	ErrorNumber,8
	jc	@@9
	;
	;now rename it.
	;
	mov	edx,offset TempFileName
	mov	edi,offset EXEFileName		;get file name mask.
	mov	ah,56h
	int	21h
	jc	@@9
	;
	clc
	ret
	;
@@9:	stc
	ret
Write3PFile	endp


;------------------------------------------------------------------------------
;
;Create 3P header data.
;
Create3PHeader	proc	near
	mov	esi,offset BuildHeaderText
	call	LEPrintString
	;
	mov	edi,offset RealHeader
	;
	;Set auto DS object number if there is one.
	;
	mov	esi,LEAddress
	mov	eax,[esi+94h]
	mov	[edi].NewHeaderStruc.NewAutoDS,ax
	;
	;Set number of segments and increase file length field.
	;
	mov	esi,ObjectList
	mov	eax,[esi]		;Get number of segments.
	mov	[edi].NewHeaderStruc.NewSegments,ax
	shl	eax,3		;8 bytes per seg.
	mov	[edi].NewHeaderStruc.NewSize,eax
	;
	;Set number of relocations and increase file length field.
	;
	mov	esi,RelocationList
	xor	eax,eax
	or	esi,esi
	jz	@@0
	mov	eax,[esi]
@@0:	mov	[edi].NewHeaderStruc.NewRelocs,eax
	shl	eax,2		;4 bytes per entry.
	add	[edi].NewHeaderStruc.NewSize,eax
	;
	;Set EXE image length and increase file length field.
	;
	mov	eax,SegmentBase	;This is now total length.
	mov	[edi].NewHeaderStruc.NewLength,eax
	mov	[edi].NewHeaderStruc.NewAlloc,eax
	add	[edi].NewHeaderStruc.NewSize,eax
	;
	;Include header in length field.
	;
	add	[edi].NewHeaderStruc.NewSize,size NewHeaderStruc
	;
	;Set entry CS:EIP
	;
	mov	esi,LEAddress
	mov	eax,[esi+18h]	;CS object number.
	dec	eax
	mov	[edi].NewHeaderStruc.NewEntryCS,ax
	mov	eax,[esi+1Ch]	;EIP value.
	mov	[edi].NewHeaderStruc.NewEntryEIP,eax
	;
	;Set entry SS:ESP
	;
	mov	esi,LEAddress
	mov	eax,[esi+20h]	;SS object number.
	dec	eax
	mov	[edi].NewHeaderStruc.NewEntrySS,ax
	mov	eax,[esi+24h]	;ESP value.
	mov	[edi].NewHeaderStruc.NewEntryESP,eax
	;
	clc
	ret
Create3PHeader	endp


;------------------------------------------------------------------------------
;
;Create a 3P format relocation table from the LE fixup tables.
;
CreateRelocations proc near
	@dbgmsg "CreateRelocations start",13,10
	mov	esi,offset BuildRelocsText
	call	LEPrintString
	;
	;Setup a pointer to the object definitions.
	;
	mov	RelocationCount,0
	;
	mov	esi,LEAddress
	mov	ecx,[esi+44h]	;Get number of Objects.
	add	esi,[esi+40h]	;Point to object table.
	mov	ObjectCount,ecx
	mov	ObjectBase,esi
	mov	eax,ObjectList
	add	eax,4
	mov	SegmentList,eax
	;
@@0:
	mov	esi,ObjectBase
	mov	ecx,[esi+10h]	;Get number of pages.
	mov	PageCount,ecx
	mov	PageCount+4,0
	mov	edx,[esi+0Ch]	;Get page table index.
	dec	edx
	mov	ebp,edx		;Set base page map entry.
@@1:	;
	mov	edx,ebp
	mov	esi,LEAddress
	add	esi,[esi+68h]	;Point to fixup page maps.
	mov	ecx,[esi+4+edx*4]	;Get next offset.
	mov	edx,[esi+edx*4]	;Get start offset.
	sub	ecx,edx		;Get number of bytes
	jz	@@4
	;
	mov	esi,LEAddress
	add	esi,[esi+6Ch]	;Point to fixup data.
	add	esi,edx		;Move to start of this pages fixups.
	;
@@2:	mov	al,[esi]		;Get type byte.
	mov	bl,al
	shr	bl,4		;Get single/multiple flag.
	mov	bh,al
	and	bh,15		;Get type.
	inc	esi
	dec	ecx
	mov	al,[esi]		;Get second type byte.
	mov	dl,al
	and	dl,3		;Get internal/external specifier.
	mov	dh,al
	shr	dh,2		;Get destination type.
	inc	esi
	dec	ecx
	;
	mov	ErrorNumber,39
	or	bl,bl		;Check it's a single entry.
	jnz	@@9
	;
	mov	ErrorNumber,40
	or	dl,dl		;Check it's an internal target.
	jnz	@@9
	;
	mov	ErrorNumber,43
	test	dh,111011b		;Check for un-known bits.
	jnz	@@9
	;
	cmp	bh,0010b		;Word segment?
	jz	@@Seg16
	cmp	bh,0111b		;32-bit offset?
	jz	@@32BitOff
	cmp	bh,0110b		;Seg:32-bit offset?
	jz	@@Seg1632BitOff
	cmp	bh,1000b		;32-bit self relative?
	jz	@@Self32Off
	;
	mov	ErrorNumber,41
	jmp	@@9
	;
@@Seg16:	;Deal with a 16-bit segment.
	;
	;EBP	- Page offset within segment.
	;w[esi] - offset within page.
	;b[esi+2] - target object+1.
	;
	mov	ErrorNumber,43
	test	dh,4
	jnz	@@9
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_Memory
	mov	eax,PageCount+4	;Get page number.
	shl	eax,12
	add	edi,eax		;Point to the right page.
	movsx	eax,w[esi]
	or	eax,eax
	js	@@Neg0
	add	edi,eax		;Point to the right offset.
	movzx	eax,b[esi+2]		;Get the target segment.
	dec	eax
	mov	[edi],ax		;Store target.
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_BaseAddress	;Get offset from image start.
	mov	eax,PageCount+4	;Get page number within segment.
	shl	eax,12
	add	edi,eax		;Include page offset.
	movzx	eax,w[esi]
	add	edi,eax		;Include byte offset.
	mov	eax,edi
	call	AddRelocationEntry	;Add it to the list.
	mov	ErrorNumber,36
	jc	@@9
	;
@@Neg0:
	add	esi,2+1
	sub	ecx,2+1
	jmp	@@3
	;
@@32BitOff:	;Deal with a 32-bit offset.
	;
	;EBP	- Page offset within segment.
	;w[esi] - offset within page.
	;b[esi+2] - target object+1
	;w[esi+3] - target offset.
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_Memory
	mov	eax,PageCount+4	;Get page number.
	shl	eax,12
	add	edi,eax		;Point to the right page.
	movsx	eax,w[esi]
	or	eax,eax
	js	@@Neg1
	add	edi,eax		;Point to the right offset.
	movzx	eax,b[esi+2]		;Get the target segment.
	dec	eax
	push	edx
	mov	edx,size _Seg_
	mul	edx
	pop	edx
	add	eax,4		;skip dword count.
	add	eax,ObjectList	;point to target segment details.
	mov	eax,[eax]._Seg_.Seg_BaseAddress	;Get target segments offset from start of image.
	movzx	ebx,w[esi+3]		;Get target offset.
	test	dh,4
	jz	@@Big0
	mov	ebx,[esi+3]		;Get target offset.
@@Big0:	add	eax,ebx
	mov	[edi],eax
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_BaseAddress	;Get offset from image start.
	mov	eax,PageCount+4	;Get page number within segment.
	shl	eax,12
	add	edi,eax		;Include page offset.
	movzx	eax,w[esi]
	add	edi,eax		;Include byte offset.
	mov	eax,edi
	or	eax,1 shl 28		;Mark it as a 32-bit offset.
	call	AddRelocationEntry	;Add it to the list.
	mov	ErrorNumber,36
	jc	@@9
	;
@@Neg1:	add	esi,2+1+2
	sub	ecx,2+1+2
	test	dh,4
	jz	@@3
	add	esi,2
	sub	ecx,2
	jmp	@@3

	;
@@Self32Off:	;Deal with a 32-bit self relative offset.
	;
	;EBP	- Page offset within segment.
	;w[esi] - offset within page.
	;b[esi+2] - target object+1
	;w[esi+3] - target offset.
	;
	mov	edi,SegmentList
	mov	ebx,[edi]._Seg_.Seg_BaseAddress
	mov	edi,[edi]._Seg_.Seg_Memory
	mov	eax,PageCount+4	;Get page number.
	shl	eax,12
	add	ebx,eax
	add	edi,eax		;Point to the right page.
	movsx	eax,w[esi]
	or	eax,eax
	js	@@sfNeg1
	add	ebx,eax
	add	edi,eax		;Point to the right offset.
	movzx	eax,b[esi+2]		;Get the target segment.
	dec	eax
	push	edx
	mov	edx,size _Seg_
	mul	edx
	pop	edx
	add	eax,4		;skip dword count.
	add	eax,ObjectList	;point to target segment details.
	mov	eax,[eax]._Seg_.Seg_BaseAddress	;Get target segments offset from start of image.
	push	ebx
	movzx	ebx,w[esi+3]		;Get target offset.
	test	dh,4
	jz	@@sfBig0
	mov	ebx,[esi+3]		;Get target offset.
@@sfBig0:	add	eax,ebx
	pop	ebx
	add	ebx,4
	sub	eax,ebx
	mov	[edi],eax
	;
@@sfNeg1:
	add	esi,2+1+2
	sub	ecx,2+1+2
	test	dh,4
	jz	@@3
	add	esi,2
	sub	ecx,2
	jmp	@@3

	;
@@Seg1632BitOff: ;Deal with an FWORD fixup by splitting into a seg16 and 32-bit
	;offset relocation entry.
	;
	;EBP	- Page offset within segment.
	;w[esi] - offset within page.
	;b[esi+2] - target object+1
	;w[esi+3] - target offset.
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_Memory
	mov	eax,PageCount+4	;Get page number.
	shl	eax,12
	add	edi,eax		;Point to the right page.
	movsx	eax,w[esi]
	or	eax,eax
	js	@@Neg2
	add	edi,eax		;Point to the right offset.
	add	edi,4		;Point to the seg bit.
	movzx	eax,b[esi+2]		;Get the target segment.
	dec	eax
	mov	[edi],ax		;Store target.
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_BaseAddress	;Get offset from image start.
	mov	eax,PageCount+4	;Get page number within segment.
	shl	eax,12
	add	edi,eax		;Include page offset.
	movzx	eax,w[esi]
	add	edi,eax		;Include byte offset.
	add	edi,4		;Point to the seg bit.
	mov	eax,edi
	call	AddRelocationEntry	;Add it to the list.
	mov	ErrorNumber,36
	jc	@@9
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_Memory
	mov	eax,PageCount+4	;Get page number.
	shl	eax,12
	add	edi,eax		;Point to the right page.
	movzx	eax,w[esi]
	add	edi,eax		;Point to the right offset.
	movzx	eax,b[esi+2]		;Get the target segment.
	dec	eax
	push	edx
	mov	edx,size _Seg_
	mul	edx
	pop	edx
	add	eax,4		;skip dword count.
	add	eax,ObjectList	;point to target segment details.
	mov	eax,[eax]._Seg_.Seg_BaseAddress	;Get target segments offset from start of image.
	movzx	ebx,w[esi+3]		;Get target offset.
	test	dh,4
	jz	@@Big1
	mov	ebx,[esi+3]		;Get target offset.
@@Big1:	add	eax,ebx
	mov	[edi],eax
	;
	mov	edi,SegmentList
	mov	edi,[edi]._Seg_.Seg_BaseAddress	;Get offset from image start.
	mov	eax,PageCount+4	;Get page number within segment.
	shl	eax,12
	add	edi,eax		;Include page offset.
	movzx	eax,w[esi]
	add	edi,eax		;Include byte offset.
	mov	eax,edi
	or	eax,1 shl 28		;Mark it as a 32-bit offset.
	call	AddRelocationEntry	;Add it to the list.
	mov	ErrorNumber,36
	jc	@@9
	;
@@Neg2:	add	esi,2+1+2
	sub	ecx,2+1+2
	test	dh,4
	jz	@@3
	add	esi,2
	sub	ecx,2
	jmp	@@3
	;
@@3:	inc	RelocationCount
	or	ecx,ecx
	jnz	@@2
	;
@@4:	inc	ebp
	inc	PageCount+4
	dec	PageCount
	jnz	@@1
	;
	add	SegmentList,size _Seg_
	add	ObjectBase,18h
	dec	ObjectCount
	jnz	@@0
	;
@@8:
	clc
	ret
	;
@@9:
	stc
	ret
CreateRelocations endp


;------------------------------------------------------------------------------
;
;Add an entry to the relocation list.
;
;On Entry:
;
;EAX	- Item to add.
;
;On Exit:
;
;Carry set on error else,
;
;All registers preserved.
;
AddRelocationEntry proc near
	pushad
	cmp	RelocationList,0
	jnz	@@0
	mov	ecx,4
	call	LEMalloc
	jc	@@9
	mov	RelocationList,esi
	mov	d[esi],0
	;
@@0:
	mov	esi,RelocationList
	mov	ecx,[esi]		;Get current number of entries.
	inc	ecx
	shl	ecx,2		;dword per entry.
	add	ecx,4		;allow for count dword.
	call	LEReMalloc
	jc	@@9
	mov	RelocationList,esi	;store new list address.
	inc	d[esi]		;increase entry count.
	add	esi,ecx
	sub	esi,4		;point to new entry.
	mov	[esi],eax		;store entry.
	;
	clc
	jmp	@@10
	;
@@9:
	stc
@@10:
	popad
	ret
AddRelocationEntry endp


;------------------------------------------------------------------------------
;
;Create 3P version of LE file in memory.
;
Create3PFile	proc	near
	@dbgmsg "Create3PFile enter",13,10
	mov	esi,offset BuildImageText
	call	LEPrintString
	;
	mov	esi,LEAddress
	mov	ecx,[esi+44h]	;Get number of Objects.
	add	esi,[esi+40h]	;Point to object table.
	mov	ObjectCount,ecx
	mov	ObjectBase,esi
	;
@@0:                    ;<--- loop
	cmp	ObjectList,0	;Started object list yet?
	jnz	@@0_0
	mov	ecx,4
	call	LEMalloc
	mov	ErrorNumber,36
	jc	@@9
	mov	ObjectList,esi
	mov	d[esi],0
@@0_0:
	mov	esi,ObjectList
	mov	eax,[esi]		;Get number of entries.
	inc	eax
	mov	edx,size _Seg_
	mul	edx
	add	eax,4
	mov	ecx,eax
	call	LEReMalloc		;Enlarge it.
	mov	ErrorNumber,36
	jc	@@9
	mov	ObjectList,esi
	inc	d[esi]
	add	esi,ecx
	sub	esi,size _Seg_	;Point to new entry.
	mov	edi,esi
	mov	ecx,size _Seg_
	xor	al,al
	rep	stosb		;Clear it out.
	mov	edi,esi
	;
	mov	eax,SegmentBase
	mov	[edi]._Seg_.Seg_BaseAddress,eax
	mov	esi,ObjectBase
	mov	ecx,[esi]		;Get segments size.
;	add	ecx,15
;	and	ecx,not 15
	add	SegmentBase,ecx
	mov	[edi]._Seg_.Seg_Length,ecx	;Set segments size.
	;
	call	LEMalloc
	mov	ErrorNumber,36
	jc	@@9
	mov	[edi]._Seg_.Seg_Memory,esi	;Store segments address.
	pushm	eax,ecx,edi
	mov	edi,esi
	xor	al,al
	rep	stosb
	popm	eax,ecx,edi
	;
	mov	esi,ObjectBase
	mov	eax,[esi+08h]	;Get objects flags.
	xor	ebx,ebx
	test	eax,4		;Executable?
	jnz	@@1
	inc	ebx		;Make it Data.
	test	eax,2		;Writeable?
	jz	@@1
;	add	ebx,2		;Read only data.
@@1:
	shl	ebx,24
	test	eax,2000h		;Big bit set?
	jz	@@2
	or	ebx,1 shl 26		;Force 32-bit.
	jmp	@@3
@@2:	or	ebx,1 shl 25		;Force 16-bit.
@@3:	mov	eax,[edi]._Seg_.Seg_Length
	cmp	eax,100000h		;>1M?
	jc	@@4
	shr	eax,12
	or	eax,1 shl 20
@@4:
	or	ebx,eax		;Include length.
	or	ebx,1 shl 27		;mark target type
	mov	[edi]._Seg_.Seg_Type,ebx	;Store the 3P type.
	;
	mov	esi,ObjectBase
	mov	ecx,[esi+10h]	;Get number of pages.
	mov	edx,[esi+0Ch]	;Get page table index.
	mov	ebp,[edi]._Seg_.Seg_Length	;Get maximum length again.
	mov	edi,[edi]._Seg_.Seg_Memory	;Point to segments memory.
	mov	ebx,LEAddress
	add	ebx,[ebx+80h]
	sub	ebx,LEOffset
	;
@@5:
	mov	eax,edx		;Get page number.
	dec	eax		;make it base 0.
	shl	eax,12		;*4096.
	add	eax,ebx		;Make offset from data pages.
	mov	esi,eax
	;
	mov	eax,4096		;Default page size.
@@6:
	cmp	ebp,eax		;Want whole page?
	jnc	@@7
	mov	eax,ebp		;Force smaller value.
@@7:
	push	ecx
	mov	ecx,eax		;Get length to copy.
	rep	movsb		;Copy this page.
	pop	ecx
	sub	ebp,eax		;Update length remaining.
	inc	edx		;Next page index.
	dec	ecx
	jnz	@@6		;Get all pages.
	;
	add	ObjectBase,18h	;Next object
	dec	ObjectCount
	jnz	@@0		;Do all objects.
	;
	clc
	ret
	;
@@9:
	stc
	ret
Create3PFile	endp


;------------------------------------------------------------------------------
;
;Fetch the specified LE file, just the LE bit not its stub.
;
FetchLEFile	proc	near
	@dbgmsg "FetchLEFile enter",13,10
	mov	esi,offset ReadingLEText
	call	LEPrintString
	;
	mov	edx,offset EXEFileName
	call	LEOpenFile
	mov	ErrorNumber,32
	jc	@@9
	mov	ecx,3ch
	xor	al,al
	call	LESetFilePointer	;Move to the LE bit.
	push	edx
	mov	edx,esp
	mov	ecx,4
	call	LEReadFile		;Read offset to LE.
	pop	edx
	mov	LEOffset,edx
	mov	ErrorNumber,33
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	xor	ecx,ecx
	mov	al,2
	call	LESetFilePointer	;Get file length.
	sub	ecx,edx		;Lose stub section from length.
	xchg	ecx,edx
	xor	al,al
	call	LESetFilePointer	;Point to LE section.
	xchg	ecx,edx
	push	ecx
	add	ecx,4095
	and	ecx,not 4095
	call	LEMalloc		;Get memory for the file.
	pop	ecx
	mov	ErrorNumber,34
	jc	@@9
	pushm	eax,ecx,edi
	add	ecx,4095
	and	ecx,not 4095
	mov	edi,esi
	xor	al,al
	rep	stosb
	popm	eax,ecx,edi
	mov	edx,esi
	call	LEReadFile		;Read the file.
	mov	ErrorNumber,35
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	call	LECloseFile		;Close the file again.
	;
	mov	LEAddress,esi	;Store LE files address.
	mov	LELength,ecx		;Store its length as well.
	clc
	ret
	;
@@9:	stc
	ret
FetchLEFile	endp


;-------------------------------------------------------------------------
;
;Create a file.
;
;On Entry:
;
;EDX	- File name.
;
;On Exit:
;
;Carry set on error and EBX=0 else,
;
;EBX	- File handle.
;
LECreateFile	proc	near
	pushm	eax,ecx
	mov	ah,3ch		;Create function.
	xor	cx,cx		;normal attributes.
	int	21h
	mov	bx,ax
	jnc	l0
	xor	bx,bx
	stc
l0:	movzx	ebx,bx
	popm	eax,ecx
	ret
LECreateFile	endp


;-------------------------------------------------------------------------
;
;Write some data to a file.
;
;On Entry:
;
;EDX	- Address to write from.
;ECX	- Length to write.
;EBX	- file handle.
;
;On Exit:
;
;EAX	- Bytes written.
;
LEWriteFile	proc	near
	pushm	ecx,edx,esi
	xor	esi,esi
@@0:	pushm	ebx,ecx,edx,esi
	cmp	ecx,65535		;size of chunks to load.
	jc	@@1
	mov	ecx,65535		;as close to 64k as can get.
@@1:	mov	ah,40h
	int	21h		;read from the file.
	popm	ebx,ecx,edx,esi
	jc	@@2
	movzx	eax,ax		;get length read.
	add	esi,eax		;update length read counter.
	add	edx,eax		;move memory pointer.
	sub	ecx,eax		;update length counter.
	jz	@@2		;read as much as was wanted.
	or	eax,eax		;did we write anything?
	jz	@@2
	jmp	@@0
@@2:	mov	eax,esi
	popm	ecx,edx,esi
	ret
LEWriteFile	endp


;-------------------------------------------------------------------------
;
;Re-size previously allocated memory.
;
;On Entry:
;
;ECX	- New size.
;ESI	- Current address of memory.
;
;On Exit:
;
;Carry set on error and ESI=0 else,
;
;ESI	- New address of memory.
;
;All other registers preserved.
;
LEReMalloc	proc	near
	pushm	eax,edx
if @Model ne 7
	add	esi,DataLinearBase
endif
	call	ResMemLinear32
	jc	l0
if @Model ne 7
	sub	esi,DataLinearBase
endif
	clc
	jmp	l1
l0:	xor	esi,esi
	stc
l1:	popm	eax,edx
	ret
LEReMalloc	endp


;-------------------------------------------------------------------------
;
;Close a file.
;
;On Entry:
;
;EBX	- file handle.
;
LECloseFile	proc	near
	push	eax
	mov	ah,3eh
	int	21h
	pop	eax
	ret
LECloseFile	endp


;-------------------------------------------------------------------------
;
;Allocate some DS relative memory.
;
;On Entry:
;
;ECX	- Bytes required.
;
;On Exit:
;
;Carry set on error and ESI=0 else,
;
;ESI	- DS relative address of allocated memory.
;
;All other registers preserved.
;
LEMalloc	proc	near
	push	eax
	call	GetMemLinear32
	jc	l0
if @Model ne 7
	sub	esi,DataLinearBase
endif
	clc
	jmp	l1
l0:
	xor	esi,esi
	stc
l1:
	pop	eax
	ret
LEMalloc	endp


;-------------------------------------------------------------------------
;
;Read some data from a file.
;
;On Entry:
;
;EDX	- Address to read to.
;ECX	- length to read.
;EBX	- file handle.
;
;On Exit:
;
;EAX	- bytes read.
;
LEReadFile	proc	near
	pushm	ecx,edx,esi
	xor	esi,esi		;reset length read.
@@0:	pushm	ebx,ecx,edx,esi
	cmp	ecx,65535		;size of chunks to load.
	jc	@@1
	mov	ecx,65535		;as close to 64k as can get.
@@1:	mov	ah,3fh
	int	21h		;read from the file.
	popm	ebx,ecx,edx,esi
	jc	@@2		;DOS error so exit NOW.
	movzx	eax,ax		;get length read.
	add	esi,eax		;update length read counter.
	add	edx,eax		;move memory pointer.
	sub	ecx,eax		;update length counter.
	jz	@@2		;read as much as was wanted.
	or	eax,eax		;did we read anything?
	jnz	@@0
@@2:	mov	eax,esi
	popm	ecx,edx,esi
	ret
LEReadFile	endp


;-------------------------------------------------------------------------
;
;Set the file pointer position for a file.
;
;On Entry:
;
;AL	- method.
;EBX	- handle.
;ECX	- position.
;
;Methods are:
;
;0	- Absolute offset from start.
;1	- signed offset from current position.
;2	- signed offset from end of file.
;
;On Exit:
;
;ECX	- absolute offset from start of file.
;
LESetFilePointer proc near
	pushm	eax,edx
	mov	dx,cx
	shr	ecx,16
	mov	ah,42h		;set pointer function.
	int	21h
	mov	cx,dx
	shl	ecx,16
	mov	cx,ax		;fetch small result.
	popm	eax,edx
	ret
LESetFilePointer endp


;-------------------------------------------------------------------------
;
;Open a file.
;
;On Entry:
;
;EDX	- File name.
;
;On Exit:
;
;Carry set on error and EBX=0 else,
;
;EBX	- File handle.
;
LEOpenFile	proc	near
	push	eax
	mov	ax,3d02h		;Open with read & write access.
	int	21h
	mov	bx,ax
	jnc	l0
	xor	bx,bx
	stc
l0:	pop	eax
	movzx	ebx,bx
	ret
LEOpenFile	endp


;-------------------------------------------------------------------------
;
;Print null terminated string on screen via DOS.
;
;On Entry:
;
;ESI	- pointer to string to print.
;
LEPrintString	proc	near
	pushm	eax,esi,edx
l0:	mov	dl,[esi]
	inc	esi
	or	dl,dl
	jz	l1
	mov	ah,2
	int	21h
	jmp	l0
l1:	popm	eax,esi,edx
	ret
LEPrintString	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
NewCauseWay	proc	near
;
;Write new CauseWay loader to a 3P file.
;
IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET debug1
	mov	ecx,SIZEOF debug1
	mov	ebx,1
	mov	ah,40h
	int	21h
	jmp	med1
debug1	DB	'In NewCauseWay',13,10
med1:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF
	mov	bx,EXEHandle
	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	@@9
	mov	EXEHandle,ax
	;
	;Create temp file.
	;
	mov	edx,offset TempFileName	;get DOS to give us a temp file.
	call	CreateFile
	mov	ErrorNumber,19	;default to can't create output.
	jc	@@9		;oops.
	mov	TempHandle,ax
	;
	;Copy 386SHELL to temp file.
	;
	mov	edx,offset SHELLFileName	;get extender stub (stub from CW.EXE)
	call	OpenFile
	mov	ErrorNumber,9	;no stub file.
	jc	@@9
	mov	SHELLHandle,ax
	;
	mov	edx,offset ProcessStubText
	call	StringPrint
	;

	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,SHELLHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	@@9
	mov	ErrorNumber,9	;stub file error.
	cmp	ax,sizeof MZHdr
	jnz	@@9
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@9
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe5		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe5:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	pushm	cx,dx
	mov	dx,-1bh
	mov	cx,-1
	mov	bx,SHELLHandle
	mov	ax,4201h
	int	21h		;move back to start of CWSTUB.EXE
	popm	cx,dx
	;
	;Get memory for shell.
	;
	mov	ebx,edx
	shl	ebx,16
	mov	bx,cx
;	push	ebx
;	sys	GetMemNear		;get memory for shell.
;	pop	ebx
	mov	ecx,ebx
	call	GetMemLinear32

	mov	ErrorNumber,2
	jc	@@9
	;
	;Read shell into memory.
	;
	pushm	ebx,esi
	mov	ecx,ebx
	mov	edi,esi
	mov	bx,SHELLHandle
	call	ReadFile
	popm	ebx,esi
	mov	ErrorNumber,8
	jc	@@9
	cmp	eax,ebx		;did we read enough?
	jnz	@@9
	pushm	ebx,esi
	;
	;Write shell to temp.
	;
	popm	ebx,esi
	pushm	ebx,esi
	mov	ecx,ebx
	mov	bx,TEMPHandle
	call	WriteFile
	popm	ebx,esi
	jc	@@9
	cmp	eax,ebx		;did we write enough?
	jnz	@@9

IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET debug3
	mov	ecx,SIZEOF debug3
	mov	ebx,1
	mov	ah,40h
	int	21h
	jmp	med3
debug3	DB	'At memory release',13,10
med3:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

;	sys	RelMemNear		;release memory now.
	call	RelMemLinear32

	;
	;Skip past old CauseWay.
	;
	mov	ErrorNumber,8
	mov	bx,EXEHandle
	mov	edx,offset exehdr	;somewhere to put the info.
	mov	ecx,sizeof MZHdr	;size of it.
	mov	ah,3fh
	int	21h
	jc	@@9
	cmp	ax,sizeof MZHdr		;did we read right amount?
	jnz	@@9
	;
	;Check for normal MZ header first.
	;
	mov	ErrorNumber,7
	cmp	exehdr.Signature,'ZM'	;Normal EXE?
	jnz	@@9
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe6		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe6:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	pushm	cx,dx
	;
	;Find out where the end of the file is.
	;
	xor	cx,cx
	mov	dx,cx
	mov	al,2
	mov	bx,EXEHandle
	call	SetFilePointer	;move to the end of the file.
	mov	ax,cx
	mov	bx,dx
	popm	cx,dx
	sub	ax,cx
	sbb	bx,dx		;get remaining length.
	pushm	ax,bx
	mov	al,0
	mov	bx,EXEHandle
	call	SetFilePointer	;point to the 3P header again.
	popm	ax,bx
	shl	ebx,16
	mov	bx,ax

IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET debug4
	mov	ecx,SIZEOF debug4
	mov	ebx,1
	mov	ah,40h
	int	21h
	jmp	med4
debug4	DB	'At get memory',13,10
med4:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

;	push	ebx
;	sys	GetMemNear		;get some memory for it.
;	pop	ebx
	mov	ecx,ebx
	call	GetMemLinear32

	mov	ErrorNumber,2
	jc	@@9
	;
	;Read real file into memory.
	;
	mov	ErrorNumber,8
	pushm	esi,ebx
	mov	ecx,ebx
	mov	edi,esi
	push	ds
	pop	es
	mov	bx,EXEHandle
	call	ReadFile
	popm	esi,ecx
	jc	@@9
	cmp	eax,ecx
	jc	@@9
	;
	;Write real file to temp.
	;
	pushm	esi,ecx
	mov	bx,TEMPHandle
	call	WriteFile
	popm	esi,ecx
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
;	sys	RelMemNear		;release the memory.
	call	RelMemLinear32

IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET debug5
	mov	ecx,SIZEOF debug5
	mov	ebx,1
	mov	ah,40h
	int	21h
	jmp	med5
debug5	DB	'At file close',13,10
med5:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

	;
	;Close the files.
	;
	mov	bx,TEMPHandle
	call	CloseFile		;close the temp file.
	mov	TEMPHandle,0
	mov	bx,EXEHandle
	call	CloseFile		;close the file again.
	mov	EXEHandle,0
	mov	bx,SHELLHandle
	call	CloseFile
	mov	SHELLHandle,0
	;

IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET EXEFileName
med6a:
	cmp	BYTE PTR [edx],0
	je	med6
	mov	ecx,1
	mov	ebx,1
	mov	ah,40h
	int	21h
	inc	edx
	jmp	med6a
med6:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

	;Delete origional .EXE
	;
	mov	edx,offset EXEFileName	;get file name mask.
	mov	ah,41h
	int	21h
	mov	ErrorNumber,8
	jc	@@9
	;

IFDEF DEBUG
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	ds
	mov	edx,OFFSET debug2
	mov	ecx,SIZEOF debug2
	mov	ebx,1
	mov	ah,40h
	int	21h
	jmp	med2
debug2	DB	'At rename',13,10
med2:
	pop	ds
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
ENDIF

	;now rename it.
	;
	mov	edx,offset TempFileName
	mov	edi,offset EXEFileName		;get file name mask.
	mov	ah,56h
	int	21h
	jc	@@9
	;
	mov	ErrorNumber,0
	xor	ax,ax
	ret
	;
@@9:	mov	ax,-1
	or	ax,ax
	ret
NewCauseWay	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
SetSystemConfig proc near
;
;Set Flags entry in header.
;
	xor	eax,eax
	;
	cmp	OptionTable+'D',0	;Dual mode?
	jz	@@2
	or	eax,65536		;enable dual mode.

; don't set data bit under dual mode because it can confuse things
;  why, I don't know, it just does
;  MED 06/11/96
	or	eax,16384		;set 16 bit code segments.
;	or	eax,1+16384		;16 bit code.

	jmp	@@0
	;
@@2:	cmp	OptionTable+'3',0
	jnz	@@0
	or	eax,1+16384		;16 bit code.
@@0:	;
	cmp	OptionTable+'L',0	;LDT ?
	jz	@@1
	or	eax,128
@@1:	;
	mov	esi,offset NewHeader
	mov	[esi].NewHeaderStruc.NewFlags,eax
	;
	;Get auto stack size.
	;
	mov	ErrorNumber,27
	cmp	OptionTable+'S',0
	jz	@@NoAutoESP
	mov	esi,d[OptionTable+128+('S'*4)]
	or	esi,esi
	jz	System
	xor	edx,edx
@@ss0:	movzx	eax,b[esi]
	or	al,al
	jz	@@ss1
	cmp	al,'0'
	jc	System
	cmp	al,'9'+1
	jnc	System
	sub	al,'0'
	push	eax
	mov	eax,10
	mul	edx
	mov	edx,eax
	pop	eax
	add	edx,eax
	inc	esi
	jmp	@@ss0
@@ss1:	mov	esi,offset NewHeader
	mov	[esi].NewHeaderStruc.NewAutoStack,edx
	;
@@NoAutoESP:	ret
SetSystemConfig endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
ExeAndMap2NewExe proc near
	;
	;Setup hard wired flags, like 16/32 bit etc.
	;
	mov	ecx,ebx
	call	SetSystemConfig
	;
	call	ReadMapFile		;process map file details.
	jnz	@@9
	;
	call	ProcessFile		;do the nitty gritty.
	jnz	@@9
	;
	mov	ErrorNumber,0	;clear error number.
@@9:	ret
ExeAndMap2NewExe endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
NewExeInfo	proc	near
;
;Display details of file in CauseWay format format.
;
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	bx,EXEHandle
	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	@@9
	mov	EXEHandle,ax
	;
	mov	ErrorNumber,8
	mov	bx,ax
	mov	edx,offset exehdr	;somewhere to put the info.
	mov	ecx,sizeof MZHdr	;size of it.
	mov	ah,3fh
	int	21h
	jc	@@9
	cmp	ax,sizeof MZHdr		;did we read right amount?
	jnz	@@9
	;
	;Check for normal MZ header first.
	;
	mov	ErrorNumber,7
	cmp	exehdr.Signature,'ZM'	;Normal EXE?
	jz	@@10
	;
	;Move back to EXE details.
	;
	mov	dx,-(1bh)
	mov	cx,-1
	mov	ax,4201h
	int	21h		;move back to start of the header.
	jmp	@@Look3P
@@10:	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe7		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe7:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	xchg	cx,dx		;swap round for DOS.
	mov	ax,4200h		;set absolute position.
	int	21h
	;
@@Look3P:	mov	ErrorNumber,8
	mov	bx,EXEHandle
	mov	edx,offset NewHeader	;somewhere to put the info.
	mov	cx,size NewHeaderStruc	;size of it.
	mov	ah,3fh
	int	21h
	jc	@@9
	mov	ErrorNumber,7
	cmp	ax,size NewHeaderStruc	;did we read right amount?
	jnz	@@9
	cmp	w[NewHeader],'P3'	;ID ok?
	jnz	@@9
	;
	mov	esi,offset NewHeader	;somewhere to put the info.
	mov	edx,offset bit16text
	test	[esi].NewHeaderStruc.NewFlags,1
	jnz	@@its16
	mov	edx,offset bit32text
	test	[esi].NewHeaderStruc.NewFlags,65536
	jz	@@its16
	mov	edx,offset bitdualtext
@@its16:	mov	ah,9
	int	21h
	;
	mov	esi,offset NewHeader	;somewhere to put the info.
	mov	edx,offset SpeedLoadText
	test	[esi].NewHeaderStruc.NewFlags,1 shl 30
	jnz	@@nospeed
	mov	edx,offset CarriageReturn2
@@NoSpeed:	mov	ah,9
	int	21h

	;
	mov	edx,offset NewSizeT
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewSize
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewLengthT
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewLength
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewAllocT
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewAlloc
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewSegmentsT
	call	StringPrint
	mov	esi,offset NewHeader
	movzx	eax,[esi].NewHeaderStruc.NewSegments
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewRelocsT
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewRelocs
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewEntryEIPT
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewEntryEIP
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewEntryCSt
	call	StringPrint
	mov	esi,offset NewHeader
	movzx	eax,[esi].NewHeaderStruc.NewEntryCS
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewEntryESPt
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewEntryESP
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewEntrySSt
	call	StringPrint
	mov	esi,offset NewHeader
	movzx	eax,[esi].NewHeaderStruc.NewEntrySS
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	mov	edx,offset NewEntryAutoSS
	call	StringPrint
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewAutoStack
	mov	ecx,8
	mov	edi,offset LineBuffer
	call	Bin2Hex
	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	;See if segment/relocation details are required.
	;
	mov	esi,d[OptionTable+128+("I"*4)]
	or	esi,esi
	jz	@@8
	;
	mov	edx,offset ReadingMainText
	call	StringPrint
	;
	;Get segment definition table memory.
	;
	mov	ErrorNumber,2
	mov	esi,offset NewHeader

;	movzx	ebx,[esi].NewHeaderStruc.NewSegments	;get number of segments.
;	shl	ebx,3		;8 bytes per entry.
;	sys	GetMemNear
	movzx	ecx,[esi].NewHeaderStruc.NewSegments	;get number of segments.
	shl	ecx,3		;8 bytes per entry.
	call	GetMemLinear32
	jc	@@9
	mov	SegmentList,esi
	;
	;Read segment definitions.
	;
	mov	ErrorNumber,8
	mov	esi,offset NewHeader
	movzx	ecx,[esi].NewHeaderStruc.NewSegments	;get number of segments.
	shl	ecx,3		;8 bytes per entry.
	mov	bx,EXEHandle
	mov	edi,SegmentList
	push	ecx
	call	ReadFile
	pop	ecx
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Get relocation table memory.
	;
	mov	ErrorNumber,2
	mov	esi,offset NewHeader

;	mov	ebx,[esi].NewHeaderStruc.NewRelocs	;get number of relocations
;	shl	ebx,2		;4 bytes per entry.
;	sys	GetMemNear
	mov	ecx,[esi].NewHeaderStruc.NewRelocs	;get number of relocations
	shl	ecx,2		;4 bytes per entry.
	call	GetMemLinear32
	jc	@@9
	mov	RelocSegment,esi
	;
	;Read relocations.
	;
	mov	ErrorNumber,8
	mov	esi,offset NewHeader
	mov	ecx,[esi].NewHeaderStruc.NewRelocs	;get number of relocations.
	shl	ecx,2		;4 bytes per entry.
	mov	bx,EXEHandle
	mov	edi,RelocSegment
	push	ecx
	call	ReadFile
	pop	ecx
	jc	@@9
	cmp	eax,ecx
	jnz	@@9
	;
	;Get image memory.
	;
	mov	ErrorNumber,2
	mov	esi,offset NewHeader

;	mov	ebx,[esi].NewHeaderStruc.NewAlloc	;get memory size required.
;	sys	GetMemNear
	mov	ecx,[esi].NewHeaderStruc.NewAlloc	;get memory size required.
	call	GetMemLinear32
	jc	@@9
	mov	EXESegment,esi
	;
	mov	ErrorNumber,8
	mov	esi,offset NewHeader
if 0
	test	[esi].NewHeaderStruc.NewFlags,1 shl 31	;compressed?
	jz	@@NotComp
	mov	bx,EXEHandle
	mov	edi,EXESegment
	sys	cwcLoad
	or	ax,ax
	jnz	@@9
	jmp	@@ImageLoaded
	;
endif    
@@NotComp:
	mov	edi,EXESegment
	mov	ecx,[esi].NewHeaderStruc.NewLength	;get image length.
	mov	bx,EXEHandle
	push	ecx
	call	ReadFile		;read the file.
	pop	ecx
	jc	@@9		;problems problems.
	cmp	eax,ecx		;did we get right amount?
	jnz	@@9
	;
@@ImageLoaded:	mov	bx,EXEHandle
	call	CloseFile
	mov	EXEHandle,0
	;
	;Check if seg details needed.
	;
	mov	esi,d[OptionTable+128+("I"*4)]
@@5:	lodsb
	or	al,al
	jz	@@6
	call	UpperChar
	cmp	al,"S"
	jnz	@@5
	;
	;Display header.
	;
	mov	edx,offset SegmentStuff
	call	StringPrint
	;
	;Display details.
	;
	mov	esi,offset NewHeader
	movzx	ecx,[esi].NewHeaderStruc.NewSegments	;get number of segments.
	mov	esi,SegmentList
	xor	eax,eax
@@0:	push	eax
	pushm	ecx,esi
	mov	edi,offset LineBuffer
	mov	ecx,4
	call	Bin2Hex
	mov	b[edi],' '
	inc	edi
	mov	eax,0[esi]
	mov	ecx,8
	call	Bin2Hex
	mov	b[edi],' '
	inc	edi
	popm	ecx,esi
	pushm	ecx,esi
	mov	eax,4[esi]
	and	eax,0fffffh		;mask to 20 bits.
	test	d[esi+4],1 shl 20	;G bit set?
	jz	@@1
	shl	eax,12
	or	eax,4095
@@1:	mov	ecx,8
	call	Bin2Hex
	mov	b[edi],' '
	mov	b[edi+1],0
	mov	edx,offset LineBuffer
	call	StringPrint
	popm	ecx,esi
	pushm	ecx,esi
	mov	eax,4[esi]
	shr	eax,21		;move type into useful place.
	and	eax,0fh		;isolate type.
	mov	edx,[SegClassList][eax*4]
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	popm	ecx,esi
	pop	eax
	inc	eax
	add	esi,8
	dec	ecx
	jnz	@@0
	;
	;See if relocation details are needed.
	;
@@6:	mov	esi,d[OptionTable+128+("I"*4)]
@@7:	lodsb
	or	al,al
	jz	@@8
	call	UpperChar
	cmp	al,"R"
	jnz	@@7
	;
	;Display header.
	;
	mov	edx,offset RelocStuff
	call	StringPrint
	;
	;Display details.
	;
	mov	esi,offset NewHeader
	mov	ecx,[esi].NewHeaderStruc.NewRelocs	;get number of relocations.
	mov	esi,RelocSegment
	xor	eax,eax
@@2:	push	eax
	pushm	ecx,esi
	mov	edi,offset LineBuffer
	mov	ecx,4
	call	Bin2Hex
	mov	b[edi],' '
	inc	edi
	mov	eax,[esi]
	shr	eax,28
	add	al,"0"
	mov	b[edi],al
	inc	edi
	mov	b[edi]," "
	inc	edi
	mov	eax,[esi]
	and	eax,0FFFFFFFh
	mov	ecx,8
	call	Bin2Hex
	mov	b[edi],' '
	inc	edi
	mov	b[edi],' '
	inc	edi
	mov	b[edi],' '
	inc	edi
	popm	ecx,esi
	pushm	ecx,esi
	mov	esi,[esi]
	mov	eax,esi
	shr	eax,28
	and	esi,0FFFFFFFh
	mov	ebx,offset NewHeader
	cmp	esi,[ebx].NewHeaderStruc.NewLength	;check against image size.
	jc	@@3
@@RelInv:	mov	esi,offset invalidtext
	movsd
	movsd
	jmp	@@4
	;
@@3:	add	esi,EXESegment
	or	eax,eax
	jz	@@Seg16
	dec	eax
	jz	@@Offset32
	jmp	@@RelInv
	;
@@Offset32:	mov	eax,[esi]
	mov	ecx,8
	call	Bin2Hex
	mov	d[edi],"    "
	add	edi,4
	cmp	eax,[ebx].NewHeaderStruc.NewLength	;check against image size.
	jnc	@@RelInv
	jmp	@@4
	;
@@Seg16:	movzx	eax,w[esi]
	push	ax
	mov	ecx,8
	call	Bin2Hex
	mov	d[edi],"    "
	add	edi,4
	pop	ax
	mov	ebx,offset NewHeader
	cmp	ax,[ebx].NewHeaderStruc.NewSegments
	jc	@@4
	mov	esi,offset invalidtext
	movsd
	movsd
	;
@@4:	mov	b[edi],0
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
	popm	ecx,esi
	pop	eax
	inc	eax
	add	esi,4
	dec	ecx
	jnz	@@2
	;
@@8:	mov	ErrorNumber,0
@@9:	ret
NewExeInfo	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=-=
Bin2Hex	proc	near
;
;Convert number into ASCII Hex version.
;
;On Entry:-
;
;EAX	- Number to convert.
;ECX	- Digits to do.
;EDI	- Buffer to put string in.
;
	pushm	edi,ecx
	mov	ebx,offset HexTable
	add	edi,ecx
	dec	edi
	mov	edx,eax
@@0:	mov	al,dl
	shr	edx,4
	and	al,15
	xlat
	mov	[edi],al
	dec	edi
	loop	@@0
	popm	edi,ecx
	add	edi,ecx
	ret
HexTable	db '0123456789ABCDEF'
Bin2Hex	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
ProcessFile	proc	near
;
;Generate new style relocation table and write new .EXE file.
;
	@dbgmsg "ProcessFile start",13,10
	mov	bx,EXEHandle
	call	CloseFile
	mov	edx,offset EXEFileName
	call	OpenFile		;Open the .EXE file.
	mov	ErrorNumber,6	;default to not found.
	jc	@@9
	mov	EXEHandle,ax
	;
	;Create temp file.
	;
	mov	edx,offset TempFileName	;get DOS to give us a temp file.
	call	CreateFile
	mov	ErrorNumber,19	;default to can't create output.
	jc	@@9		;oops.
	mov	TempHandle,ax
	;
	;Copy 386SHELL to temp file.
	;
	cmp	OptionTable+'B',0
	jz	@@NoBind
	;
	mov	edx,offset SHELLFileName	;get extender stub CWSTUB.EXE
	call	OpenFile
	mov	ErrorNumber,9	;no stub file.
	jc	@@9
	mov	SHELLHandle,ax
	;
	mov	edx,offset ProcessStubText
	call	StringPrint
	;
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,SHELLHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	@@9
	mov	ErrorNumber,9	;stub file error.
	cmp	ax,sizeof MZHdr
	jnz	@@9
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@9
	@dbgmsg "ProcessFile - stub file exists and has correct header",13,10
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe8		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe8:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	mov	cx,ax
	pushm	cx,dx
	mov	dx,-1bh
	mov	cx,-1
	mov	bx,SHELLHandle
	mov	ax,4201h
	int	21h		;move back to start of 386SHELL.
	popm	cx,dx
	;
	;Get memory for shell.
	;
	mov	ebx,edx
	shl	ebx,16
	mov	bx,cx

;	push	ebx
;	sys	GetMemNear		;get memory for shell.
;	pop	ebx
	mov	ecx,ebx
	call	GetMemLinear32
	mov	ErrorNumber,2
	jc	@@9
	@dbgmsg "ProcessFile - memory alloc ok",13,10
	;
	;Read shell into memory.
	;
	pushm	ebx,esi
	mov	ecx,ebx
	mov	edi,esi
	mov	bx,SHELLHandle
	call	ReadFile
	popm	ebx,esi
	mov	ErrorNumber,8
	jc	@@9
	cmp	eax,ebx		;did we read enough?
	jnz	@@9
	@dbgmsg "ProcessFile - stub read ok",13,10
	pushm	ebx,esi
	;
	;Write shell to temp.
	;
	popm	ebx,esi
	pushm	ebx,esi
	mov	ecx,ebx
	mov	bx,TEMPHandle
	call	WriteFile
	popm	ebx,esi
	jc	@@9
	cmp	eax,ebx		;did we write enough?
	jnz	@@9
;	sys	RelMemNear		;release memory now.
	call	RelMemLinear32
	@dbgmsg "ProcessFile - stub write to tmp file ok",13,10

	;
@@NoBind:	;Process .EXE relocation table and produce new format header.
	;
	mov	edx,offset exehdr
	mov	ecx,sizeof MZHdr
	mov	bx,EXEHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	mov	ErrorNumber,8	;default to IO error.
	jc	@@9
	mov	ErrorNumber,7	;bad header.
	cmp	ax,sizeof MZHdr
	jnz	@@9
	cmp	exehdr.Signature,'ZM'	;Correct ID?
	jnz	@@9

	@dbgmsg "ProcessFile - .exe read ok",13,10
	;
	;Get header size in bytes.
	;
	mov	bx,exehdr.HeaderSize	;Work out header size.
	xor	cx,cx		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	mov	w[ExeSize1],bx	;/
	mov	w[ExeSize1+2],cx	;/
	;
	;Get exe image size in bytes.
	;
	mov	ax,exehdr._Length+2	;get length in 512 byte blocks
	cmp	exehdr._Length,0
	je	medexe9		; not rounded if no modulo

	dec	ax		;lose 1 cos its rounded up

medexe9:
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,exehdr._Length	;add length mod 512
	adc	dx,0		;add any carry to dx
	sub	ax,bx		;remove header total size.
	sbb	dx,cx		;/
	mov	w[ExeSize2],ax	;/
	mov	w[ExeSize2+2],dx	;/
	;
	shl	edx,16
	mov	dx,ax
	mov	esi,offset NewHeader
	mov	[esi].NewHeaderStruc.NewLength,edx	;store EXE image length in the header.
	mov	eax,SegmentTotal
	mov	[esi].NewHeaderStruc.NewSegments,ax
	movzx	eax,exehdr.RelocNum
	mov	[esi].NewHeaderStruc.NewRelocs,eax
	;
	movzx	eax,exehdr.EntryIP
	mov	[esi].NewHeaderStruc.NewEntryEIP,eax	;setup entry offset.
	;
	cmp	OptionTable+'Z',0
	jz	@@NormStack
	cmp	OptionTable+'N',0
	jnz	@@GotStackNear
@@NormStack:	movzx	eax,exehdr.EntrySP
	mov	[esi].NewHeaderStruc.NewEntryESP,eax	;setup ESP offset.
@@GotStackNear: ;
	movzx	eax,exehdr.EntryCS	;get entry CS.
	mov	edi,SegmentList	;make segment details addressable.
	mov	ecx,SegmentTotal	;get number of entries to check.
	xor	dx,dx		;reset entry number.
	cmp	OptionTable+'N',0
	jz	@@4
	add	edi,16
	inc	dx
@@4:	mov	ebx,[edi+0]		;get segment base.
	shr	ebx,4		;convert to paragraph.
	cmp	eax,ebx		;this the one we're after?
	jz	@@5
	add	edi,16
	inc	dx		;update entry counter.
	loop	@@4
	xor	dx,dx		;reset to zero.
	mov	edi,SegmentList	;/
@@5:	mov	[esi].NewHeaderStruc.NewEntryCS,dx	;store segment entry number.
	mov	ErrorNumber,20
	cmp	w[edi+8],0
	jnz	@@9		;entry point is not in a code segment.
	;
	cmp	OptionTable+'Z',0
	jz	@@NormStack2
	cmp	OptionTable+'N',0
	jnz	@@10
@@NormStack2:	;
	movzx	eax,exehdr.StackSeg	;get entry SS.
	mov	edi,SegmentList	;make segment details addressable.
	mov	ecx,SegmentTotal	;get number of entries to check.
	xor	dx,dx		;reset entry number.
@@6:	cmp	w[edi+8],2		;stack segment?
	jnz	@@6_0
	mov	ebx,[edi+0]		;get segment base.
	shr	ebx,4		;convert to paragraph.
	cmp	eax,ebx		;this the one we're after?
	jz	@@7
@@6_0:	add	edi,16
	inc	dx		;update entry counter.
	loop	@@6
	;
	cmp	exehdr.EntrySP,0
	jz	@@8
	mov	ErrorNumber,25	;force an error.
	jmp	@@9
@@7:	mov	[esi].NewHeaderStruc.NewEntrySS,dx	;store segment entry number.
	jmp	@@10
@@8:	mov	[esi].NewHeaderStruc.NewEntrySS,0
	;
@@10:	;Get some memory for the relocation table.
	;
	mov	edx,offset ProcessExeText	;let user know whats happening.
	call	StringPrint
	;

;	movzx	ebx,exehdr.RelocNum	;get number of relocation items.
;	or	ebx,ebx
;	jz	@@NoRelocMem
;	shl	ebx,2
;	sys	GetMemNear
	movzx	ecx,exehdr.RelocNum	;get number of relocation items.
	test	ecx,ecx
	jz	@@NoRelocMem
	shl	ecx,2
	call	GetMemLinear32
	mov	ErrorNumber,2	;not enough memory.
	jc	@@9
	mov	RelocSegment,esi	;stow the memory address.
	;
	mov	dx,exehdr.RelocFirst
	mov	cx,0
	mov	bx,EXEHandle
	mov	ax,4200h
	int	21h		;move to relocation table.
	;
	movzx	ecx,exehdr.RelocNum
	shl	ecx,2
	mov	bx,EXEHandle
	mov	edi,RelocSegment
	call	ReadFile		;read the relocation table.
	mov	ErrorNumber,8
	jc	@@9
	;
	;Convert relocation table to linear offsets.
	;
	movzx	ecx,exehdr.RelocNum	;number of entries.
	mov	esi,RelocSegment	;list of relocations.
@@1:	pushm	ecx,esi
	movzx	eax,w[esi+2]		;get segment offset.
	shl	eax,4		;make it linear.
	movzx	ebx,w[esi+0]		;get offset.
	add	eax,ebx		;add in offset.
	mov	d[esi],eax		;store linear offset.
	popm	ecx,esi
	add	esi,4
	loop	@@1
	;
@@NoRelocMem:	;Get some memory for the exe image.
	;

;	mov	ebx,d[ExeSize2]	;get exe image size.
;	sys	GetMemNear
	mov	ecx,d[ExeSize2]	;get exe image size.
	call	GetMemLinear32
	mov	ErrorNumber,2
	jc	@@9
	mov	EXESegment,esi
	;
	mov	dx,w[ExeSize1]	;get image file offset.
	mov	cx,w[ExeSize1+2]
	mov	bx,EXEHandle
	mov	ax,4200h
	int	21h		;move to start of exe image.
	mov	ErrorNumber,8
	jc	@@9
	mov	edi,EXESegment
	mov	bx,EXEHandle
	mov	ecx,d[ExeSize2]
	call	ReadFile		;read exe image into memory.
	;
	mov	edx,offset GenerateExeText
	call	StringPrint
	;
	mov	bx,TEMPHandle
	mov	cx,0
	mov	dx,0
	mov	ax,4201h
	int	21h
	shl	edx,16
	mov	dx,ax
	mov	Real3POffset,edx
	;
	;Write main header.
	;
	mov	edx,offset NewHeader	;write the header to make space.
	mov	bx,TempHandle
	mov	cx,size NewHeaderStruc
	mov	ah,40h
	int	21h
	;
	;Write segment definitions.
	;
	mov	ecx,SegmentTotal	;get number to do.
	mov	esi,SegmentList	;the segment definitions.
@@0:	pushm	ecx,esi
	mov	eax,[esi+0]		;get the base.
	mov	d[LineBuffer],eax
	mov	eax,[esi+4]		;get the limit.
	cmp	eax,100000h
	jc	@@Small
	cmp	eax,-1
	jz	@@NoRoundUp
	add	eax,4095
@@NoRoundUp:	shr	eax,12		;lose bottom bits.
	or	eax,1 shl 20		;Set our version of the G bit.
@@Small:	movzx	ebx,w[esi+8]		;get segment type.
	and	ebx,15		;allows same variety as real selectors.
	shl	ebx,21		;put it somewhere useful.
	or	eax,ebx
	mov	d[LineBuffer+4],eax	;stow it in the table.
	mov	edx,offset LineBuffer
	mov	cx,8
	mov	bx,TEMPHandle
	mov	ah,40h
	int	21h		;write this entry.
	popm	ecx,esi
	mov	ErrorNumber,8
	jc	@@9
	cmp	ax,8
	jnz	@@9
	add	esi,16
	loop	@@0
	;
	;Write relocation table.
	;
	movzx	ecx,exehdr.RelocNum	;number of entries.
	shl	ecx,2
	mov	esi,RelocSegment	;where they are.
	mov	bx,TEMPHandle
	call	WriteFile
	;
	;Update exe image with real segment numbers.
	;
	movzx	ecx,exehdr.RelocNum	;number of entries.
	jecxz	@@NoReloc
	mov	esi,RelocSegment	;the relocations.
@@3:	pushm	ecx,esi
	mov	esi,[esi]		;get relocation offset.
	add	esi,ExeSegment	;offset into exe image.
	movzx	eax,w[esi]		;get value that needs relocating.
	;
	mov	ecx,SegmentTotal	;number of segments to scan.
	mov	edi,SegmentList	;list of segment definitions.
	xor	edx,edx		;reset segment number.
@@30:	cmp	OptionTable+'N',0	;check for flat mode.
	jz	@@NotFlat
	mov	edx,0		;force to data referance.
	jmp	@@31
	;
@@NotFlat:	mov	ebx,[edi+0]		;get current segments base.
	shr	ebx,4		;round it down.
	cmp	eax,ebx
	jz	@@31
	add	edi,16		;next segment definition.
	inc	edx		;update segment number.
	loop	@@30
	mov	ErrorNumber,23
	popm	ecx,esi
	jmp	@@9
	;
@@31:	mov	[esi],dx		;store new segment value.
	popm	ecx,esi
	add	esi,4		;next relocation entry.
	loop	@@3
	;
@@NoReloc:	;Write exe image.
	;
	mov	ecx,d[ExeSize2]
	mov	esi,ExeSegment
	mov	bx,TEMPHandle
	call	WriteFile
	;
	;Calculate file size.
	;
	mov	bx,TEMPHandle
	mov	cx,0
	mov	dx,0
	mov	ax,4201h
	int	21h
	shl	edx,16
	mov	dx,ax
	sub	edx,Real3POffset
	mov	esi,offset NewHeader
	mov	[esi].NewHeaderStruc.NewSize,edx
	;
	;Now go back and write the real header.
	;
	mov	edx,Real3POffset
	mov	cx,dx
	shr	edx,16
	xchg	cx,dx
	mov	bx,TEMPHandle
	mov	ax,4200h
	int	21h
	mov	edx,offset NewHeader	;write the header to make space.
	mov	bx,TempHandle
	mov	cx,size NewHeaderStruc
	mov	ah,40h
	int	21h
	;
	;Close the files.
	;
	mov	bx,TEMPHandle
	call	CloseFile		;close the temp file.
	mov	TEMPHandle,0
	mov	bx,EXEHandle
	call	CloseFile		;close the file again.
	mov	EXEHandle,0
	mov	bx,SHELLHandle
	call	CloseFile
	mov	SHELLHandle,0
	;
	;Delete origional .EXE
	;
	mov	edx,offset EXEFileName	;get file name mask.
	mov	ah,41h
	int	21h
	mov	ErrorNumber,8
	jc	@@9
	;
	;now rename it.
	;
	mov	edx,offset TempFileName
	mov	edi,offset EXEFileName		;get file name mask.
	mov	ah,56h
	int	21h
	jc	@@9
	;
	mov	ErrorNumber,0
	xor	ax,ax
	@dbgmsg "ProcessFile exit, ax=0 (ok)",13,10
	ret
	;
@@9:
	@dbgmsg "ProcessFile exit, ax=-1 (error)",13,10
	mov	ax,-1
	or	ax,ax
	ret
ProcessFile	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
ReadMapFile	proc	near
;
;Convert the input files.
;
	;
	;Get memory for segment list.
	;
	mov	ErrorNumber,2	;not enough memory.

;	mov	ebx,16*8192		;allow for maximum segments.
;	sys	GetMemNear
	mov	ecx,16*8192
	call	GetMemLinear32
	jc	@@9
	mov	SegmentList,esi
	;
	;Try and open the map file.
	;
	mov	ErrorNumber,10	;default to no map file.
	mov	edx,offset MapFileName
	call	OpenFile		;try and open the map file.
	jc	@@9
	mov	MapHandle,ax		;store the handle.
	mov	edx,offset ProcessMapText
	call	StringPrint

	call	InitFileBuffer

	;
	;Look for segment list ID string.
	;
@@LookID:	mov	edi,offset LineBuffer
	mov	bx,MapHandle
	call	ReadLine		;read a line from the map file.
	mov	ErrorNumber,8	;default to general IO error.
	jc	@@9
	mov	ErrorNumber,11	;default to no segment header.
	or	ax,ax		;anything to look at?
	jnz	@@9
	mov	ErrorNumber,18
	cmp	cx,1024
	jnc	@@90
	;
	;See if right text on this line.
	;
	mov	esi,offset SegHeaderText
	mov	edi,offset LineBuffer
	;
@@0:	cmp	b[esi],' '		;need to skip white space.
	jz	@@1
	cmp	b[esi],9
	jnz	@@2
@@1:	inc	esi
	jmp	@@0
	;
@@2:	cmp	b[edi],' '		;skip white space.
	jz	@@3
	cmp	b[esi],9
	jnz	@@4
@@3:	inc	edi
	jmp	@@2
	;
@@4:	cmp	b[esi],0		;end of the header string?
	jz	@@5
	cmp	b[edi],0		;end of the line buffer?
	jz	@@LookID
	;
	mov	al,[esi]
	call	UpperChar		;case insensitive search.
	xchg	ah,al
	mov	al,[edi]
	call	UpperChar
	cmp	al,ah
	jnz	@@LookID		;next line if they don't match
	inc	esi
	inc	edi
	jmp	@@0		;next char if they do.
	;
@@5:	;Find the next none blank line.
	;
	mov	bx,MapHandle
	mov	edi,offset LineBuffer
	call	ReadLine		;read another line.
	mov	ErrorNumber,8	;default to general IO error.
	jc	@@9
	or	ax,ax		;did we get anything?
	jnz	@@DoSegs		;bloody strange! No segments to process.
	or	cx,cx		;blank line?
	jz	@@5		;keep reading till something happens.
	mov	ErrorNumber,18
	cmp	cx,1024
	jnc	@@90
	;
	;Looks like we can start fetching segment values at last.
	;
	mov	eax,SegmentList	;get segment buffer address.
	mov	SegCurrent,eax	;setup current pointer.
	;
@@LookSeg:	mov	esi,offset LineBuffer	;source data.
	mov	edi,offset SegLayout-1	;definition of data layout.
	mov	ebx,SegCurrent
	mov	d[ebx+0],0		;reset linear base.
	mov	d[ebx+4],0		;reset byte limit.
	mov	w[ebx+8],0		;reset type.
	;
@@6:	cmp	b[esi],' '		;skip leading white space.
	jz	@@7
	cmp	b[esi],9
	jnz	@@8
@@7:	inc	esi
	jmp	@@6
	;
@@8:	inc	edi		;move to next item on the list.
	cmp	b[edi],-1		;finished scan?
	jz	@@NextSeg
	cmp	b[edi],0		;ignoring this column?
	jz	@@Ignore
	cmp	b[edi],1		;start address?
	jz	@@Start
	cmp	b[edi],2		;length?
	jz	@@Length
	jmp	@@Class		;must be 3 (class) then.
	;
@@Ignore:	cmp	b[esi],' '		;scan till more white space.
	jz	@@6		;check next item in the list.
	cmp	b[esi],9
	jz	@@6		;check next item in the list.
	inc	esi
	jmp	@@Ignore
	;
@@Start:	xor	edx,edx		;reset acumulated value.
@@11:	movzx	eax,b[esi]		;fetch a digit.
	or	al,al
	jz	@@10		;finished geting value so store it.
	cmp	al,' '
	jz	@@10		;finished geting value so store it.
	call	ASCII2Bin
	mov	ErrorNumber,12	;default to bad definition.
	jc	@@90
	cmp	al,'H'		;end of the number?
	jz	@@30
	shl	edx,4		;update acumulated value.
	add	edx,eax		;/
	inc	esi
	jmp	@@11		;keep reading till we run out.
@@30:	inc	esi		;skip 'H'
@@10:	mov	ebx,SegCurrent
	mov	eax,edx
	and	edx,0fffffff0h	;segment has to be paragraph aligned.
	mov	[ebx+0],edx		;store start address.
	and	eax,0fh
	add	d[ebx+4],eax		;update limit base value.
	jmp	@@6		;do next item along.
	;
@@Length:	xor	edx,edx		;reset accumulated value.
@@12:	movzx	eax,b[esi]		;fetch a digit.
	or	al,al
	jz	@@13		;finished getting value so store it.
	cmp	al,' '
	jz	@@13		;finished getting value so store it.
	call	ASCII2Bin
	mov	ErrorNumber,12	;default to bad definition.
	jc	@@90
	cmp	al,'H'		;end of the number?
	jz	@@31
	shl	edx,4		;update acumulated value.
	add	edx,eax		;/
	inc	esi
	jmp	@@12		;keep reading till we run out.
@@31:	inc	esi		;skip 'H'
@@13:	mov	ebx,SegCurrent
	add	d[ebx+4],edx		;store length.
	jmp	@@6		;do next item along.
	;
@@Class:	;Search class lists for this string.
	;
	mov	ErrorNumber,14	;default to un-known class.
	mov	ebp,offset SegClassList	;list of lists.
	mov	edx,esi		;store source position.
@@14:	cmp	ds:d[ebp],-1		;end of the list?
	jz	@@90
	mov	ebx,ds:[ebp]		;get table pointer.
	mov	esi,edx
	mov	d[@@WildStart1],0
	;
@@15:	cmp	b[ebx],'*'		;match anything after this point?
	jnz	@@25
	mov	d[@@WildStart1],ebx	;store sub-string start.
	mov	d[@@WildStart2],esi
	cmp	b[ebx+1],' '		;last thing in string?
	jz	@@25
	cmp	b[ebx+1],0
	jz	@@25
	inc	ebx		;move to next char.
	mov	d[@@WildStart1],ebx	;store sub-string start.
	jmp	@@15
	;
@@25:	cmp	b[ebx],' '		;end of sub string?
	jz	@@26
	cmp	b[ebx],0
	jnz	@@27
@@26:	mov	d[@@WildStart1],0
	;
@@27:	cmp	b[esi],' '		;white space?
	jz	@@19
	cmp	b[esi],9		;white space?
	jz	@@19
	cmp	b[esi],0		;end of the line?
	jnz	@@20
	;
@@19:	cmp	b[ebx],' '		;if list entry is finished then we have a match.
	jz	@@21
	cmp	b[ebx],0
	jz	@@21
	cmp	b[ebx],'*'
	jz	@@21
	jmp	@@17		;try next class name.
	;
@@20:	mov	al,[esi]
	call	UpperChar		;no case sensitivity.
	xchg	ah,al
	mov	al,[ebx]
	call	UpperChar
	cmp	al,ah		;any match?
	jz	@@16
	cmp	d[@@WildStart1],0	;fail it.
	jz	@@17
	inc	d[@@WildStart2]	;move along in source string.
	mov	ebx,d[@@WildStart1]	;point to sub string again.
	mov	esi,d[@@WildStart2]
	jmp	@@15		;look again.
	;
@@17:	cmp	b[ebx],0		;end of this class list?
	jz	@@18
	cmp	b[ebx],' '		;seperator?
	jz	@@22		;keep going till the end or a space.
	inc	ebx
	jmp	@@17
@@22:	mov	esi,edx
	inc	ebx		;skip the space.
	mov	d[@@WildStart1],0
	jmp	@@15		;try new name.
	;
@@16:	inc	esi
	inc	ebx
	jmp	@@15		;keep comparing.
	;
@@18:	add	ebp,4		;next class list entry.
	jmp	@@14
	;
@@21:	sub	ebp,offset SegClassList	;get class *4
	shr	ebp,2		;real class.
	push	ebx
	mov	ebx,SegCurrent
	mov	w[ebx+8],bp		;store class.
	pop	ebx
	cmp	bp,4		;flat?
	jnz	@@6
	mov	OptionTable+'N','+'	;signal flat mode.
	mov	OptionTable+'G',0
	mov	OptionTable+'3','+'
	push	esi
	mov	esi,offset NewHeader
	and	[esi].NewHeaderStruc.NewFlags,65535-(1+16384)	;32 bit code.
	pop	esi
	jmp	@@6
	;
@@NextSeg:	mov	ebx,SegCurrent
	cmp	w[ebx+8],2
	jnz	@@NoRound
	add	d[ebx+4],15
	and	d[ebx+4],0fffffff0h
@@NoRound:	;
	add	SegCurrent,16	;next segment storage slot.
	mov	eax,SegCurrent
	sub	eax,SegmentList
	shr	eax,4
	cmp	eax,8192		;check for to many segments.
	mov	ErrorNumber,17
	jnc	@@9
	;
	mov	bx,MapHandle
	mov	edi,offset LineBuffer
	call	ReadLine
	mov	ErrorNumber,8	;default to general IO error.
	jc	@@9
	or	ax,ax		;EOF?
	jnz	@@DoSegs
	mov	ErrorNumber,18
	cmp	cx,1024
	jnc	@@90
	or	cx,cx		;Blank line?
	jnz	@@LookSeg		;Fetch next segment value.
	;
@@DoSegs:	;We've got all the segments so its time to make use of them.
	;
	cmp	OptionTable+'N',0	;Flat mode?
	jz	@@NoFlat
	;
	mov	eax,SegCurrent
	sub	eax,SegmentList
	shr	eax,4		;/16
	cmp	eax,1
	mov	ErrorNumber,24	;only one segment allowed.
	jnz	@@9
	mov	esi,SegmentList
	mov	eax,0[esi]		;get base.
	mov	ebx,4[esi]		;get limit.
	mov	d[esi+4],-1		;set limit.
	add	esi,16		;next entry.
	mov	0[esi],eax		;set base.
	mov	d[esi+4],ebx		;set limit.
	mov	w[esi+8],0		;set class to code.
	add	esi,16
	mov	SegCurrent,esi	;pretend normal now.
	sub	esi,16
	;
	cmp	OptionTable+'Z',0
	jz	@@NoFlat
	;
	mov	edi,offset NewHeader
	mov	eax,[edi].NewHeaderStruc.NewAutoStack
	or	eax,eax
	jnz	@@GotESPSize
	mov	eax,1024
@@GotESPSize:	add	eax,4[esi]
	add	eax,3
	and	eax,0FFFFFFFCh
	mov	4[esi],eax
	mov	[edi].NewHeaderStruc.NewEntryESP,eax
	mov	[edi].NewHeaderStruc.NewEntrySS,0
	;
@@NoFlat:	mov	eax,SegCurrent
	sub	eax,SegmentList
	shr	eax,4		;/16
	mov	SegmentTotal,eax
	or	eax,eax		;any segments found?
	mov	ErrorNumber,13
	jz	@@9
	;
	mov	bx,MapHandle
	call	CloseFile		;close the file again.
	mov	MapHandle,0
	;
	;Work out where the very end of the program will be. This is the size of memory
	;we will need to allocate for it.
	;
	mov	ebx,SegmentList	;make segment details addressable.
	mov	ecx,SegmentTotal	;number of segments.
	xor	edx,edx		;reset comparison value.
@@23:	cmp	d[ebx+4],-1
	jz	@@24
	mov	eax,[ebx+0]		;get segment base.
	add	eax,[ebx+4]		;add in length.
	cmp	eax,edx		;biggest yet?
	jc	@@24
	mov	edx,eax		;store new limit.
@@24:	add	ebx,16
	loop	@@23		;do all of them.
	mov	esi,offset NewHeader
	mov	[esi].NewHeaderStruc.NewAlloc,edx	;store it in the new header.
	;
	;Go through segments altering limits so that they extend to the
	;end of the program.
	;
	cmp	OptionTable+'G',0	;group addressing?
	jz	@@NoGroup
	mov	esi,offset NewHeader
	mov	eax,[esi].NewHeaderStruc.NewAlloc	;get program limit.
	mov	esi,SegmentList	;the segment details.
	mov	ecx,SegmentTotal	;number of segments to process.
@@G0:	cmp	w[esi+8],0		;code segment?
	jz	@@G1		;leave code segs as they are.
	mov	ebx,eax		;copy program limit.
	sub	ebx,0[esi]		;minus segment base.
	mov	4[esi],ebx		;set new limit.
@@G1:	add	esi,16		;next segment.
	loop	@@G0		;do all of them.
	;
@@NoGroup:	;Now make sure no 2 segments have the same base value.
	;
	cmp	OptionTable+'N',0
	jnz	@@NoBaseChk
	mov	esi,SegmentList	;make segment details addressable.
	mov	ecx,SegmentTotal	;number of segments.
@@SameBase0:	pushm	ecx,esi
	mov	eax,[esi]		;get base.
	mov	ebp,esi
	add	esi,16
	dec	ecx
@@SameBase1:	or	ecx,ecx
	jz	@@SameBase2
	js	@@SameBase2
	cmp	eax,[esi]		;same base?
	jz	@@SameBase3
@@SameBase4:	add	esi,16
	dec	ecx
	jmp	@@SameBase1
@@SameBase2:	popm	ecx,esi
	add	esi,16
	dec	ecx
	or	ecx,ecx
	jz	@@NoBaseChk
	js	@@NoBaseChk
	jmp	@@SameBase0
	;
@@SameBase3:	pushm	eax,ecx,esi,ebp

	if	0
	mov	eax,esi		;get current pointer.
	sub	eax,SegmentList
	xor	edx,edx
	mov	ebx,16
	div	ebx
	mov	ecx,4
	mov	edi,offset SegSamet1
	call	Bin2Hex
	popm	eax,ecx,esi,ebp
	popm	ebx,edi
	pushm	ebx,edi
	pushm	eax,ecx,esi,ebp
	mov	eax,edi
	sub	eax,SegmentList
	xor	edx,edx
	mov	ebx,16
	div	ebx
	mov	ecx,4
	mov	edi,offset SegSamet0
	call	Bin2Hex
	mov	edx,offset SegSamet
	call	StringPrint
	endif

	mov	edx,offset SegSamet
	call	StringPrint
	mov	SegSamet,0

	popm	eax,ecx,esi,ebp
	;
	;Upgrade first segment to largest length of the two.
	;
	push	eax
	mov	eax,ds:[ebp+4]
	mov	edx,[esi+4]
	cmp	eax,edx
	jnc	@@SameBase5
	mov	eax,edx
@@SameBase5:	mov	ds:[ebp+4],eax
	mov	[esi+4],eax
	pop	eax
	jmp	@@SameBase4
@@NoBaseChk:	;
	mov	ErrorNumber,0
	xor	ax,ax
	ret
	;
@@90:	mov	edx,offset CarriageReturn
	call	StringPrint
	mov	edx,offset LineBuffer
	call	StringPrint		;print the offending line.
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
@@9:	mov	ax,-1
	or	ax,ax
	ret
_BSS segment
@@WildStart1	dd ?
@@WildStart2	dd ?
_BSS ends
ReadMapFile	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
ASCII2Bin	proc	near
;
;Convert hex digit in AL into binary value.
;
	call	UpperChar
	cmp	al,'H'
	jz	@@8
	cmp	al,'0'
	jc	@@9
	cmp	al,'A'
	jc	@@Dec
	cmp	al,'F'+1
	jnc	@@9
	sub	al,'A'-10
	jmp	@@8
@@Dec:	cmp	al,'9'+1
	jnc	@@9
	sub	al,'0'
@@8:	clc
	ret
	;
@@9:	stc
	ret
ASCII2Bin	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
ReadConfig	proc	near
;
;Read configuration file.
;
	call	InitFileBuffer

	mov	edx,offset ConfigName
	cmp	OptionTable+'C',0	;new config file name?
	jz	@@NoOveride
	mov	edx,d[OptionTable+128+('C'*4)]
	or	edx,edx
	jz	@@NotCurrent
@@NoOveride:
	call	OpenFile
	jnc	@@ConfigOK
	cmp	OptionTable+'C',0	;new config file name?
	jnz	@@8		;don't look for default.
@@NotCurrent:	;
	mov	ebx, psp
if 0
	mov	es,w[ebx+2ch]
	xor	esi,esi
else
	mov bx,[ebx+2Ch]
	mov ax,6
	int 31h
	push cx
	push dx
	pop esi
endif
	xor	ebp,ebp
@@c1:
	mov	al,[esi]		;Get a byte.
	inc	esi		;/
	or	al,al		;End of a string?
	jnz	@@c1		;keep looking.
	mov	al,[esi]		;Double zero?
	or	al,al		;/
	jnz	@@c1		;keep looking.
	add	esi,3		;Skip last 0 and word count.
	mov	edi,offset ConfigPath
@@c2:
	movsb
	cmp	b[esi-1],'\'
	jnz	@@c3
	mov	ebp,edi
@@c3:
	cmp	b[esi-1],0		;got to the end yet?
	jnz	@@c2
	or	ebp,ebp
	jnz	@@c99
	mov	ebp,edi
@@c99:
	dec	ebp
	mov	edi,ebp
	mov	b[edi],'\'
	inc	edi
	mov	esi,offset ConfigName
@@c4:
	movsb
	cmp	b[esi-1],0
	jnz	@@c4
	;
	mov	edx,offset ConfigPath	;use new name.
	call	OpenFile
	jc	@@8		;don't have to have a config.
	;
@@ConfigOK:	mov	ConfigHandle,ax
	;
@@Read:	mov	bx,ConfigHandle
	mov	edi,offset LineBuffer
	call	ReadLine		;read a line.
	mov	ErrorNumber,8	;default to io error.
	jc	@@9
	or	ax,ax		;end of the file?
	jnz	@@7
	mov	ErrorNumber,18
	cmp	cx,1024
	jnc	@@90
	;
@@Scan:	;Check this line for variables.
	;
	mov	edx,offset LineBuffer
	mov	ebp,offset VariableList
	mov	edi,edx
	cmp	b[edi],';'
	jz	@@Read		;comment so ignore it.
	cmp	b[edi],0		;blank line?
	jz	@@Read
	;
@@0:	mov	ErrorNumber,15
	cmp	ds:d[ebp],-1		;end of the list?
	jz	@@90
	mov	esi,ds:[ebp]		;get text pointer.
	mov	edi,edx		;source data.
	;
@@1:	cmp	b[edi],'='		;end of the string?
	jnz	@@3
	cmp	b[esi],0		;end of our version as well?
	jz	@@4
	;
@@3:	mov	ErrorNumber,15
	cmp	b[edi],0		;end of the line?
	jz	@@90
	;
	cmp	b[esi],0		;end of the text?
	jz	@@5
	;
	mov	al,[esi]
	call	UpperChar
	xchg	ah,al
	mov	al,[edi]
	call	UpperChar
	cmp	al,ah		;match?
	jz	@@2
	;
@@5:	add	ebp,16		;next variable.
	jmp	@@0
	;
@@2:	inc	esi
	inc	edi
	jmp	@@1
	;
@@4:	inc	edi
	call	ds:d[ebp+4]		;call the handler code.
	jz	@@Read
	mov	ErrorNumber,16	;invalid setting.
	jmp	@@90
	;
@@7:	mov	bx,ConfigHandle	;close the file again.
	call	CloseFile
	mov	ConfigHandle,0
	jmp	@@10
	;
@@8:	mov	edx,offset InternalConfig
	call	StringPrint
	;
@@10:	mov	ErrorNumber,0
	xor	ax,ax
	ret
	;
@@90:	mov	edx,offset CarriageReturn
	call	StringPrint
	mov	edx,offset LineBuffer
	call	StringPrint
	mov	edx,offset CarriageReturn
	call	StringPrint
	;
@@9:	mov	ax,-1
	or	ax,ax
	ret
ReadConfig	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=-=
WhiteSpaceString proc near
;
;Retrieve a string and convert multiple white space into single spaces. Last white space is line
;terminator.
;
	;
	mov	esi,edi
	mov	d[@@Source],esi
	mov	edi,ds:[ebp+8]	;get target address.
	cmp	b[edi-1],-1		;first time?
	mov	b[edi-1],0
	jz	@@AtStart
@@30:	mov	al,[edi]
	or	al,al
	jz	@@AtStart
	inc	edi
	jmp	@@30
@@AtStart:	;
	mov	b[edi],' '
	inc	edi
	;
	xor	ah,ah		;clear spacing flag.
@@0:	lodsb
	cmp	al,'\'		;line continuation?
	jnz	@@5
	;
	;Read a new line.
	;
	pushm	edi,ebp
	mov	bx,ConfigHandle
	mov	edi,d[@@Source]
	call	ReadLine		;read a line.
	popm	edi,ebp
	jc	@@9
	or	ax,ax		;end of the file?
	jnz	@@7
	cmp	cx,1024
	jnc	@@9
	;
	mov	esi,d[@@Source]
@@6:	lodsb
	cmp	al,' '
	jz	@@6
	cmp	al,9
	jz	@@6
	dec	esi
	mov	al,' '
	stosb
	xor	ah,ah
	jmp	@@0		;start reading again.
	;
@@5:	stosb
	cmp	b[esi-1],0		;end of the string?
	jz	@@1
	cmp	b[esi-1],' '		;need multiple space check?
	jz	@@2
	cmp	b[esi-1],9
	jz	@@2
	xor	ah,ah		;clear spacing flag.
	jmp	@@0
	;
@@2:	or	ah,ah		;this part 2?
	jnz	@@3
	mov	b[edi-1],' '		;make sure its a space.
	mov	ah,1		;signal spacing start.
	jmp	@@0
	;
@@3:	dec	edi		;move back to last one.
	jmp	@@0
	;
@@1:	dec	edi		;back to terminator.
	cmp	edi,ds:[ebp+8]	;back at the start yet?
	jz	@@4
	cmp	b[edi-1],' '		;trailing space?
	jnz	@@4
	dec	edi
@@4:	;
@@7:	mov	b[edi],0		;terminate the line.
	;
	xor	ax,ax
	ret
	;
@@9:	mov	ax,-1
	or	ax,ax
	ret
_BSS segment
@@Source	dd ?
_BSS ends
WhiteSpaceString endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
SegFormatCode	proc near
;
;Convert input string to segment format table layout.
;
	mov	edx,edi		;get source address.
	mov	edi,ds:[ebp+8]	;get target address.
	;
@@5:	mov	esi,edx
	cmp	b[esi],0		;end of input?
	jz	@@6
	;
	mov	ebp,offset SegFormatTexts	;list of strings to match against.
@@0:	mov	ErrorNumber,16	;syntax error.
	cmp	ds:d[ebp],-1		;end of the list?
	jz	@@9		;not a valid statement then.
	mov	ebx,ds:[ebp]		;get variable text.
	mov	esi,edx		;get source string.
@@1:	cmp	b[esi],0		;line end?
	jz	@@7
	cmp	b[esi],' '		;seperator?
	jnz	@@3
@@7:	cmp	b[ebx],0		;end of variable text as well?
	jz	@@4		;looks like we've got a match.
@@3:	cmp	b[esi],' '		;seperator?
	jz	@@2		;next variable text.
	cmp	b[ebx],0		;end of variable text?
	jz	@@2		;next variable text.
	mov	al,[esi]
	call	UpperChar
	xchg	ah,al
	mov	al,[ebx]
	call	UpperChar
	cmp	al,ah		;do they match?
	jnz	@@2
	inc	esi
	inc	ebx
	jmp	@@1		;keep looking till something happens.
	;
@@2:	add	ebp,4		;next string.
	jmp	@@0
	;
@@4:	sub	ebp,offset SegFormatTexts	;get index *4
	shr	ebp,2		;get operation type.
	mov	eax,ebp
	mov	b[edi],al		;put it in the destination.
	inc	edi
	add	ebp,offset SegFormatSlots	;point at slot table.
	mov	ds:b[ebp],1		;mark this operation type as present.
	;
	mov	edx,esi
	cmp	b[esi],0		;did we finish on 0?
	jz	@@5		;catch it up there.
	inc	edx		;skip space seperator.
	jmp	@@5		;scan the rest of the line.
	;
@@6:	mov	b[edi],-1		;terminate the list.
	mov	esi,offset SegFormatSlots+1	;point at slot table. (ignore NULL entry)
	mov	ecx,3		;only 3 types at the moment.
	mov	ErrorNumber,16	;default to syntax error.
@@8:	lodsb
	or	al,al		;this entry used?
	jz	@@9		;ALL positions have to be defined.
	loop	@@8		;do them all.
	;
	xor	ax,ax
	ret
	;
@@9:	mov	ax,-1
	or	ax,ax
	ret
SegFormatCode	endp



;-------------------------------------------------------------------------------
Bord	proc	near
	pushm	ax,dx
	mov	ah,al
	mov	dx,3dah
	in	al,dx
	mov	dl,0c0h
	mov	al,11h
	out	dx,al
	mov	al,ah
	out	dx,al
	mov	al,20h
	out	dx,al
	popm	ax,dx
	ret
Bord	endp


;==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-==-=-=-=-=-=-=-=-=-=
UpperChar	proc	near
;
;Convert a character into upper case.
;
;On Entry:-
;AL - Character to convert.
;
;On Exit:-
;AL - New character code.
;
	cmp	al,61h		; 'a'
	jb	@@0
	cmp	al,7Ah		; 'z'
	ja	@@0
	and	al,5Fh
@@0:	ret
UpperChar	endp

;--- get linear memory
;--- in: size in ECX
;--- out: linear address in ESI
;--- must preserve ECX

GetMemLinear32 PROC uses ecx bx di
	push ecx
	pop cx
	pop bx
	mov ax, 501h
	int 31h
	jc fail
	push bx
	push cx
	pop esi
fail:
	ret
GetMemLinear32 ENDP

;--- free linear memory block
;--- in: address in ESI

RelMemLinear32 PROC uses di
	push esi
	pop di
	pop si
	mov ax, 502h
	int 31h
	ret
RelMemLinear32 ENDP

;--- resize linear memory block
;--- in: address in ESI, new size in ECX

ResMemLinear32 PROC uses ecx di bx
	push esi
	pop di
	pop si
	push ecx
	pop cx
	pop bx
	mov ax, 503h	; new size in BX:CX, handle (=address) in SI:DI
	int 31h
	push si
	push di
	pop esi
	ret
ResMemLinear32 ENDP

	include command.inc
	include files.inc
	include print.inc

ifndef CWAPP
 if @Model ne 7
	include initpm.inc
 endif
endif

	end start
