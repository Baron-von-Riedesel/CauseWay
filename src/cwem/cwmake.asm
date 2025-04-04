
;--- reads cwem.exe
;--- writes cwem.cw
;--- expects cwem.exe as CauseWay app ( with DOS extender stub ).

	.model small
	.stack 400h
	.386

	include general.inc
	include ..\strucs.inc

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

	.data
;
PSPSegment	dw ?
;
InFileName	db 'cwem.exe',0
InFileHandle	dw ?
OutFileName	db 'cwem.cw',0
OutFileHandle	dw ?
	even

exehdr MZHDR <>
	even
;
ExeSize1	dw ?,?	;Header size
ExeSize2	dw ?,?	;Real file size.
;
NewHeader	NewHeaderStruc <>	;make space for a header.
;
DataSegBase	dd 0
DataSegLimit	dd 0fffffh+(1 shl 20)+(1 shl 21)
CodeSegBase	dd 0
CodeSegLimit	dd ?
;
RelocSegment	dw 0
EXESegment	dw 0
Real3POffset	dd 0
;
	.code

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Start	proc	near
	mov	ax,DGROUP
	mov	ds,ax
	mov	PSPSegment,es
	;
	mov	cx,sp
	shr	cx,4
	inc	cx
	mov	ax,es
	mov	bx,ss		;Get program end segment.
	add	bx,cx
	sub	bx,ax		;Size program.
	mov	ah,4ah
	int	21h		;Re-size memory block.
	;
	call	ProcessFile
	;
	mov	ax,4c00h
	int	21h
Start	endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
ProcessFile	proc	near
	mov	dx,offset InFileName
	mov	ax,3d00h
	int	21h
	jc	@@9
	mov	InFileHandle,ax
	;
	mov	dx,offset OutFileName
	mov	ax,3c00h
	mov	cx,0
	int	21h
	jc	@@9
	mov	OutFileHandle,ax
	;
	;Process .EXE relocation table and produce new format header.
	;
	mov	dx,offset exehdr
	mov	cx,sizeof exehdr
	mov	bx,InFileHandle
	mov	ah,3fh
	int	21h		;read the .EXE header.
	jc	@@9
	cmp	ax,sizeof exehdr
	jnz	@@9
	cmp	[exehdr.Signature],'ZM'	;Correct ID?
	jnz	@@9
	;
	;Get header size in bytes.
	;
	mov	bx,[exehdr.HeaderSize]	;Work out header size.
	xor	cx,cx		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	add	bx,bx		;/
	adc	cx,0		;/
	mov	w[ExeSize1+0],bx	;/
	mov	w[ExeSize1+2],cx	;/
	;
	;Get exe image size in bytes.
	;
	mov	ax,w[exehdr._Length+2]	;get length in 512 byte blocks
	dec	ax		;lose 1 cos its rounded up
	add	ax,ax		;mult by 2
	mov	dh,0
	mov	dl,ah
	mov	ah,al
	mov	al,dh		;mult by 256=*512
	add	ax,w[exehdr._Length]	;add length mod 512
	adc	dx,0		;add any carry to dx
	sub	ax,bx		;remove header total size.
	sbb	dx,cx		;/
	mov	w[ExeSize2+0],ax	;/
	mov	w[ExeSize2+2],dx	;/
	;
	shl	edx,16
	mov	dx,ax
	mov	si,offset NewHeader
	mov	NewHeader.NewLength[si],edx	;store EXE image length in the header.
	;
	push	edx
	cmp	edx,100000h
	jc	@@0
	add	edx,4095
	shr	edx,12		;lose bottom bits.
	or	edx,1 shl 20		;Set our version of the G bit.
@@0:	mov	CodeSegLimit,edx
	pop	edx
	;
	add	edx,1024
	mov	NewHeader.NewAlloc[si],edx
	and	edx,0ffffffffh-3
	mov	NewHeader.NewEntryESP[si],edx
	mov	NewHeader.NewSegments[si],2
	movzx	eax,exehdr.RelocNum
	mov	NewHeader.NewRelocs[si],eax
	movzx	eax,exehdr.EntryIP
	mov	NewHeader.NewEntryEIP[si],eax	;setup entry offset.
	mov	NewHeader.NewEntryCS[si],1
	mov	NewHeader.NewEntrySS[si],0
	;
	;Read relocation table.
	;
	mov	bx,exehdr.RelocNum	;get number of relocation items.
	or	bx,bx
	jz	@@NoRelocMem
	shl	bx,2
	shr	bx,4
	inc	bx
	mov	ah,48h
	int	21h
	jc	@@9
	mov	RelocSegment,ax	;stow the memory address.
	;
	mov	dx,exehdr.RelocFirst
	mov	cx,0
	mov	bx,InFileHandle
	mov	ax,4200h
	int	21h		;move to relocation table.
	;
	mov	cx,exehdr.RelocNum
	shl	cx,2
	mov	bx,InFileHandle
	push	ds
	mov	ds,RelocSegment
	mov	dx,0
	mov	ax,3f00h
	int	21h
	pop	ds
	jc	@@9
	;
@@NoRelocMem:	;Get some memory for the exe image.
	;
	mov	ebx,d[ExeSize2]	;get exe image size.
	shr	ebx,4
	inc	bx
	mov	ah,48h
	int	21h
	jc	@@9
	mov	EXESegment,ax
	;
	mov	dx,w[ExeSize1]	;get image file offset.
	mov	cx,w[ExeSize1+2]
	mov	bx,InFileHandle
	mov	ax,4200h
	int	21h		;move to start of exe image.
	jc	@@9
	mov	bx,InFileHandle
	mov	cx,w[ExeSize2]
	mov	dx,w[ExeSize2+2]
	push	ds
	mov	ds,EXESegment
	mov	dx,0
	mov	ah,3fh
	int	21h
	pop	ds
	jc	@@9
	;
	mov	bx,OutFileHandle
	mov	cx,0
	mov	dx,0
	mov	ax,4201h
	int	21h
	shl	edx,16
	mov	dx,ax
	mov	Real3POffset,edx
	;
	;Update exe image with real segment numbers.
	;
	mov	cx,exehdr.RelocNum	;number of entries.
	jcxz	@@NoReloc
	mov	es,RelocSegment	;the relocations.
	mov	si,0
@@3:	mov	di,es:[si]
	mov	ax,es:[si+2]
	add	ax,EXESegment
	mov	fs,ax
	mov	fs:w[di],0
	add	si,4		;next relocation entry.
	loop	@@3
	;
	;Convert relocation table to linear offsets.
	;
	mov	cx,exehdr.RelocNum	;number of entries.
	mov	es,RelocSegment	;list of relocations.
	mov	si,0
@@1:	movzx	eax,es:w[si+2]	;get segment offset.
	shl	eax,4		;make it linear.
	movzx	ebx,es:w[si+0]	;get offset.
	add	eax,ebx		;add in offset.
	mov	es:d[si],eax		;store linear offset.
	add	si,4
	loop	@@1
@@NoReloc:	;
	;Write main header.
	;
	mov	dx,offset NewHeader	;write the header to make space.
	mov	bx,OutFileHandle
	mov	cx,size NewHeaderStruc
	mov	ah,40h
	int	21h
	;
	;Write segment definitions.
	;
	mov	dx,offset DataSegBase
	mov	cx,8+8
	mov	bx,OutFileHandle
	mov	ah,40h
	int	21h
	;
	;Write relocation table.
	;
	mov	cx,exehdr.RelocNum	;number of entries.
	shl	cx,2
	mov	bx,OutFileHandle
	push	ds
	mov	ds,RelocSegment	;where they are.
	mov	dx,0
	mov	ah,40h
	int	21h
	pop	ds
	jc	@@9
	;
	;Write exe image.
	;
	mov	cx,w[ExeSize2]
	mov	dx,w[ExeSize2+2]
	mov	bx,OutFileHandle
	push	ds
	mov	ds,ExeSegment
	mov	dx,0
	mov	ah,40h
	int	21h
	pop	ds
	jc	@@9
	;
	;Calculate file size.
	;
	mov	bx,OutFileHandle
	mov	cx,0
	mov	dx,0
	mov	ax,4201h
	int	21h
	shl	edx,16
	mov	dx,ax
	sub	edx,Real3POffset
	mov	si,offset NewHeader
	mov	NewHeader.NewSize[si],edx
	;
	;Now go back and write the real header.
	;
	mov	edx,Real3POffset
	mov	cx,dx
	shr	edx,16
	xchg	cx,dx
	mov	bx,OutFileHandle
	mov	ax,4200h
	int	21h
	mov	dx,offset NewHeader	;write the header to make space.
	mov	bx,OutFileHandle
	mov	cx,size NewHeaderStruc
	mov	ah,40h
	int	21h
	;
	mov	bx,InFileHandle
	mov	ah,3eh
	int	21h
	mov	bx,OutFileHandle
	mov	ah,3eh
	int	21h
	;
@@9:	ret
ProcessFile	endp

	end	Start
