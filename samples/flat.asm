
;--- 32-bit sample, memory model flat.
;--- assemble: ml -c flat.asm
;--- link: WL32 flat
;--- or use OW's wlink: wlink system causeway f flat.obj

;--- to debug with OW's WD:
;--- assemble: jwasm -Zi -c flat.asm
;--- link      wlink DEBUG c OP cvp,symfile SYSTEM causeway FILE flat.obj OP m
;--- debug:    wd /tr=cw flat.exe

	.386
	.model flat
	.stack

	.data

text1 db "hello, world",13,10,'$'

	.code

main proc c

	mov edx,offset text1
	mov ah,9
	int 21h
	ret
main endp

start:
	mov eax,ss
	mov ds,eax
	mov es,eax
	call main
	mov ax,4c00h
	int 21h

	END start

