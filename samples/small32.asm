
;--- 32-bit sample, memory model small. To create:
;---   ml -c small32.asm
;---   wl32 small32
;--- or, alternatively:
;---   ml -c -D?FLAT small32.asm
;---   wl32 /f small32

	.386
	.model small
	.dosseg
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

;--- entry in 3P format:
;--- SS:ESP = segment stack
;--- DS,ES = PSP
;--- small model assumes: DS=ES=SS=DGROUP

start:
	mov eax,@data
	mov ds,eax
	mov es,eax
ifndef ?FLAT
	lsl edx,eax
	inc edx
	mov ss,eax
	mov esp,edx
endif
	call main
	mov ax,4c00h
	int 21h

	END start

