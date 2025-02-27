
;--- 32-bit sample, memory model tiny.
;--- assemble: ml -c tiny32.asm
;--- link: WL32 tiny32

	.386
	.model tiny
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

start:
;--- model tiny needs an alias, since DGROUP is code
	mov ebx,cs
	mov ax,000ah
	int 31h
	mov ds,eax
	mov es,eax
	lsl edx,eax
	inc edx
	mov ss,eax
	mov esp,edx
	call main
	mov ax,4c00h
	int 21h

	END start

