
;--- 16-bit sample, memory model tiny.
;--- assemble: ml -c tiny16.asm
;--- link: wl32 tiny16

	.286
	.model tiny
	.dosseg
	.stack

	.data

text1 db "hello, world!",13,10,'$'

	.code

main proc c

	mov dx,offset text1
	mov ah,9
	int 21h
	ret
main endp

start:
;--- model tiny needs an alias, since DGROUP is a code selector
	mov bx,cs
	mov ax,000ah
	int 31h
	mov ds,ax
	mov es,ax
	lsl dx,ax
	inc dx
	mov ss,ax
	mov sp,dx
;--- now tiny is setup correctly, cs=ss=ds=es
	call main
	mov ax,4c00h
	int 21h

	END start

