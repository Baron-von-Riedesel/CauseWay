
;--- 32-bit sample, memory model tiny. To create:
;---   ml -c tiny32.asm
;---   wl32 tiny32
;--- or, alternativly:
;---   ml -c -D?FLAT tiny32.asm
;---   wl32 /f tiny32

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
ifndef ?FLAT
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
else
	mov eax,ss
	mov ds,eax
	mov es,eax
endif
	call main
	mov ax,4c00h
	int 21h

	END start

