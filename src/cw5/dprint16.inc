
;--- simple 16-bit printf() implementation;
;--- used by debug displays only.
;--- assume ds:nothing, ss:nothing

;--- ltob(long n[eax], char *outb[bx], int base[cx]);
;--- convert long to string;
;--- outb is assumed to be stack-located.

@dprintf macro fmt,a1,a2,a3,a4
  for x,<a4,a3,a2,a1>
   ifnb <x>
	push x
   endif
  endm
	call dprintf16
	db fmt,0
endm

ltob16 PROC uses edi edx

	movzx edi, cx
	mov ch,0
	cmp di,-10
	jne @F
	mov di,10
	and eax,eax
	jns @F
	neg eax
	mov ch,'-'
@@:
	add bx,10
	mov BYTE PTR ss:[bx],0
	dec bx
@@nextdigit:
	xor edx, edx
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @F
	add dl,7+20h
@@:
	mov ss:[bx],dl
	dec bx
	and eax, eax
	jne @@nextdigit
	cmp ch,0
	je @F
	mov ss:[bx],ch
	dec bx
@@:
	inc bx
	mov ax,bx
	ret

ltob16 ENDP

dprintf16 PROC

local size_:word
local flag:byte
local longarg:byte
local fill:byte
local szTmp[12]:byte

fmt equ <[bp+2]>

	pusha
	push eax
	lea di,[bp+2+2]
@@L335:
	mov si,fmt
nextchar:
	lodsb cs:[si]
	or al,al
	je done
	cmp al,'%'
	je formatitem
	call print_char
	jmp nextchar
done:
	sub di,2
	mov ss:[di],si   ;store new IP
	mov ax,[bp]      ;get old value of BP
	mov bp,sp
	mov [bp+4+2*2],ax;now BP will be restored by popa
	mov [bp+4+8*2],di;finally store new SP
	pop eax
	popa
	pop sp
	retn

formatitem:
	push @@L335
	xor dx,dx
	mov [longarg],dl
	mov bl,1
	mov cl,' '
	cmp BYTE PTR cs:[si],'-'
	jne @F
	dec bx
	inc si
@@:
	mov [flag],bl
	cmp BYTE PTR cs:[si],'0'
	jne @F
	mov cl,'0'
	inc si
@@:
	mov [fill],cl
	mov bx,dx

	.while byte ptr cs:[si] >= '0' && byte ptr cs:[si] <= '9'
		lodsb cs:[si]
		sub al,'0'
		cbw
		imul cx,bx,10		;cx = bx * 10
		add ax,cx
		mov bx,ax
	.endw

	mov [size_],bx
	cmp BYTE PTR cs:[si],'l'
	jne @F
	mov [longarg],1
	inc si
@@:
	lodsb cs:[si]
	mov fmt,si
	cmp al,'x'
	je handle_x
	cmp al,'X'
	je handle_x
	cmp al,'c'
	je handle_c
	cmp al,'d'
	je handle_d
	cmp al,'i'
	je handle_i
	cmp al,'s'
	je handle_s
	cmp al,'u'
	je handle_u
	cmp al,0
	jnz @@L359
	pop ax
	jmp done
handle_c:
	mov ax,ss:[di]
	add di,2
@@L359:
	call print_char
	retn

handle_x:
	mov cx,16
	jmp @@lprt262
handle_d:
handle_i:
	mov cx,-10
	jmp @@lprt262
handle_u:
	mov cx,10
@@lprt262:
	mov ax,ss:[di]
	add di,2
	sub dx,dx
	cmp cx,0		;signed or unsigned?
	jge @F
	cwd
@@:
	cmp [longarg],0
	je @F
	mov dx,ss:[di]
	add di,2
@@:
	push dx
	push ax
	pop eax
	lea bx,[szTmp]
	call ltob16
	mov si,ax
	push ds
	push ss
	pop ds
	call print_string
	pop ds
	retn

handle_s:
	mov si,ss:[di]
	add di,2
	cmp [longarg],0
	je print_string
	push ds
	mov ds,ss:[di]
	add di,2
	call print_string
	pop ds
	retn

print_string:	;display string at ds:si
	mov ax,si
	mov bx,size_
	.while byte ptr [si]
		inc si
	.endw
	sub si,ax
	xchg ax,si
	sub bx,ax
	.if flag == 1
		.while sword ptr bx > 0
			mov al,[fill]
			call print_char
			dec bx
		.endw
	.endif

	.while byte ptr [si]
		lodsb
		call print_char
	.endw

	.while sword ptr bx > 0
		mov al,[fill]
		call print_char
		dec bx
	.endw
	retn
print_char:
	cmp al,10
	jnz @F
	mov al,13
	call @F
	mov al,10
@@:
	push bx
	xor bx,bx
	mov ah,0Eh
	int 10h
	pop bx
	retn

fmt equ <>

dprintf16 ENDP

