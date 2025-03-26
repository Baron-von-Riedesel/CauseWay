
;--- simple 32-bit printf() implementation;
;--- used by debug displays only.
;--- assume ds:nothing, ss:nothing

;--- since there are 2 32-bit segments, this file
;--- is included twice - hence the dpsuffix text macro.

;--- ltob(long n[eax], char *outb[ebx], int base[cx]);
;--- convert long to string;
;--- outb is assumed to be stack-located.

@dprintf macro cond,fmt,a1,a2,a3,a4,a5
if DBGOPT and cond
  for x,<a5,a4,a3,a2,a1>
   ifnb <x>
	push x
   endif
  endm
%	call dprintf32&dpsuffix
	db fmt,0
endif
endm

ifdef LLOUT
	include vioout.inc
endif

%ltob32&dpsuffix PROC uses edi

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
	add ebx,10
	mov BYTE PTR ss:[ebx],0
	dec ebx
@@nextdigit:
	xor edx, edx
	div edi
	add dl,'0'
	cmp dl,'9'
	jbe @F
	add dl,7+20h
@@:
	mov ss:[ebx],dl
	dec bx
	and eax, eax
	jne @@nextdigit
	cmp ch,0
	je @F
	mov ss:[ebx],ch
	dec ebx
@@:
	inc ebx
	mov eax,ebx
	ret

%ltob32&dpsuffix ENDP

%dprintf32&dpsuffix PROC

local size_:dword
local flag:byte
local longarg:byte
local fill:byte
local szTmp[12]:byte

fmt equ <[ebp+4]>

	pushad
	pushfd
	lea edi,[ebp+4+4]
@@L335:
	mov esi,fmt
nextchar:
	lodsb cs:[esi]
	or al,al
	je done
	cmp al,'%'
	je formatitem
	call print_char
	jmp nextchar
done:
	sub edi,4
	mov ss:[edi],esi ;store new EIP
	mov eax,[ebp]    ;get old value of EBP
	popfd
	mov ebp,esp
	mov [ebp+2*4],eax;now EBP will be restored by popad
	mov [ebp+8*4],edi;finally store new SP
	popad
	pop esp
	retn

formatitem:
	push @@L335
	xor edx,edx
	mov [longarg],dl
	mov bl,1
	mov cl,' '
	cmp BYTE PTR cs:[esi],'-'
	jne @F
	dec ebx
	inc esi
@@:
	mov [flag],bl
	cmp BYTE PTR cs:[esi],'0'
	jne @F
	mov cl,'0'
	inc esi
@@:
	mov [fill],cl
	mov ebx,edx

	.while byte ptr cs:[esi] >= '0' && byte ptr cs:[esi] <= '9'
		lodsb cs:[esi]
		sub al,'0'
		movzx eax,al
		shl ebx,1           ;ebx*2
		lea ebx,[ebx*4+ebx]	;ebx*10
		add ebx,eax
	.endw

	mov [size_],ebx
	cmp BYTE PTR cs:[esi],'l'
	jne @F
	mov [longarg],1
	inc esi
@@:
	lodsb cs:[esi]
	mov fmt,esi
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
	mov eax,ss:[edi]
	add edi,4
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
	mov eax,ss:[edi]
	add edi,4
	cmp [longarg],0
	jne @F
	movzx eax,ax
@@:
	lea ebx,[szTmp]
%	call ltob32&dpsuffix
	mov esi,eax
	push ds
	push ss
	pop ds
	call print_string
	pop ds
	retn

handle_s:
	mov esi,ss:[edi]
	add edi,4
	cmp [longarg],0
	je print_string
	push ds
	mov ds,ss:[edi]
	add di,4
	call print_string
	pop ds
	retn

print_string:	;display string at ds:si
	mov eax,esi
	mov ebx,size_
	.while byte ptr [esi]
		inc esi
	.endw
	sub esi,eax
	xchg eax,esi
	sub ebx,eax
	.if flag == 1
		.while sdword ptr ebx > 0
			mov al,[fill]
			call print_char
			dec ebx
		.endw
	.endif

	.while byte ptr [esi]
		lodsb
		call print_char
	.endw

	.while sdword ptr ebx > 0
		mov al,[fill]
		call print_char
		dec ebx
	.endw
	retn
print_char:
	cmp al,10
	jnz @F
	mov al,13
	call @F
	mov al,10
@@:
ifdef LLOUT
%	call VioPutChr32&dpsuffix
else
	push ebx
	xor ebx,ebx
	mov ah,0Eh
	int 10h
	pop ebx
endif
	retn

fmt equ <>

%dprintf32&dpsuffix ENDP

	align 4
