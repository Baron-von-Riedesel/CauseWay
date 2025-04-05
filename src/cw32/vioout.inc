
;--- debug trace low-level video output;
;--- should work for both real- and protected-mode,

bs equ 8	; backspace

@getcursorpos macro
	movzx ebx, byte ptr ds:[62h];page
	mov bx, ds:[EBX*2+50h]
endm

@setcursorpos macro
	movzx ebx, byte ptr ds:[62h];page
	mov ds:[EBX*2+50h], ax
endm

;--- print a char
;--- no registers modified
;--- no assumptions for ss, ds, es!

ifndef dpsuffix
VioPutChr16 proc
else
%VioPutChr32&dpsuffix proc
endif

wCols textequ <di>
bRows textequ <dh>
bChar textequ <dl>

	push ds
	pushad

	mov bChar, al
	mov ax,0040h
	mov ds, ax
	mov ch, ds:[084h]			; rows-1
	mov cl, ds:[04Ah]			; cols
	mov bRows, ch
	@getcursorpos				; bh=row, bl=col
	mov al, bh		;row pos
	mov ch, 0
	mov wCols, cx
	mul cl
	add ax, ax
	mov bh, 00  ; bx=col pos
	add bx, bx
	add bx, ax
	mov si, ds:[04Eh]			; page offset
	cmp word ptr ds:[063H],3B4h
	jz @F
	add si, 8000h
@@:

	mov al, bChar

	cmp al, 13
	jnz @F
	mov ax, bx
	shr ax, 1
	div cl
	mov al, ah
	xor ah, ah
	add ax, ax
	sub bx, ax
	jmp char_done
@@:
	cmp al, 10
	jnz @F
	add bx, cx
	add bx, cx
	jmp char_done
@@:
	cmp al, bs
	jnz @F
	sub bx, 2
	jmp char_done
@@:
	push ds
	push ax
	mov ax,0B000H    ; CauseWay peculiarity - may work in pm, but requires "VIDEOGDTSELS equ 1"
ifndef dpsuffix
	mov bp, cs
	cmp bp, GROUP16  ; 16-bit only: in real-mode?
	jz @F
endif
	lar bp, ax       ; in raw/vcpi mode?
	jz @F
	mov ax,_B000H    ; in DPMI mode
@@:
	mov ds,ax
	pop ax
	mov [bx+si], al
	pop ds

	inc bx
	inc bx
char_done:
	mov al, bRows
	inc al
	mul cl
	add ax, ax
	cmp bx, ax
	jc @F
	call scrollup
	mov bx, ax
@@:
	mov ax, bx
	push ax
	mov cx, wCols
	shr ax, 1
	div cl
	xchg al, ah
	@setcursorpos
	pop ax
	call cursorset
	popad
	pop ds
	ret

cursorset:
	add ax, ds:[04EH]	;offset page
	mov dx, ds:[063H]
	shr ax, 1			;the CRT offset is one plane only, no attribute bytes 
	mov cl, al			;first high byte
	mov al, 0eh
	out dx, ax
	mov ah, cl			;then low byte
	mov al, 0fh
	out dx, ax
	retn

scrollup:			;scroll up one line
	push ds
	push es
	mov cx,0B000H    ; CauseWay peculiarity
ifndef dpsuffix
	mov bp, cs
	cmp bp, GROUP16  ; 16-bit only: in real-mode?
	jz @F
endif
	lar bp, cx       ; in raw/vcpi mode?
	jz @F
	mov cx,_B000H    ; in DPMI mode
@@:
	mov ds,cx
	mov es,cx
	mov cx, wCols
	push di
	mov di, si
	push di
	mov si, cx
    shl si, 1
	add si, di
	mov al, bRows
	mul cl
	push cx
	mov cx, ax
	shr cx,1
	rep movsd es:[di], ds:[si]
	pop cx
	push di
	mov eax,07200720h
	shr cx, 1
	rep stosd es:[di]
	pop ax
	pop cx
	sub ax, cx
	pop di
	pop es
	pop ds
	retn

wCols textequ <>
bRows textequ <>
bChar textequ <>

ifndef dpsuffix
VioPutChr16 endp
else
%VioPutChr32&dpsuffix endp
endif

