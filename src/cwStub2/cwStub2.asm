
;*** stub to load uncompressed CauseWay extender CW32.EXE
;--- will not understand LFNs in directories contained in PATH.

	.286
	.model tiny
	.stack 512
	option casemap:none

cr equ 13
lf equ 10

if 0;def __JWASM__
	option mz:40h	;if JWasm's -mz option is used, header size will be min(64)
endif

MZHdr struct
	dw ?	;00
	dw ?	;02
wPages	dw ?;04 # of 512 byte pages
	dw ?	;06
	dw ?	;08
MinAllc dw ?;0A min paras to add
	dw ?	;0C
EntrySS	dw ?;0E
EntrySP	dw ?;10
	dw ?	;12
EntryIP	dw ?;14
EntryCS	dw ?;16
	dw ?	;18 First relocation item offset.
	db ?	;1A Overlay number.
MZHdr ends

	.code

start:
	mov ax,cs
	mov bx,ss
	mov cx,sp		;cx=size stack
	mov dx,es
	sub bx,ax		;bx=size code segment (para)
	shl bx,4		;bx=size code segment (bytes)
	mov ss,ax		;SS=CS
	add sp,bx
	mov cx,sp		;cx=size code + stack

main proc

local  pathvar:word        ;tmp var during PATH scan
local  wPSP:word  
local  dosexec[4]:word
local  szPgm[128]:byte
local  mzhdr:MZHdr

;--- move code to the end of this memory block

	xchg bx,cx		;BX=size code + stack, CX=size code
	shr bx,4
	mov ax,es:[2]	;end of block - para address
	sub ax,bx		;ax=destination address paragraph
;--- the memory block may be so small that dest < src + size;
;--- therefore the move is done with direction "down".
	mov es,ax
	mov ss,ax		;SS=new DGROUP
	mov di,cx
	mov si,cx
	std
	rep movsb es:[di],cs:[si]
	cld
	push es
	push @F
	retf			;CS=new DGROUP
@@:
	mov ds,ax		;DS=new DGROUP

;--- move done. Now search extender

	mov wPSP,dx
	mov es,dx		;ES=PSP
	mov es,es:[002Ch]
	call searchpathvar	;returns DI=offset PATH=, if none exists, ES:[DI]==0
	call searchextender	;search CW32.exe
	jb error3           ;CW32 not found

;--- extender found. Read MZ header

	mov bx,ax
	lea dx,mzhdr
	mov cx,sizeof MZHdr
	mov ah,3Fh
	int 21h
	jc error2
	cmp ax,cx
	jnz error2
	mov ah,3eh
	int 21h

;--- prepare overlay load

	lea bx,dosexec
	mov ax,wPSP
	add ax,10h
	mov [bx+0],ax
	mov [bx+2],ax
	mov cx,mzhdr.wPages
	shl cx,5			;pages to paras (512-256-128-64-32-16)
;	add cx,mzhdr.MinAllc
	add ax,cx
	mov cx,cs
	cmp cx,ax
	jb error1			;current block too small
	push ds
	pop es				;DS:DX=path, ES:BX=parameter block
	lea dx,szPgm		;path of CW32.EXE
	mov ax,4B03h		;load as overlay
	int 21h
	jb error2			;fatal load error

;--- load ok. finally jump to extender startup code.

	mov ax,dosexec		;AX=CW32's base
	mov bx,ax
	mov si,wPSP			;SI=PSP
	add ax,mzhdr.EntrySS
	mov dx,mzhdr.EntrySP
	mov cx,mzhdr.EntryIP
	add bx,mzhdr.EntryCS
	mov es,si
	mov ds,si
	mov ss,ax
	mov sp,dx
	push bx
	push cx
	retf
error1:
	mov dx,offset err1	;not enough memory
	jmp erroutandexit
error2:
	mov dx,offset err2	;fatal load error
	jmp erroutandexit
error3:
	mov dx,offset err3	;cw32.exe not found
erroutandexit:
	push cs
	pop ds
	mov ax,0900h
	int 21h
	mov ax,4D00h		;get DOS error code
	int 21h
	mov ah,4Ch			;terminate
	int 21h

;*** search PATH environment variable
;*** In: ES=environment segment, CS,DS,SS=DGROUP
;--- Out: offset PATH in DI or -1

searchpathvar:
	sub di,di
nextline:
	mov si,offset dPath
	mov cx,sizeof dPath
	repz cmpsb
	jz found
	mov al,00
	mov ch,7Fh
	repnz scasb
	cmp al,es:[di]
	jnz nextline
;--- if PATH not found, [ES:DI] = 0
found:
	retn

;*** search extender, first in current dir, then dirs in PATH
;*** In: ES:DI=PATH variable or byte ptr ES:[DI]==0
;***     CS,SS,DS=DGROUP
;--- Out: NC if found, AX=handle 

searchextender:
	mov pathvar,di		;init with start PATH
	lea dx,szPgm
	mov di,dx
nextpath:				;<----
	mov si,offset szExtender
nextchr:
	lodsb cs:[si]
	mov [di],al
	inc di
	cmp al,00
	jnz nextchr
	mov ax,3D00h		;try to open
	int 21h
	jnb pgmfound		;found!
	mov si,pathvar
	cmp byte ptr es:[si],0	;if PATH doesn't exist or is at its end
	jz pgmnotfound
	mov di,dx
	lea bx,[di+(sizeof szPgm - (SIZEEXTENDER + 1))]
nextchr2:
	lodsb es:[si]	;search next path
	cmp al,';'
	jz endpath1
	cmp al,00
	jz endpath1x
	cmp bx,di		;path too long?
	jb nextchr2
	mov [di],al
	inc di
	jmp nextchr2
endpath1x:
	dec si
endpath1:
	cmp di,dx
	jz endpath2
	CMP byte ptr [di-01],'\'
	jz endpath2
	mov byte ptr [di],'\'
	inc di
endpath2:
	mov pathvar,si
	jmp nextpath
pgmnotfound:
	stc
pgmfound:
	retn
main endp

;--- constants

dPath  db 'PATH='

szExtender db 'CW32.EXE',00
SIZEEXTENDER equ $ - szExtender
err1   db 'Not enough memory',cr,lf,'$'
err2   db 'Fatal load error',cr,lf,'$'
err3   db 'Cannot find DOS-Extender CW32.EXE',cr,lf,'$'

if ($-start) and 0fh
	db 16 - (($-start) and 0fh) dup (0)
endif

	end start
