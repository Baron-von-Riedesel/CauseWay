
;*** check dpmi host ***

	.286
	.model tiny
	.stack 500h
	.dosseg
	option casemap:none
	option proc:private

	.386

?MAXNEST equ 32	;max nesting level for option -t

lf	equ 10

PF16 typedef ptr far16

;--- CStr() define a string in .CONST

CStr macro text:VARARG
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm

;*** definitions DOS Protected Mode Interface ***

RMCS    struct          ;real mode call structure
union
rEDI    dd ?            ;+0
rDI		dw ?
ends
union
rESI    dd ?            ;+4
rSI		dw ?
ends
union
rEBP    dd ?            ;+8
rBP		dw ?
ends
RESERVE dd ?            ;+12
union
rEBX    dd ?            ;+16
rBX     dw ?
ends
union
rEDX    dd ?            ;+20
rDX     dw ?
ends
union
rECX    dd ?            ;+24
rCX		dw ?
ends
union
rEAX    dd ?            ;+28
rAX		dw ?
ends
rFlags  dw ?            ;+32
rES     dw ?            ;+34
rDS     dw ?            ;+36
rFS     dw ?            ;+38    
rGS     dw ?            ;+40  
union
rCSIP	dd ?            ;+42
struct
rIP     dw ?
rCS     dw ?
ends
ends
union
rSSSP	dd ?            ;+46
struct
rSP     dw ?
rSS     dw ?
ends
ends
RMCS    ends

MEMINFO struct
maxBlock      dd ?            ;0	;in bytes
freeUnlocked  dd ?            ;4	;pages (max unlocked allocation)
maxLockable   dd ?            ;8	;pages (max locked allocation)
totalAdrSpace dd ?            ;12
unlocked      dd ?            ;16	;total unlocked (=virtual)
freePhys      dd ?            ;20
totalPhys     dd ?            ;24
freeAdrSpace  dd ?            ;28
swapFile      dd ?            ;32
res           db 12 dup (?)
MEMINFO ends

MEMINFOX struct	;used by int 31h, ax=50bh
dwTotalPhys   dd ?
dwTotalHost   dd ?
dwFreeHost    dd ?
dwTotalVM     dd ?
dwFreeVM      dd ?
dwTotalClient dd ?
dwFreeClient  dd ?
dwTotalLocked dd ?
dwMaxLocked   dd ?
dwHighestAddr dd ?
dwLargestBlock dd ?
dwMinSize     dd ?
dwAllocUnit   dd ?
MEMINFOX ends

EXCFRAME struct
_dpmieip	dd ?
_dpmics		dd ?
errorcode	dd ?
_eip		dd ?
_cs			dd ?
_eflags		dd ?
_esp		dd ?
_ss			dd ?
EXCFRAME ends

EXCFRAME16 struct
_dpmieip	dw ?
_dpmics		dw ?
errorcode	dw ?
_eip		dw ?
_cs			dw ?
_eflags		dw ?
_esp		dw ?
_ss			dw ?
EXCFRAME16 ends

EXCFRAMEV10 struct
	EXCFRAME <?>
rDPMIIPx	dd ?	;+20h
rDPMICSx	dd ?	;+24h
rErrx		dd ?	;+28h	;filled with dr6 if exc 01
rEIPx		dd ?	;+2Ch
rCSx		dw ?	;+30h
rInfoBits	dw ?	;+32h
rEFLx		dd ?	;+34h
rESPx		dd ?	;+38h
rSSx		dd ?	;+3Ch
rESx		dd ?	;+40h
rDSx		dd ?	;+44h
rFSx		dd ?	;+48h
rGSx		dd ?	;+4Ch
rCR2		dd ?	;+50h	;exc 0E only
rPTE		dd ?	;+54h	;exc 0E only, bits 0-8 of PTE only
EXCFRAMEV10 ends


EXECRM struct
wEnv     dw ?
wCmdOfs  dw ?
wCmdSeg  dw ?
wFcb1Ofs dw ?
wFcb1Seg dw ?
wFcb2Ofs dw ?
wFcb2Seg dw ?
EXECRM ends

	.data

dwAlloc		dd 0	;-s: amount of memory to alloc before starting shell
dwLinAddr	dd 400000h;-l: default addr for int 31h, ax=504h alloc
wPort		dw 21h	;-i: default port used for IN benchmark
wXMSVer		dw 0
_0000H      dw 0	;descriptor for access to page 0
bIsNT		db 0	; avoid NT/XP's DOSX bug

;--- exec parameter block used for shell option

execparm EXECRM <0,offset cmdl, 0, offset fcb, 0, offset fcb, 0>
	dd 0,0

;--- option flags

OP_ALLOC	equ 1	;-m: alloc memory 501
OP_LINALLOC	equ 2	;-l: alloc memory 504
OP_REALLOC	equ 4	;-n: alloc memory 501, then realloc until error
OP_RMCB		equ 8	;-b: alloc real-mode callbacks until error occurs
OP_SHELL	equ 16	;-s: alloc some memory, then start a shell
OP_MODESW	equ 32	;-r: mode switch benchmark
OP_CLISTI	equ 64	;-c: CLI/STI benchmark
OP_IN		equ 128	;-i: IN benchmark
OP2_NESTED	equ 1	;-t: nested execution test
OP2_HSFATAL	equ 2	;-e: host stack exhaustion
OP2_DESC	equ 4	;-d: alloc descriptors
OP2_EXTMEM	equ 8	;-x: use int 31h, ax=50bh
OP2_HOOK	equ 16	;-z: hook int 21h, then launch another DPMI instance
OP2_EXC0E	equ 32	;-p: cause a page fault

wOpt		label word
bOpt1		db 0
bOpt2		db 0
b16Bit		db 0	;-16: 1=start as 16bit client
bFill		db 0	;-f: fill memory allocated with options -m or -s
bWaitKey	db 0	;-w: wait for a key press in pm
bNestLvl	db 2	;-t: default nesting level

	.const

szMSDOS	db "MS-DOS",0        
szCOMSP	db "COMSPEC="
fcb		db 0, "           ", 0, 0, 0, 0
cmdl	db 0,13
szHelp label byte
	db "cwDPMI v1.0, MIT License.",lf
	db "HX's DPMI app adjusted for CauseWay.",lf
	db "Displays infos about CauseWay's DPMI emulation.",lf
	db "Usage: cwDPMI [ -options ]",lf
;	db "  -16: run as 16-bit client even if host supports 32-bit",lf
	db "  -b: allocate real-mode callbacks until error",lf
	db "  -c: measure STI/CLI [enable/disable interrupt] execution time in p-mode",lf
	db "  -d: allocate descriptors until error",lf
	db "  -e: provoke host stack exhaustion",lf
	db "  -f: fill allocated memory block (-m, -s) with value 'DPMI'",lf
	db "  -i<port>: measure execution time of IN opcode in protected-mode",lf
	db "  -l[addr]: allocate 1000h bytes at addr (def 400000h) with function 504h",lf
	db "  -m: allocate largest free block with function 501h",lf
	db "  -n: realloc a memory block until an error occurs",lf
	db "  -p: hook exc 0Eh, cause a page fault, then skip the faulting instruction",lf
	db "  -r: measure mode switch execution time",lf
	db "  -s<n>: allocate <n> kB of extended memory, then launch a shell",lf
	db "  -t[n]: test nested execution of mode switches; n: nesting level (def 2)",lf
	db "  -w: wait in protected-mode for a keypress before terminating",lf
	db "  -x: try to get extended memory infos thru function 50Bh",lf
	db "  -z: intercept int 21h, then launch another instance of cwDPMI.",lf
	db "      intercept code will modify AH=2 (print char) DOS calls.",lf
	db 0

	.data?

buffer	db 80h dup (?);used to save task state, and DPMI values
blanks 	db ?MAXNEST * 4 + 1 dup (?)
meminf	MEMINFO <>
rmcs	RMCS <>	;real mode call structure used for mode switch benchmark

dfSaveStatePM label fword	;far32 address to call to save task state in pm
dwSaveStatePM	dd ?		;far16 address to call to save task state in pm
				dw ?
dwSaveStateRM	dd ?		;far16 address to call to save task state in rm
wSaveStateSize	dw ?		;size in bytes for save state
dfRawSwitch2RM	df ?		;raw mode switch to rm
dwRawSwitch2PM	dd ?		;raw mode switch to pm

;--- real-mode DGROUP value ( CS, DS, ES, SS )
wDgrp		dw ?
;--- segment register values after initial switch to pm
wCSSel		dw ?
wSSSel		dw ?
wDSSel		dw ?
wPSPSel		dw ?   
wFSSel		dw ?
wGSSel		dw ?

wEnvSel		dw ?	;environment selector
wEnvSeg		dw ?	;environment segment
wParent		dw ?	;PSP's parent segment
wDPMIFlgs	dw ?	;flags returned by DPMI function 0x400
wPICs		dw ?	;master/slave PICs
dwRMCB		dd ?	;realmode callback used for mode switch bench
desc		db 8 dup (?)   

	.code

	include cwdpmi.inc

;--- in: bl=base (10 or 16)

getnum proc
	xor edx, edx
	movzx ebx,bl
	mov ch,0
	.while (cl > 1)
		mov al,es:[si+1]
		or al,20h
		sub al,'0'
		jb done
		cmp al,9
		jbe @F
		sub al,27h
@@:
		movzx eax, al
		cmp eax, ebx
		jnc done
		push eax
		mov eax, edx
		mul ebx
		pop edx
		add edx, eax
		inc si
		dec cl
		inc ch
	.endw
done:
	cmp ch,1
	ret
getnum endp

;--- hook int 21h, then launch another instance of DPMI

hookandrun proc

	.data

oldint21 df 0
wCnt     dw 0

	.code

	mov wCnt, 0

	mov es, wEnvSel
	xor di, di
	xor ax, ax
@@:
	cmp ax, es:[di]
	jz @F
	inc di
	jmp @B
@@:
	add di, 4

	mov rmcs.rSSSP, 0
	mov rmcs.rDX, di
	mov rmcs.rAX, 4B00h
	mov rmcs.rBX, offset execparm

	mov ax, [wEnvSeg]
	mov rmcs.rDS, ax
	mov execparm.wEnv, ax

	mov ax, wDgrp
	mov rmcs.rES, ax
	mov execparm.wCmdSeg, ax
	mov execparm.wFcb1Seg, ax
	mov execparm.wFcb2Seg, ax

	@printf <"launching another instance of DPMI with int 21h hooked",lf>

;--- intercept int 21h

	mov bl, 21h
	mov ax, 204h
	int 31h
	cmp b16Bit, 0
	jz @F
	movzx edx, dx
@@:
;--- set int 21h
	mov dword ptr oldint21+0, edx
	mov word ptr oldint21+4, cx
	mov cx, cs
	mov dx, offset myint21
	movzx edx, dx
	mov ax, 205h
	int 31h

	push ds
	pop es
	mov edi, offset rmcs
	xor cx, cx
	mov bx, 0021h
	mov ax, 300h
	int 31h

;--- restore int 21h
	mov edx, dword ptr oldint21+0
	mov cx, word ptr oldint21+4
	mov bl, 21h
	mov ax, 205h
	int 31h

	test rmcs.rFlags, 1
	jnz err1
	@printf <"DPMI: launch succeeded, int 21h calls: %u",lf>, wCnt
	ret
err1:
	@printf <"DPMI: launch failed",lf>
	ret

myint21:
	push ds
	mov ds, cs:[wDSSel]
	inc [wCnt]
	pop ds
	cmp ah,2
	jnz @F
	cmp dl,'a'
	jb @F
	cmp dl,'z'
	ja @F
	sub dl, 20h
@@:
	jmp cs:[oldint21]

hookandrun endp        

startshell proc
	mov eax, dwAlloc
	and eax, eax
	jz @F
	shl eax, 10		;convert kB to bytes
	push eax
	pop cx
	pop bx
	mov ax,0501h
	int 31h
	jc exit1
	cmp bFill,0
	jz @F
	push bx
	push cx
	pop eax
	mov edx,dwAlloc
	shl edx,10
	call fillblock 
@@:
	mov es, wEnvSel
	xor di,di
	.while (byte ptr es:[di])
		mov dx, di
		mov si, offset szCOMSP
		mov cx, sizeof szCOMSP
		repz cmpsb
		jz found
		mov di, dx
		mov cx,-1
		mov al,0
		repnz scasb
	.endw
	jmp exit2
found:
	mov rmcs.rSSSP, 0
	mov rmcs.rDX, di
	mov rmcs.rAX, 4B00h
	mov rmcs.rBX, offset execparm

	mov ax, [wEnvSeg]
	mov rmcs.rDS, ax
	mov execparm.wEnv, ax

	@printf <"type 'exit' to return to DPMI">

	mov ax, wDgrp
	mov rmcs.rES, ax
	mov execparm.wCmdSeg, ax
	mov execparm.wFcb1Seg, ax
	mov execparm.wFcb2Seg, ax
	push ds
	pop es
	mov edi, offset rmcs
	mov bx,0021h
	mov cx,0
	mov ax,0300h
	int 31h
	test rmcs.rFlags, 1
	jnz err1
	@printf <"DPMI: back from shell",lf>
	ret
err1:
	@printf <"DPMI: launch failed",lf>
	ret
exit1:
	@printf <"unable to alloc memory",lf>
	ret
exit2:
	@printf <"COMSPEC not found",lf>
	ret
startshell endp        

;--- myint69/myint69ex runs in real-mode

myint69 proc
	iret
myint69 endp

myint69ex proc
	sti		; reenable interrupts
	mov ecx, 100000
nextloop:
	call cs:[dwRMCB]
	dec ecx
	jnz nextloop
	iret
myint69ex endp

;--- realmode callback
;--- ds:e/si -> realmode stack
;--- es:e/di -> rmcs
;--- interrupts disabled

mycb32 proc
	cld
	db 67h
	lodsd
	mov es:[edi].RMCS.rCSIP,eax
	add es:[edi].RMCS.rSP,sizeof dword
	iretd
mycb32 endp

mycb16 proc
	cld
	lodsd
	mov es:[di].RMCS.rCSIP,eax
	add es:[di].RMCS.rSP,sizeof dword
	iret
mycb16 endp

;--- this returns timer value in ms

_GetTimerValue proc uses ds

	mov ds, [_0000H]

tryagain:
	mov edx,ds:[46ch] 
	mov al,0C2h		;read timer 0 status + value low/high
	out 43h, al
	xchg edx, edx
	in al,40h
	mov cl,al		;CL = status
	xchg edx, edx
	in al,40h
	mov ah, al		;AH = value low
	xchg edx, edx
	in al,40h		;AL = value high

	test cl,40h		;was latch valid?
	jnz tryagain
	cmp edx,ds:[46ch]	;did an interrupt occur in the meantime?
	jnz tryagain		;then do it again!

	xchg al,ah
;--- usually (counter mode 3) the timer is set to count down *twice*! 
;--- however, sometimes counter mode 2 is set!
	mov ch,cl
	and ch,0110B	;bit 1+2 relevant
	cmp ch,0110B	;counter mode 3?
	jnz @F
;--- in mode 3, PIN status of OUT0 will become bit 15
	shr ax,1
	and cl,80h
	or ah, cl
@@:
;--- now the counter is in AX (counts from FFFF to 0000)
	neg ax
;--- now the count is from 0 to FFFF
	ret
_GetTimerValue endp

;--- get timer value in ms in eax

gettimer proc
	call _GetTimerValue

;--- the timer ticks are in EDX:AX, timer counts down 
;--- a 16bit value with 1,193,180 Hz -> 1193180/65536 = 18.20648 Hz
;--- which are 54.83 ms
;--- to convert in ms:
;--- 1. subticks in ms: AX / 1193
;--- 2. ticks in ms: EDX * 55
;--- 3. total 1+2

	push edx
	movzx eax,ax	;step 1
	cdq
	mov ecx, 1193
	div ecx
	mov ecx, eax
	pop eax 		;step 2
	mov edx, 55
	mul edx
	add eax, ecx	;step 3
	ret
gettimer endp

savereststate proc
	cmp wSaveStateSize,0
	jz done
	mov edi, offset buffer
	cmp b16Bit,1
	jz @F
	call dfSaveStatePM
	ret
@@:
	call dwSaveStatePM
done:
	ret
savereststate endp

enableints proc
	pushf
	pop ax
	test ah,2
	jnz @F
	mov ax,0901h
	int 31h
	@printf <"Interrupts enabled for benchmark",lf>
@@:
	ret
enableints endp

isreliable proc
	test di,200h
	jnz @F
	@printf <"results are unreliable since host has disabled interrupts!",lf>
@@:
	ret
isreliable endp

;--- mode switch tests
;--- 1. pm->rm->pm via INT 69h
;--- 2. pm->rm->pm via int 31h,ax=0300h
;--- 3. rm->pm->rm via realmode callback
;--- 4. pm->rm->pm via raw mode switches

modeswtest proc

;--- set int 69h real-mode

	mov bl,69h
	mov ax,0200h
	int 31h

	push cx
	push dx

	mov cx, wDgrp
	mov dx, offset myint69
	mov ax,0201h
	int 31h

	call enableints

;--- benchmark calling int 69h real-mode directly via INT instruction

	call gettimer
	mov esi, eax
	mov ecx,100000
@@:
	int 69h
	dec ecx
	jnz @B
	call gettimer
	sub eax,esi
	@printf <"time executing 100.000 * INT 69h: %lu ms",lf>, eax

;--- benchmark calling int 69h real-mode via int 31h, ax=0300h

	call gettimer
	mov esi, eax
	mov edi, offset rmcs
	mov rmcs.rSSSP,0
	pushf
	pop rmcs.rFlags
	push ds
	pop es
	mov ecx,100000
@@:
	push ecx
	mov bl,69h
	mov cx,0
	mov ax,0300h
	int 31h
	pop ecx
	dec ecx
	jnz @B
	call gettimer
	sub eax,esi
	@printf <"time executing 100.000 * INT 31h, AX=0300h (Sim INT 69h): %lu ms",lf>, eax

;--- benchmark realmode callback

	mov cx, wDgrp
	mov dx, offset myint69ex
	mov bl,69h
	mov ax,0201h
	int 31h

	push ds
	push cs
	pop ds
	.if b16Bit == 1
		mov si,offset mycb16
		mov di,offset rmcs
	.else
		mov esi,offset mycb32;DS:ESI -> CS:EIP of protected mode routine
		mov edi,offset rmcs	;ES:EDI -> rmcs to be used
	.endif
	mov ax,0303h	;alloc realmode callback
	int 31h
	pop ds
	jc normcb
	mov word ptr dwRMCB+0,dx
	mov word ptr dwRMCB+2,cx
	call gettimer
	mov esi, eax
	int 69h
	call gettimer
	sub eax,esi
	@printf <"time executing 100.000 * real-mode callback: %lu ms",lf>, eax

	mov dx, word ptr dwRMCB+0
	mov cx, word ptr dwRMCB+2
	mov ax,0304h	;free realmode callback
	int 31h
	jmp rmcb_done
normcb:
	@printf <"no realmode callback could be allocated",lf>
rmcb_done:
;--- restore int 69h real-mode vector
	pop dx
	pop cx
	mov bl,69h
	mov ax,0201h
	int 31h

;--- benchmark raw mode switches

	call gettimer
	push eax
	push ds
	pop es
	mov al,00
	call savereststate
	mov ebp, 100000		;register EBP is guaranteed to be preserved 
nexttrip:
	mov ax,wDgrp		;rm DS
	mov dx,ax			;rm SS
	mov si,ax			;rm CS
	mov cx,ax			;rm ES
	mov di,offset raw_rm;rm IP
	mov bx,sp			;rm SP
	jmp dfRawSwitch2RM
raw_rm:					;in real mode now
	mov dx,wSSSel		;pm SS
	mov si,wCSSel		;pm CS
	mov edi,offset raw_pm;pm E/IP
	mov ax,wDSSel		;pm DS
	mov cx,ax			;pm ES
	movzx ebx,sp		;pm E/SP
	jmp dwRawSwitch2PM
raw_pm:					;back in protected mode
	dec ebp
	jnz nexttrip
	mov al,01
	call savereststate
	pushf
	pop di				;save interrupt status
	call gettimer
	pop esi
	sub eax,esi
	@printf <"time executing 100.000 * raw mode switches PM-",3Eh,"RM-",3Eh,"PM: %lu ms",lf>, eax
	call isreliable
	ret

modeswtest endp

;--- test execution speed of disable/enable interrupts

clitest proc

	pushfd
	pop eax
	mov cx,ax
	shr cx,12
	and cl,3
	@printf <"EFlags: %lx (IOPL=%u)",lf>, eax, cx

;--- measure CLI/STI execution time

	call gettimer
	mov esi, eax
	mov ecx, 500000
@@:
	CLI
	nop
	STI
	dec ecx
	jnz @B
	call gettimer
	sub eax,esi
	@printf <"time executing 500.000 * CLI/STI: %lu ms",lf>, eax

;--- measure int 31h, ax=090xh execution time

	call gettimer
	mov esi, eax
	mov ecx, 500000
@@:
	mov ax,0900h
	int 31h
	nop
	int 31h	;restore value
	dec ecx
	jnz @B
	call gettimer
	sub eax,esi
	@printf <"time executing 500.000 * disable/enable interrupts via DPMI: %lu ms",lf>, eax

	ret
clitest endp

;--- test execution speed of IN

inptest proc
	pushfd
	pop eax
	mov cx,ax
	shr cx,12
	and cl,3
	@printf <"EFlags: %lx (IOPL=%u)",lf>, eax,cx

	call enableints

	call gettimer
	mov esi, eax
	mov ecx,200000
	mov dx, wPort
@@:
	in al,dx
	dec ecx
	jnz @B
	call gettimer
	sub eax,esi
	mov dx, wPort
	@printf <"time executing 200.000 * IN %x: %lu ms",lf>, dx, eax
	ret
inptest endp

;--- test RMCB allocation

rmcbtest proc uses bp
	xor bp,bp
	.while bp < 256
		push cs
		pop ds
		mov esi,offset mycb32 ;DS:E/SI -> CS:E/IP of protected mode routine
		mov edi,offset rmcs	;ES:E/DI -> rmcs to be used
		mov ax,0303h
		int 31h
		push es
		pop ds
		jc done
		push cx
		push dx
		inc bp
		@printf <"callback %u (%x:%x) allocated",lf>, bp, cx, dx
	.endw
done:
	.while bp > 0
		dec bp
		pop dx
		pop cx
		mov ax,0304h
		int 31h
		jnc @F
		@printf <"free rmcb %x:%x returned error",lf>, cx, dx
@@:
	.endw
	ret
rmcbtest endp

;--- 32-bit real-mode callback

rmcb32 proc

;	assume ds:nothing,es:nothing,ss:nothing

	cld
	cmp cs:b16Bit,1
	jnz @F
	movzx edi,di
	movzx esi,si
@@:
	db 67h
	lodsd
;--- simulate a RETF in real-mode
	add es:[edi].RMCS.rSP,sizeof dword

	mov ecx, esp
	push eax		;save rm cs:ip

	mov dx,ds
	push es
	pop ds
	@printf <"%sinside rm callback, ss:esp=%x:%lx, ds:esi=%x:%lx",lf>, offset blanks, ss, ecx, dx, esi
	@printf <"%ses:edi=%x:%lx, rm ss:sp=%x:%x, rm cx=%x",lf>, offset blanks, es, edi,\
		es:[edi].RMCS.rSS, es:[edi].RMCS.rSP, es:[edi].RMCS.rCX
	mov al, bNestLvl
	.if ( al > byte ptr es:[edi].RMCS.rCX )
		inc word ptr es:[edi].RMCS.rCX
		mov es:[edi].RMCS.rIP,offset myrmproc
		mov ax, wDgrp
		mov es:[edi].RMCS.rCS,ax
		@printf <"%scalling rm proc [%x:%x]",lf>, offset blanks, es:[edi].RMCS.rCS, es:[edi].RMCS.rIP
		push es:[edi].RMCS.rSSSP	;dpmi ax=0301h may not update rm SP correctly
		mov cx,0
		mov ax,0301h
		int 31h
		@printf <"%sback in rm callback, rm ss:sp=%x:%x, rm cx=%x; exiting",lf>, offset blanks, es:[edi].RMCS.rSS, es:[edi].RMCS.rSP, es:[edi].RMCS.rCX
		pop es:[edi].RMCS.rSSSP
	.else
		@printf <"%sexiting",lf>, offset blanks
	.endif

	pop eax
	mov es:[edi].RMCS.rCSIP, eax

	cmp cs:b16Bit,1
	jz @F
	iretd
@@:
	iret
rmcb32 endp


myrmproc proc far
	mov ds,cs:[wDgrp]
;	assume ds:DGROUP
	mov ax,sp
	.if cx <= ?MAXNEST
		mov word ptr [bx],'  '
		add bx,2
		mov byte ptr [bx],0
		@printf <"%sinside rm proc, ss:sp=%x:%x, cx=%x",lf>, offset blanks, ss, ax, cx
		@printf <"%scalling rm callback %x:%x",lf>, offset blanks, word ptr dwRMCB+2, word ptr dwRMCB+0
		mov word ptr [bx],'  '
		add bx,2
		mov byte ptr [bx],0
		call [dwRMCB]
		sub bx, 2
		mov byte ptr [bx],0
		@printf <"%sback in rm proc, ss:sp=%x:%x; exiting",lf>, offset blanks, ss, sp
		sub bx, 2
		mov byte ptr [bx],0
	.else
		@printf <"unexpected call of rm proc, ss:sp=%x:%x, cx=%x; exiting",lf>, ss, ax, cx
	.endif
	ret
myrmproc endp

;	assume es:DGROUP,ss:DGROUP
;--- test nested execution

nesttest proc
	mov esi,offset rmcb32	;DS:ESI -> CS:EIP of protected mode routine
	push cs
	pop ds
	mov edi,offset rmcs		;ES:E/DI -> rmcs to be used
	mov ax,0303h
	int 31h
	push es
	pop ds
	jc error
	mov word ptr dwRMCB+0,dx
	mov word ptr dwRMCB+2,cx
	@printf <"allocated rm callback %x:%x, rmcs=%x:%lx",lf>, cx, dx, es, edi

	sub sp, sizeof RMCS
	movzx edi, sp
	mov [di].RMCS.rSSSP,0
	mov [di].RMCS.rIP,offset myrmproc
	mov ax,wDgrp
	mov [di].RMCS.rCS,ax
	mov [di].RMCS.rCX,1
	mov [di].RMCS.rBX,offset blanks
	@printf <"calling rm proc [%x:%x], rm cx=%x",lf>, wDgrp, [di].RMCS.rIP, [di].RMCS.rCX
	mov [di].RMCS.rFlags,0
	mov cx,0
	mov ax,0301h
	int 31h
	jnc @F
	@printf <"calling rm proc failed",lf>
	jmp done
@@:
	@printf <"back in protected-mode, rm ss:sp=%x:%x, rm cx=%x",lf>, [di].RMCS.rSS, [di].RMCS.rSP, [di].RMCS.rCX
done:
	lea esp,[esp+sizeof RMCS]
	mov dx,word ptr dwRMCB+0
	mov cx,word ptr dwRMCB+2
	mov ax,0304h
	int 31h
	ret
error:
	@printf <"could not allocate a rm callback",lf>
	ret
nesttest endp

;--- option -e
;--- cause host stack exhaustion by using INT to switch to real-mode
;--- and a raw mode switch to switch back to protected-mode

hsfatal proc
local loops:word
local trmcs:RMCS

	mov loops,0
newloop:
	mov ax,wDgrp
	mov trmcs.rDX,sp
	mov trmcs.rBP,bp
	mov trmcs.rFlags,3202h
	mov trmcs.rES,ax
	mov trmcs.rDS,ax
	mov trmcs.rIP,offset hsfatalrm
	mov trmcs.rCS,ax
	mov trmcs.rSSSP,0
	push ss
	pop es
	lea edi,trmcs
	mov bx,0
	mov cx,0
	mov ax,0301h
	int 31h
	ret
hsfatalx:
	inc loops
	@printf <"%u. iteration",lf>, loops
	jmp newloop
hsfatalrm:
	add sp,4			;no need to return from "far proc"
	movzx ebx,dx		;pm E/SP
	@printf <"in rm, ss:sp=%x:%x",lf>,ss,sp
	mov dx,wSSSel		;pm SS
	mov si,wCSSel		;pm CS
	mov edi,offset hsfatalx;pm E/IP
	mov ax,wDSSel		;pm DS
	mov cx,ax			;pm ES
	jmp dwRawSwitch2PM

hsfatal endp

descalloc proc

	mov bx,0
next:
	mov cx,1
	mov ax,0
	int 31h
	jc error
	inc bx
	jmp next
error:
	@printf <"%u descriptors allocated",lf>, bx
	ret

descalloc endp

;--- get a flat selector in BX

getflatsel proc
	mov cx,1
	mov ax,0
	int 31h
	jc exit
	mov bx,ax
	mov cx,0
	mov dx,0
	mov ax,7
	int 31h
	jc exit
	mov cx,-1
	mov dx,-1
	mov ax,8
	int 31h
exit:
	ret
getflatsel endp

;--- fill a memory block with "DPMI"
;--- eax = block linear address
;--- edx = size in bytes

fillblock proc uses es
	push eax
	push edx
	call getflatsel
	pop ecx
	pop edi
	jc exit
	mov es,bx
	mov eax, "IMPD"
	shr ecx, 2
	cld
	db 67h
	rep stosd
	@printf <"memory block filled with value 'DPMI'",lf>
	push ds
	pop es
	mov ax,1
	int 31h
exit:
	ret
fillblock endp

stdalloc proc
	mov cx,word ptr meminf.maxBlock+0
	mov bx,word ptr meminf.maxBlock+2
	mov ax,0501h
	int 31h
	jc memalloc_failed
	mov eax, meminf.maxBlock
	shr eax, 10
	push si
	push di
	pop esi
	push bx
	push cx
	pop ecx
	@printf <"alloc largest mem block (size=%lu kB) returned handle %lx, base %lx",lf>,\
		eax, esi, ecx
	cmp bFill, 1
	jnz @F
	push bx
	push cx
	pop eax		;linear address in eax
	mov edx,meminf.maxBlock   
	call fillblock
@@:
	mov ax,0502h
	int 31h
	ret
memalloc_failed:
	@printf <"alloc largest block (%lx) failed, AX=%x",lf>,meminf.maxBlock, ax
	ret
stdalloc endp

realloc proc uses ebp

	mov ebp, 1000h
	mov ax,0501h
nexttry:
	push ebp
	pop cx
	pop bx
	int 31h
	jc memalloc_failed
;--- handle in SI:DI now
	mov eax, ebp
	shr eax, 10
	push bx
	push cx
	pop ecx
	@printf <"(re)alloc memory block ok, linear address=%lx, size=%lu kB",lf>, ecx, eax
	add ebp, 100000h	;resize in 1 MB chunks
	jc done	;overflow, shouldn't happen
	mov bp,0
	mov ax,0503h
	jmp nexttry
memalloc_failed:
	.if (ebp > 1000h)
		mov eax, ebp
		shr eax,10
		push si
		push di
		pop esi
		@printf <"realloc memory block (handle=%lx) failed, req. size=%lu kB",lf>, esi, eax
	.else
		@printf <"alloc memory block failed, size= 4 kB",lf>
	.endif
done:
	ret
realloc endp

;--- option -l

linalloc proc
	mov ebx,dwLinAddr
	mov ecx,1000h
	mov edx,1
	mov ax,0504h
	int 31h
	jc linalloc_failed
	@printf <"alloc linear memory at %lxh returned handle %lx, base %lx",lf>, dwLinAddr, esi, ebx
	ret
linalloc_failed:
	@printf <"alloc linear memory at %lxh failed, AX=%x",lf>,dwLinAddr,ax
	ret
linalloc endp

;--- option -p

pagefault proc
local oldexc0e:fword
	mov bl, 0Eh
	mov ax, 202h
	int 31h
	mov dword ptr [oldexc0e], edx
	mov word ptr [oldexc0e+4], cx
	mov edx, offset exc0E32
	cmp b16Bit, 0
	jz @F
	mov dx, offset exc0E16
@@:
	mov cx, cs
	mov ax, 203h
	int 31h

	call getflatsel
	mov es, bx
	xor cx, cx
ifdef __JWASM__
	mov ax, es:[40000000h]	; accepted by Masm, but truncated to 16-bit (=offset 0000)
else
	mov edx, 40000000h
	mov ax, es:[edx]
endif

continue:

	.if cx
		@printf <"page fault occured. Ok.",lf>
	.else
		@printf <"page fault did NOT occur. Error.",lf>
	.endif

	push ds
	pop es
	mov ax, 1
	int 31h

	mov edx, dword ptr [oldexc0e]
	mov cx, word ptr [oldexc0e+4]
	mov bl, 0Eh
	mov ax, 203h
	int 31h
	ret

exc0E16:
	push bp
	mov bp, sp
	mov [bp+2].EXCFRAME16._eip, offset continue
	inc cx
	pop bp
	retf
exc0E32:
	mov [esp].EXCFRAME._eip, offset continue
	inc cx
	db 66h
	retf

pagefault endp

;--- option -x

dispextmem proc

local mix:MEMINFOX

	lea di,mix
	push ss
	pop es
	mov ax,050Bh
	int 31h
	jc notsupp
	mov eax, mix.dwTotalPhys
	shr eax, 10
	@printf <"total physical=%lx (%lu kB)",lf>, mix.dwTotalPhys, eax
	mov eax, mix.dwTotalHost
	shr eax, 10
	mov ecx, mix.dwFreeHost
	shr ecx, 10
	@printf <"total/free host=%lx/%lx (%lu/%lu kB)",lf>, mix.dwTotalHost, mix.dwFreeHost, eax, ecx
	mov eax, mix.dwTotalVM
	shr eax, 10
	mov ecx, mix.dwFreeVM
	shr ecx, 10
	@printf <"total/free VM=%lx/%lx (%lu/%lu kB)",lf>, mix.dwTotalVM, mix.dwFreeVM, eax, ecx
	mov eax, mix.dwTotalClient
	shr eax, 10
	mov ecx, mix.dwFreeClient
	shr ecx, 10
	@printf <"total/free Client=%lx/%lx (%lu/%lu kB)",lf>, mix.dwTotalClient, mix.dwFreeClient, eax, ecx
	mov eax, mix.dwTotalLocked
	shr eax, 10
	mov ecx, mix.dwMaxLocked
	shr ecx, 10
	@printf <"total/max locked=%lx/%lx (%lu/%lu kB)",lf>, mix.dwTotalLocked, mix.dwMaxLocked, eax, ecx
	@printf <"highest addr=%lx",lf>, mix.dwHighestAddr
	mov eax, mix.dwLargestBlock
	shr eax, 10
	@printf <"largest block=%lx (%lu kB)",lf>, mix.dwLargestBlock, eax
	@printf <"min. size=%lx",lf>, mix.dwMinSize
	@printf <"allocation unit=%lx",lf>, mix.dwAllocUnit
	ret
notsupp:
	@printf <"int 31h, ax=50Bh not supported",lf>
	ret
dispextmem endp

;--- check for vendor MS-DOS

chk2f168a proc
	push 0
	pop es
	xor edi, edi
	mov ax, 168Ah
	mov esi, offset szMSDOS
	int 2Fh
	cmp al, 0
	jnz failed
	@printf <"vendor '%s' API entry: %x:%lx",lf>,si,es,edi
	cmp b16Bit,0
	jnz @F

;--- XP's DOSX will return with a 16-bit RETF even for 32-bit clients,
;--- so avoid this call in that case.
	cmp bIsNT, 1
	jz done

	push es
	push edi
	mov ax,100h		;get LDT selector
	call far32 ptr [esp]
	lea esp, [esp+6]
	jc failed2
	jmp ok
@@:
	push bp
	mov bp,sp
	push es
	push di
	mov ax,100h		;get LDT selector
	call far16 ptr [bp-4]
	mov sp,bp
	pop bp
	jc failed2
ok:
	@printf <"'%s' API, ax=100h (get LDT selector): %x",lf>, si, ax
done:
	ret
failed:
	@printf <"no API entry for vendor '%s' found",lf>, si
	ret
failed2:
	@printf <"call '%s' API, ax=100h failed",lf>, si
	ret
chk2f168a endp

print_caps proc
	@printf <"capabilities: %x [">, bx
	test bl,1
	jz @F
	@printf <"paged_acc/dirty ">
@@:
	test bl,2
	jz @F
	@printf <"exc_restartability ">
@@:
	test bl,4
	jz @F
	@printf <"dev_mapping ">
@@:
	test bl,8
	jz @F
	@printf <"conv_mem_mapping ">
@@:
	test bl,16
	jz @F
	@printf <"demand_zero-fill ">
@@:
	test bl,32
	jz @F
	@printf <"write-prot_client ">
@@:
	test bl,64
	jz @F
	@printf <"write-prot_host ">
@@:
	@printf <"]",lf>
	ret
print_caps endp

;*** main for protected-mode 

do_protmode proc

;--- alloc a descriptor for access to BIOS variables
	xor ax, ax
	mov cx, 1
	int 31h
	mov [_0000H], ax
	mov bx, ax
	mov dx, -1
	xor cx, cx
	mov ax, 8
	int 31h

	cmp wOpt, 0
	jnz @F

	@printf <"in protected mode, client CS/SS/DS/FS/GS: %x/%x/%x/%x/%x",lf>,\
		wCSSel, wSSSel, wDSSel, wFSSel, wGSSel
	pushfd
	pop eax
	mov cx, ax
	shr cx, 12
	and cl, 11b
	@printf <"Eflags=%lx (IOPL=%u), ES (=PSP): %x (environment: %x, parent PSP segm: %x)",lf>,\
		eax, cx, wPSPSel, wEnvSel, wParent
	sub sp,6
	mov bp,sp
	sgdt [bp]
	@printf <"GDTR: %x.%lx">, word ptr [bp],dword ptr [bp+2]
	sidt [bp]
	@printf <", IDTR: %x.%lx">, word ptr [bp],dword ptr [bp+2]
	sldt ax
	@printf <", LDTR: %x">,ax
	str ax
	@printf <", TR: %x",lf>, ax
	add sp,6

	mov ax,0400h
	int 31h
	xchg dh,dl
	mov [wDPMIFlgs],bx
	mov [wPICs],dx
	@printf <"DPMI version flags: %x",lf>, wDPMIFlgs
	movzx ax, byte ptr wPICs+0
	movzx cx, byte ptr wPICs+1
	@printf <"master/slave PICs base: %x/%x",lf>, ax, cx
@@:
;--- get save/restore state addresses, BX:CX for rm, SI:E/DI for pm
;--- size of buffer in AX
	mov ax,305h
	int 31h
	mov [wSaveStateSize],ax
	mov word ptr [dwSaveStateRM+0],cx
	mov word ptr [dwSaveStateRM+2],bx
	.if b16Bit == 1
		mov word ptr [dwSaveStatePM+0],di
		mov word ptr [dwSaveStatePM+2],si
	.else
		mov dword ptr [dfSaveStatePM+0],edi
		mov word ptr [dfSaveStatePM+4],si
	.endif
	cmp wOpt, 0
	jnz @F
	.if b16Bit == 1
		@printf <"state save protected-mode: %x:%x">, si, di
	.else
		@printf <"state save protected-mode: %x:%lx">, si, edi
	.endif
	@printf <", real-mode: %x:%x",lf>, word ptr [dwSaveStateRM+2], word ptr [dwSaveStateRM+0]
	@printf <"size state save buffer: %u bytes",lf>, wSaveStateSize
@@:
;--- get raw mode switch addresses, BX:CX for switch to pm, SI:E/DI for switch to rm
	movzx edi,di
	mov ax,0306h
	int 31h
	mov word ptr [dwRawSwitch2PM+0],cx	;offs real (CX)
	mov word ptr [dwRawSwitch2PM+2],bx	;seg  real (BX)
	mov dword ptr [dfRawSwitch2RM+0],edi;offs prot (EDI)
	mov word ptr [dfRawSwitch2RM+4],si	;seg  prot (SI)
	cmp wOpt, 0
	jnz @F
	.if b16Bit == 1
		@printf <"raw jump to real-mode: %x:%x">, si, di
	.else
		@printf <"raw jump to real-mode: %x:%lx">, si, edi
	.endif
	@printf <", to protected-mode: %x:%x",lf>, word ptr [dwRawSwitch2PM+2], word ptr [dwRawSwitch2PM+0]
@@:
;--- get memory info into es:e/di
	mov di, offset meminf
	movzx edi,di
	mov ax,0500h
	int 31h

	cmp wOpt, 0
	jnz status_displayed

	mov eax, [di.MEMINFO.maxBlock]
	shr eax, 10 						; bytes -> kBytes
	mov ecx, [di.MEMINFO.maxLockable]
	shl ecx, 2							; pages -> kBytes
	@printf <"largest free/lockable memory block (kB): %lu/%lu",lf>, eax, ecx
	mov eax, [di.MEMINFO.freeUnlocked]
	.if (eax != -1)
		shl eax,2
	.endif
	@printf <"free unlocked (=virtual) memory (kB): %lu",lf>, eax
	mov eax, [di.MEMINFO.totalAdrSpace]
	.if (eax != -1)
		shl eax,2
	.endif
	mov ecx, [di.MEMINFO.freeAdrSpace]
	.if (ecx != -1)
		shl ecx,2
	.endif
	@printf <"total/free address space (kB): %lu/%lu",lf>, eax, ecx
	mov eax, [di.MEMINFO.totalPhys]
	.if (eax != -1)
		shl eax,2
	.endif
	mov ecx, [di.MEMINFO.freePhys]
	.if (ecx != -1)
		shl ecx,2
	.endif
	@printf <"total/free physical memory (kB): %lu/%lu",lf>, eax, ecx

	mov ax,0E00h
	stc
	int 31h
	.if CARRY?
		@printf <"Int 31h, ax=0E00h (get FPU status) not supported",lf>
	.else
		@printf <'Coprocessor status: %x',lf>, ax
	.endif

;--- get dpmi 1.0 infos into es:e/di
;--- make sure the first 3 bytes are cleared, since
;--- WinXP may return with Carry cleared!

	mov di,offset buffer
	movzx edi,di
	mov word ptr [di],0
	mov byte ptr [di+2],0
	mov ax,401h
	int 31h
	jc no401
	push ax
	movzx ax, buffer+0
	movzx cx, buffer+1
	lea si, buffer+2
	@printf <"vendor: '%s', version: %u.%u",lf>, si, ax, cx
	pop bx
	call print_caps
no401:
	call chk2f168a

status_displayed:
	test bOpt1, OP_ALLOC
	jz @F
	call stdalloc
@@:
	test bOpt1, OP_REALLOC
	jz @F
	call realloc
@@:
	test bOpt1, OP_LINALLOC
	jz @F
	call linalloc
@@:
	test bOpt1, OP_RMCB
	jz @F
	call rmcbtest
@@:
	test bOpt1, OP_MODESW
	jz @F
	call modeswtest
@@:
	test bOpt1, OP_CLISTI
	jz @F
	call clitest
@@:
	test bOpt1, OP_IN
	jz @F
	call inptest
@@:
	test bOpt2, OP2_NESTED
	jz @F
	call nesttest
@@:
	test bOpt2, OP2_HSFATAL
	jz @F
	call hsfatal
@@:
	test bOpt2, OP2_DESC
	jz @F
	call descalloc
@@:
	test bOpt2, OP2_EXTMEM
	jz @F
	call dispextmem
@@:
	test bOpt2, OP2_EXC0E
	jz @F
	call pagefault
@@:
	test bOpt2, OP2_HOOK
	jz @F
	call hookandrun
@@:
	test bOpt1, OP_SHELL
	jz @F
	call startshell
@@:
	cmp bWaitKey, 0
	jz @F
	@printf <"press a key to exit protected-mode...">
	mov ah,10h
	int 16h
	@printf <lf>
@@:
	mov bx, [_0000H]
	mov ax, 1
	int 31h
	ret
do_protmode endp

;--- start a DPMI client

startclient proc

	mov [wPSPSel],es
	mov ax,es:[002Ch]
	mov [wEnvSel],ax
	mov ax,es:[0016h]
	mov [wParent],ax

;--- copy program to conventional memory.

	mov bx,sp
	add bx,2+2
	add bx,16-1
	shr bx,4
	mov ax,100h
	int 31h
	jc exit
	mov es,dx
	mov cx,sp
	add cx,2+2
	mov si,0
	mov di,0
	rep movsb
	mov ds,dx
	mov ss,dx
	mov wDgrp, ax
	mov bx,cs
	mov ax,000bh
	mov di,offset desc
	int 31h
	jc exit
	movzx eax,wDgrp
	shl eax,4
	mov [di+2],ax
	shr eax,16
	mov [di+4],al
	mov [di+7],ah
	mov ax,000ch
	int 31h

	mov [wDSSel],ds
	mov [wCSSel],cs
	mov [wSSSel],ss
	mov [wFSSel],fs
	mov [wGSSel],gs

	movzx eax,ax	;clear register hiwords, just to be safe
	movzx edx,dx
	movzx ecx,cx
	movzx ebx,bx
	movzx esi,si
	movzx edi,di
	movzx ebp,bp	;hiword EBP must be cleared for LEAVE in 32-bit client!
	movzx esp,sp

	mov b16Bit, 1
;	mov b16Bit, 0

	call do_protmode
exit:
	ret
startclient endp

;--- main

main proc c

	mov ax,es:[100h+58]	;EPSP real env
	mov wEnvSeg, ax

	mov si,80h
	mov cl,es:[si]
	mov ah,0
	.while (cl)
		inc si
		mov al,es:[si]
		.if ((ah == '/') || (ah == '-'))
			or al,20h
			.if (al == 'm')
				or bOpt1, OP_ALLOC
			.elseif (al == 'n')
				or bOpt1, OP_REALLOC
			.elseif (al == 'b')
				or bOpt1, OP_RMCB
			.elseif (al == 'c')
				or bOpt1, OP_CLISTI
			.elseif (al == 'd')
				or bOpt2, OP2_DESC
			.elseif (al == 'x')
				or bOpt2, OP2_EXTMEM
			.elseif (al == 'e')
				or bOpt2, OP2_HSFATAL
			.elseif (al == 'z')
				or bOpt2, OP2_HOOK
			.elseif (al == 'f')
				mov bFill, 1
			.elseif (al == 'l')
				or bOpt1, OP_LINALLOC
				mov bl,16
				call getnum
				jc @F
				mov dwLinAddr, edx
@@:
			.elseif (al == 's')
				or bOpt1, OP_SHELL
				mov bl,10
				call getnum
				jc @F
				mov dwAlloc, edx
@@:
			.elseif (al == 'i')
				or bOpt1, OP_IN
				mov bl,16
				call getnum
				jc @F
				mov wPort, dx
@@:
			.elseif (al == 'r')
				or bOpt1, OP_MODESW
			.elseif (al == 'p')
				or bOpt2, OP2_EXC0E
			.elseif (al == 't')
				or bOpt2, OP2_NESTED
				mov bl,10
				call getnum
				jc @F
				.if edx == 0 || edx > ?MAXNEST
					@printf <"error: nesting level must be > 0 and <= %u",lf>, ?MAXNEST
					jmp exit
				.endif
				mov bNestLvl, dl
@@:
			.elseif ((al == '1') && (byte ptr es:[si+1] == '6') && (cl > 1))
				mov b16Bit, 1
				dec cl
				inc si
			.elseif (al == 'w')
				mov bWaitKey, 1
			.else
				mov ax, offset szHelp
				call _strout
				jmp exit
			.endif
		.endif
		mov ah,al
		dec cl
	.endw

	mov ax,3306h
	int 21h
	cmp bx,3205h
	jnz @F
	mov bIsNT, 1
@@:
	call startclient
exit:
	ret

main endp

;--- start program, cs = dgroup, ds,es = psp, ss = stack

start:
	mov bx, cs
	mov ax, 000ah
	int 31h
	mov ds,ax
	lsl dx,ax
	inc dx
	mov ss,ax
	mov sp,dx
	call main
	mov ax,4C00h
	int 21h

	END start
