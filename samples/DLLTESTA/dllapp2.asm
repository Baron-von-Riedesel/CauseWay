
;--- dll sample in assembly - using the CauseWay API directly.
;--- run mdllapp2.bat to create the binaries.

	.386
	.model flat
;	.dosseg
	.const  ;without this wlink may put CONST after STACK
	.stack

cwFindModule   equ 0ff33h
cwUnFindModule equ 0ff34h
cwFindFunction equ 0ff35h

sys macro x
	mov ax,x
	int 31h
endm

;--- the EPSP struct is usually defined in cw.inc
EPSP_Struc struct
	db 100h dup (?)
	db 44h dup (?)
EPSP_Exports dd ?
EPSP_Struc ends

CStr macro text:vararg
local sym
    .const
sym db text,0
	.code
	exitm <offset sym>
endm

;--- causeway wants strings in a peculiar format

ExpName macro text:vararg
local sym
    .const
sym db sizeof sym - 1,text
	.code
	exitm <offset sym>
endm

	.code

main proc c

local pProc:dword  
local hModule:dword

	mov esi,ExpName("dll1")
	sys cwFindModule
	.if !CARRY?
		mov hModule, edi
		mov edx,CStr("Module dll1.dll loaded sucessfully",13,10,'$')
		mov ah,9
		int 21h
		mov edi,hModule
		mov edi, [edi].EPSP_Struc.EPSP_Exports
		push ebp
		mov ebp,ExpName("_SayHello")
		sys cwFindFunction
		pop ebp
		.if !CARRY?
			mov eax,[edi+0]
			mov pProc, eax
			push CStr("Hello World!",13,10)
			call pProc
			add esp,4
		.else
			mov edx,CStr("CW API FindFunction '_SayHello' failed",13,10,'$')
			mov ah,9
			int 21h
		.endif

		mov edi, hModule
		sys cwUnFindModule
		.if !CARRY?
			mov edx, CStr("dll1.dll unloaded",13,10,'$')
			mov ah,9
			int 21h
		.endif
	.else
		mov edx, CStr("CW API FindModule failed for dll1",13,10,'$')
		mov ah,9
		int 21h
	.endif
	ret
main endp

start:
	mov eax, ss
	mov ds, eax
	mov es, eax
	call main
	mov ax,4c00h
	int 21h

	end start
