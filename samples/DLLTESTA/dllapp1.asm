
;--- dll sample in assembly, using C wrapper functions in dllfunc.obj.
;--- run mdllapp1.bat to create the binaries.

	.386
	.model flat
;	.dosseg
	.const  ;ensures that wlink puts CONST before STACK
	.stack

LoadModule     proto c :ptr
GetProcAddress proto c :dword, :ptr
FreeModule     proto c :dword

CStr macro text:vararg
local sym
    .const
sym db text,0
	.code
	exitm <offset sym>
endm

	.code

main proc c

local pProc:dword  
local hModule:dword

	invoke LoadModule, CStr("dll1")
	.if eax
		mov hModule, eax
		mov edx,CStr("dllapp1: Module dll1.dll loaded sucessfully",13,10,'$')
		mov ah,9
		int 21h
		invoke GetProcAddress, hModule, CStr("_SayHello")
		.if eax
			mov pProc, eax
			push CStr("Hello World!",13,10)
			call pProc
			add esp,4
		.else
			mov edx,CStr("dllapp1: GetProcAddress('_SayHello') failed",13,10,'$')
			mov ah,9
			int 21h
		.endif

		invoke FreeModule, hModule
		mov edx, CStr("dllapp1: dll1.dll unloaded",13,10,'$')
		mov ah,9
		int 21h

	.else

		mov edx, CStr("dllapp1: Failed to load dll1.dll",13,10,'$')
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
