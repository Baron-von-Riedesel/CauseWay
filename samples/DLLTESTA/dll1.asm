
;--- dll in assembly

    .386
    .model flat

CStr macro text:vararg
local sym
    .const
sym db text,0
    .code
    exitm <offset sym>
endm

    .code

main proc c reason:dword

    cmp reason,0
    jnz @F
    mov eax,0
    ret
@@:
    mov eax,0
    ret
main endp


SayHello proc c pMsg:ptr
    mov edx, CStr("Received DLL Message: $")
    mov ah,9
    int 21h
    mov edx, pMsg
    mov ecx, edx
    .while byte ptr [ecx]
        inc ecx
    .endw
    sub ecx, edx
    mov ebx,1
    mov ah,40h
    int 21h
    ret
SayHello endp

    end
