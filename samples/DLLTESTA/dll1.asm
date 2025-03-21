
;--- CauseWay dll in assembly

    .386
    .model flat
    option casemap:none

CStr macro text:vararg
local sym
    .const
sym db text,0
    .code
    exitm <offset sym>
endm

    .code

LibMain proc c reason:dword

    cmp reason,0
    jnz @F
    mov edx, CStr("dll1.LibMain: start (reason == 0)",13,10,'$')
    mov ah,9
    int 21h
    mov eax,0
    ret
@@:
    mov edx, CStr("dll1.LibMain: exit (reason != 0)",13,10,'$')
    mov ah,9
    int 21h
    mov eax,0
    ret
LibMain endp


SayHello proc c pMsg:ptr
    mov edx, CStr("dll1.SayHello: received message: $")
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

;--- in CauseWay, ds & es hold the PSP selector on entry; ss=flat 4G

start:
    mov edx,ss
    mov ds,edx
    mov es,edx
    invoke LibMain, eax
    retd

    end start
