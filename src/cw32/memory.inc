
;--- emulation of DPMI ax=050xh, ax=060xh, ax=0703h and ax=080xh functions;
;--- also the VMM page fault handler for swapfile management.

;--- todo: ax=801h is a dummy - must be implemented for DOS/4G compatibility

_cwDPMIEMU$1 segment
	align 4
OldExcep14      df ?
_cwDPMIEMU$1 ends

;--- make a stack switch;
;--- ds set to GROUP16
;--- eax & es modified; old ss:esp pushed onto new stack.

RawSwitchStack proc
        mov     ax,KernalDS             ;make data addresable.
        mov     ds,eax
        assume ds:GROUP16
        push    ebx
        push    ecx
        push    edx
        mov     ebx,ss
        lea     ecx,[esp+4*4]
        pushfd
        cli
        mov     edx,[RawStackPos]
        sub     [RawStackPos],RawStackDif
        pop     eax
        test    ah,2
        jz      @F
        sti
@@:
        mov     ax,KernalSS
        mov     ss,eax
        mov     esp,edx
        push    ebx          ;save old SS:ESP
        push    ecx
        push    edx          ;old RawStackPos
        sub     ecx,4*4      ;ds:ecx->edx, ecx, ebx, retaddr
        mov     es,ebx
        mov     edx,es:[ecx+0*4]
        mov     ebx,es:[ecx+2*4]
        mov     eax,es:[ecx+3*4]
        mov     ecx,es:[ecx+1*4]
        jmp     eax
        
RawSwitchStack endp

;--- switch stack back
;--- this must be interrupt safe, that is,
;--- [RawStackPos] must be set only AFTER stack has been restored.
;--- do no change flags here!

@RawRestoreStack macro reg
        pop     reg
        lss     esp,[esp]
        mov     [RawStackPos],reg
endm

;-------------------------------------------------------------------------------
;
;Get memory using RAW/VCPI. The worker behind int 31h, ax=501h
;Also called by RawResizeMemory.
;
;On Entry:-
;
;BX:CX  - Size of block.
;
;On Exit:-
;
;Carry on error else,
;
;SI:DI  - Handle.
;BX:CX  - Logical address of block.
;note: hiwords of ebx,ecx,esi,edi modified, but the caller (RawDPMIPatch) will restore them;
;      and in case of errors, all 4 regs are fully restored.
;
RawGetMemory    proc    near
        ;
        @dprintf DOPT_MEM or DOPT_STACK,<"RawGetMemory enter, bx:cx=%04X:%04X, ss:esp=%X:%lX",10>,ebx,ecx,ss,esp
        push    eax
        push    edx
        push    ebp
        push    ds
        push    es
;
        call    RawSwitchStack
        assume ds:GROUP16

        mov     ax,KernalZero
        mov     es,eax
        shl     ebx,16          ;get block size as 32-bit.
        mov     bx,cx
        add     ebx,4095        ;round up to next 4k.
        shr     ebx,12
;        or      ebx,ebx
        jz      mem1_error
;mem1_start:
        mov     edi,LinearBase  ;Get starting point.
        mov     ecx,LinearLimit
        sub     ecx,edi         ;Get memory present.
        shr     ecx,12          ;Get pages.
        shr     edi,12          ;Get page number.
;        shl     edi,2           ;dword per entry.
;        add     edi,DETMAPADDR ;point to page details.
        lea     edi,[edi*4+DETMAPADDR]
        ;
        ;Look for a big enough block of free address space.
        ;
        xor     eax,eax         ;value to look for.
        cld

        ;
        ;EAX    - zero, what we're looking for.
        ;ECX    - entries to check.
        ;EDI    - address of entries to check.
        ;
        ;EBX    - number of entries we want.
        ;

mem1_l0:
        repne   scasd

        ;
        ;Nothing found means extend end of memory map.
        ;
        jnz     mem1_ExtendNew

        ;
        ;Point everything back to the matching value.
        ;
        sub     edi,4
        inc     ecx

        ;
        ;Save current position as start of last free block incase
        ;this block extends to the end of memory.
        ;
        mov     esi,edi         ;save last position.

        ;
        ;If we only wanted one page then we're done, this is it!
        ;
        cmp     ebx,1
        jz      mem1_l2         ;Speed up 4K allocations.

        ;
        ;Set the number of pages to scan. This should be the number of
        ;pages we want but has to be reduced to the number of pages
        ;actually available when the request would go off the end of
        ;memory.
        ;
        mov     edx,ebx
        cmp     ecx,ebx         ;Enough space to check?
        jnc     mem1_l1
        mov     edx,ecx

        ;
        ;Scan specified number of entries to see if they're all free.
        ;
mem1_l1:
        push    ecx
        push    edi
        mov     ecx,edx
        repe    scasd           ;Scan for free pages.
        pop     edi
        pop     ecx
        jnz     mem1_l3         ;not enough free entries.

        ;
        ;All the entries were free, now check if we were truncating the
        ;length to stop us running off the end of memory.
        ;
        cmp     edx,ebx         ;Truncated length?
        jnz     mem1_ExtendEnd
        jmp     mem1_l2

        ;
        ;One way or another we didn't find the number of entries we were
        ;after so restart the search.
        ;
mem1_l3:
        add     edi,4
        dec     ecx
        jmp     mem1_l0

        ;
        ;We've found what we wanted without any extra messing around so
        ;hand this over to the marker.
        ;
mem1_l2:
        sub     esi,DETMAPADDR ;point to page details.
        shr     esi,2
        jmp     mem1_MarkMemRet


        ;
        ;Last block is free so use it as the basis for extension.
        ;
mem1_ExtendEnd:
        mov     edi,ecx         ;remaining entries.
        sub     esi,DETMAPADDR
        shr     esi,2           ;Get start page number.
        mov     ecx,ebx         ;Total pages needed.
        sub     ecx,edi         ;Pages found so far.
        jmp     mem1_Extend
        ;
mem1_ExtendNew:
        ;Last block not free so new block will start at LinearLimit.
        ;
        mov     ecx,ebx         ;Set pages needed.
        mov     esi,LinearLimit ;New start address.
        shr     esi,12
        ;
mem1_Extend:
        ;Memory map needs extending so get on with it.
        ;
        call    ExtendLinearMemory      ;Try and extend memory map.
        jnc     mem1_MarkMemRet         ;Mark the block as used.
        ;
        ;Not enough memory so report failure.
        ;
        stc
        jmp     mem1_exit
        ;
mem1_MarkMemRet:
        ;Got the memory so mark it in use.
        ;
        sub     FreePages,ebx   ;reduce free page count.
        add     AllocedPages,ebx
        ;
        mov     ecx,ebx         ;Get number of pages again.
        mov     edx,esi
;        mov     edi,esi         ;Get base page again.
;        shl     edi,2
;        add     edi,DETMAPADDR
        lea     edi,[esi*4+DETMAPADDR]
        mov     eax,MEM_END
        rep     stosd
        mov     d es:[edx*4+DETMAPADDR],MEM_START
        ;
        ;Now return details to caller.
        ;
        shl     esi,12          ;Convert back to a real address.
        @dprintf DOPT_MEM or DOPT_STACK,<"RawGetMemory exit, base=%lX",10>,esi
        mov     di,si
        shr     esi,16
        mov     cx,di
        mov     bx,si
        clc
        jmp     mem1_exit
        ;
mem1_error:
        stc
        ;
mem1_exit:
        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edx
        pop     eax
        ret
RawGetMemory    endp


;-------------------------------------------------------------------------------
;
;Re-size a block of memory.
;
;On Entry:-
;
;SI:DI  - Handle.
;BX:CX  - New block size.
;
;On Exit:-
;
;SI:DI  - New handle.
;BX:CX  - New address.
;note: hiwords of ebx,ecx,esi,edi are modified, but the caller (RawDPMIPatch) will restore them;
;      and in case of errors, all 4 regs are fully restored.
;
;3 cases are handled:
; - block shrinks - the released part is still accessible, though (DOS/4G compatible)
; - block grows in-place (linear start address doesn't change)
; - block grows, linear start address changes
;
RawResizeMemory    proc    near
        ;
        @dprintf DOPT_MEM or DOPT_STACK,<"RawResizeMemory enter, bx:cx=%04X:%04X, ss:esp=%X:%lX",10>,ebx,ecx,ss,esp
        push    eax
        push    edx
        push    ebp
        push    ds
        push    es
;
        call    RawSwitchStack
        assume ds:GROUP16

        mov     ax,KernalZero
        mov     es,eax
        shl     esi,16
        mov     si,di           ;Get real address.

        shl     ecx,16
        shrd    ecx,ebx,16      ;BX:CX -> ECX

        add     ecx,4095
        shr     ecx,12          ;round up to page size
        jz      mem2_error
        ;
        ;Check for a valid address.
        ;
        test    si,0fffh        ;bits 0-11 of handle must be 0; all memory on page boundaries.
        jnz     mem2_error
        cmp     esi,LinearBase
        jc      mem2_error
        cmp     esi,LinearLimit
        jnc     mem2_error
        shr     esi,12          ;Get page number.
        mov     eax,es:[esi*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_START
        jnz     mem2_error      ;must be marked as block start
        ;
        ;Work out the block's current size.
        ;
        push    ecx
        mov     edx,esi
        mov     ecx,LinearLimit
        shr     ecx,12
        sub     ecx,edx
mem2_l0:
        inc     edx
        mov     eax,es:[edx*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_END
        loopz   mem2_l0
;mem2_l1:
        sub     edx, esi
        mov     ebp, edx
        ;Shrinking or expanding?
        ;
        pop     ecx
        cmp     ecx,ebp
        jz      mem2_RetNewAddr ;size hasn't changed
        jnc     mem2_Bigger
        ;
;mem2_Smaller:
        ;Shrinking the block so get on with it.
        ;
        ;ECX - New size in pages.
        ;EBP - Current size in pages.
        ;ESI - Current start page.
        ;
        mov     edx,esi
        add     edx,ecx         ;move to new block end.
        sub     ebp,ecx         ;get number of pages to lose.
        mov     ecx,ebp
        ;
        add     FreePages,ebp   ;update number of free pages.
        sub     AllocedPages,ebp
        ;
mem2_s0:
        and     d es:[edx*4+DETMAPADDR],not MEM_MASK
        or      d es:[edx*4+DETMAPADDR],MEM_FREE
        and     d es:[edx*4+PTMAPADDR],not (1 shl PT_DIRTYBIT)  ;reset dirty bit
        inc     edx
        loop    mem2_s0
        call    EmuCR3Flush
        jmp     mem2_RetNewAddr
        ;
mem2_Bigger:
        ;Want to expand the block so get on with it.
        ;
        ;ECX - New size in pages.
        ;EBP - Current size in pages.
        ;ESI - Current start page.
        ;
;mem2_b0:
        mov     edx,esi
        add     edx,ebp         ;move to end of this block.
        mov     ebx,LinearLimit
        shr     ebx,12
        sub     ebx,edx         ;get pages to end of memory.
        or      ebx,ebx
        jz      mem2_Extend
        ;
        ;See if the next block is free.
        ;
        mov     eax,es:[edx*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_FREE
        jnz     mem2_NewBlock   ;no hope so get a new block.
        ;
        ;Check how big this next block is.
        ;
        mov     edi,ebp         ;use current size as basis.
mem2_b1:
        mov     eax,es:[edx*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_FREE
        jnz     mem2_NewBlock   ;No hope so get a new block.
        inc     edi
        cmp     edi,ecx         ;Got enough yet?
        jz      mem2_MarkAndRet
        inc     edx
        dec     ebx
        jnz     mem2_b1         ;keep trying.
        ;
        ;Reached the end of the memory map so try extending it.
        ;
        push    ecx
        sub     ecx,edi         ;pages still needed.
        call    ExtendLinearMemory
        pop     ecx
        jc      mem2_error
        jmp     mem2_MarkAndRet
        ;
mem2_Extend:
        ;Need to extend the memory map to provide a block of free memory
        ;after the current block.
        ;
        push    ecx
        sub     ecx,ebp         ;pages needed.
        call    ExtendLinearMemory
        pop     ecx
        jc      mem2_error
        ;
mem2_MarkAndRet:
        ;Mark the new memory as in use and exit.
        ;
        ;ECX - New size in pages.
        ;EBP - Current size in pages.
        ;ESI - Current start page.
        ;
        mov     edx,esi
        add     edx,ebp         ;move to start of new memory.
        sub     ecx,ebp         ;pages to mark.
        ;
        sub     FreePages,ecx   ;update number of free pages.
        add     AllocedPages,ecx
        ;
mem2_mr0:
        and     d es:[edx*4+DETMAPADDR],not MEM_MASK
        or      d es:[edx*4+DETMAPADDR],MEM_END
        inc     edx
        loop    mem2_mr0
        jmp     mem2_RetNewAddr
        ;
mem2_NewBlock:
        ;Nothing for it but to try and allocate a new block of memory.
        ;
        push    ecx
        push    ebp
        push    esi
        shl     ecx,12
        mov     ebx,ecx
        shr     ebx,16
        call    RawGetMemory
        pushfd
        shl     ebx,16
        mov     bx,cx
        popfd
        pop     esi
        pop     ebp
        pop     ecx
        jc      mem2_error
        ;
        ;Copy current block to new block.
        ;
        pushad
        mov     ecx,ebp
        shl     ecx,12
        shl     esi,12
        mov     edi,ebx
        push    ds
        push    es
        pop     ds
        shr     ecx,2
        rep     movsd
        pop     ds
        popad
        ;
        ;Release current block.
        ;
        pushad
        shl     esi,12
        mov     di,si
        shr     esi,16
        call    RawRelMemory
        popad
        ;
        ;Use new block in place of original.
        ;
        mov     esi,ebx
        shr     esi,12
        ;
mem2_RetNewAddr:
        ;Return possibly new address/handle to caller.
        ;
        shl     esi,12          ;Get a real address again and
        mov     di,si           ;use it as both the memory
        mov     cx,si           ;address to return and the handle.
        shr     esi,16
        mov     bx,si
        clc
        jmp     mem2_exit
        ;
mem2_error:
        stc
mem2_exit:
        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edx
        pop     eax
        ret
RawResizeMemory    endp


;-------------------------------------------------------------------------------
;
;Release block of memory using RAW/VCPI.
; note: as in resize case, pages of block remain accessible! (DOS/4G compatible)
;
;On Entry:-
;
;SI:DI  - handle (Base linear address).
;
RawRelMemory    proc    near
        ;
        @dprintf DOPT_MEM or DOPT_STACK,<"RawRelMemory enter, si:di=%X:%X, ss:esp=%X:%lX",10>,esi,edi,ss,esp
        pushad
        push    ds
        push    es
        ;
        shl     esi,16          ;Get block base address.
        mov     si,di
;
        call    RawSwitchStack
        assume ds:GROUP16

        mov     ax,KernalZero
        mov     es,eax
        ;
        ;Check for a legal address.
        ;
        test    si,0fffh
        jnz     mem3_error
        cmp     esi,LinearBase  ;inside logical memory map?
        jc      mem3_error
        cmp     esi,LinearLimit
        jnc     mem3_error
        shr     esi,12          ;Get page number.
        mov     eax,es:[esi*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_START
        jnz     mem3_error

        mov     ecx,LinearLimit
        shr     ecx,12
        sub     ecx,esi         ;Get max pages remaining.
        ;
        ;Mark all this block's pages as free.
        ;
mem3_0:
        inc     FreePages
        dec     AllocedPages
        and     d es:[esi*4+DETMAPADDR],not MEM_MASK
        or      d es:[esi*4+DETMAPADDR],MEM_FREE
        and     d es:[esi*4+PTMAPADDR],not (1 shl PT_DIRTYBIT)
        inc     esi
        mov     eax,es:[esi*4+DETMAPADDR]
        and     eax,MEM_MASK
        cmp     eax,MEM_END
        loopz   mem3_0
        ;
mem3_1:
        call    EmuCR3Flush
        ;
        clc
        jmp     mem3_exit
        ;
mem3_error:
        stc
mem3_exit:
        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        popad
        ret
RawRelMemory    endp


;-------------------------------------------------------------------------------
;
;Reset dirty bit for specified memory.
;worker behind DPMI func ax=703h
;
;On Entry:
;
;BX:CX  Starting linear address of pages to discard
;SI:DI  Number of bytes to discard
;
;On Exit:
;
;Carry clear, all other registers preserved.
;
;-------------------------------------------------------------------------------
RawDiscardPages proc    near
        push    ds
        push    es
        pushad
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax

;EBX=base address, ESI=size
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
;
;Round start up a page.
;
        mov     eax,ebx
        add     ebx,4095
;        and     ebx,not 4095
        and     bx,0f000h
        sub     eax,ebx
        neg     eax
        cmp     eax,esi
        jnc     mem4_8
        sub     esi,eax
;
;Round length down a page.
;
;        and     esi,not 4095
        and     si,0f000h
        or      esi,esi
        jz      mem4_8
;
;Get page values.
;
        mov     edx,ebx
        shr     ebx,12
        shr     esi,12
        mov     ecx,esi
;
;Modify all page tables.
;
        mov     esi,PTMAPADDR           ;base of page alias memory.
mem4_0:
        cmp     edx,LinearBase
        jc      mem4_1
        cmp     edx,LinearLimit
        jnc     mem4_2
        and     d es:[esi+ebx*4],not ((1 shl PT_DIRTYBIT)+(1 shl PT_SWAPBIT)) ;clear dirty & disk bits.
mem4_1:
        add     edx,4096
        inc     ebx
        loop    mem4_0
;
mem4_2:
        call    EmuCR3Flush             ;update page cache.
;
mem4_8:
        clc
        popad
        pop     es
        pop     ds
        assume ds:nothing
        ret
RawDiscardPages endp

;--- scan mapping entries in page directory
;--- edi=page directory
;--- ebx=physical start of region
;--- ecx=size of region in 4MB chunks
;--- out: edi = linear address of region
;
;The check if there's already a mapping for the required region is faulty and never succeeds;
;however, this is also true for DOS/4G - so it's not been fixed.

        assume ds:GROUP16

if 0 ;not active, since not compatible with DOS/4G

SearchRegion proc

        movzx   eax,[wP2LIndex]         ;current bottom index of P2L mappings
        jmp     sr_chk
sr_next:
        mov     edx,es:[edi+eax*4]
        and     dx,(1 shl PT_PRESBIT) or (1 shl PT_PBLKBIT)
        cmp     dx,(1 shl PT_PRESBIT) or (1 shl PT_PBLKBIT)
        jnz     sk_skip

        push    edi
        mov     edi,eax
        shl     edi,10
        mov     edx,es:[edi+PTMAPADDR]
        pop     edi
        and     dx,0f000h
;--- this check compares just the start address. That's not DPMI compliant.
        cmp     edx,ebx                 ; see if matches desired linear address
        jne     sr_skip                 ; no, continue as before
        mov     edi,eax
        shl     edi,10+12               ; convert index to 4M space
        clc
        ret
sr_skip:
        inc     eax
sr_chk:
        cmp     ax,DETINDEX
        jnz     sr_next
        stc
        ret
SearchRegion endp
endif

;--- search for a free region that's large enough
;--- out: if NC, eax=PDE index of free region.
;---      C if no region found.

SearchFreeRegion proc

        movzx   eax,[wP2LIndex]         ;current bottom index of P2L mappings
        jmp     sfr_chk
sfr_next:
        test    b es:[edi+eax*4],1 shl PT_PRESBIT
        jz      sfr_freebeg
        inc     eax
sfr_chk:
        cmp     ax,DETINDEX
        jnz     sfr_next
        stc
        ret
sfr_freebeg:
        mov     edx,eax
        push    ecx
        jmp     sfr_chk2
@@:
        inc     eax
        test    d es:[edi+eax*4],1 shl PT_PRESBIT
sfr_chk2:        
        loopz   @B
        pop     ecx
        jnz     sfr_chk
        mov     eax,edx
        ret

SearchFreeRegion endp

;-------------------------------------------------------------------------------
;
;Map physical memory into address space and return linear access address.
;This is the worker behind int 31h, ax=800h emulation.
;
;BX:CX  Physical address of memory
;SI:DI  Size of region to map in bytes
;
;Returns
;
;Carry set on error else,
;
;BX:CX  Linear address that can be used to access the physical memory.
;
RawMapPhys2Lin  proc    near
        push    ds
        push    es
        push    eax
        push    edx
        push    esi
        push    edi
        push    ecx
        push    ebx
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax
;
;ebx=physical address, esi=length 
;
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        @dprintf DOPT_MEM,<"RawMapPhys2Lin enter, BX:CX=%lX, SI:DI=%lX",10>,ebx,esi

        lea     esi,[ebx+esi-1]
        mov     eax,esi
        cmp     eax,ebx
        jb      mem5_err        ;size too large or 0
        and     bx,0f000h
        sub     esi,ebx
        add     esi,4095+1
        shr     esi,12          ;is always > 0
;
;addresses below 1 Meg won't be translated
;
        cmp     eax,100000h+10000h
        jc      mem5_8

        mov     edi,PageDirLinear

;Work out how many page tables we need
        mov     ecx,esi
        add     ecx,1024-1
        shr     ecx,10      ;ECX: size in pages -> size in 4MB chunks

;Search an already defined region
;        call    SearchRegion
;        jnc     mem5_8
;Search a free region
        call    SearchFreeRegion
        jc      mem5_noreg
        push    eax
        push    ecx
        or      d es:[edi+eax*4],1 shl PT_PBLKBIT
@@:
        or      d es:[edi+eax*4],1 shl PT_PRESBIT
        inc     eax
        loop    @B
        pop     ecx
        pop     edi
        shl     edi,12
        jmp     mem5_1
mem5_noreg:
;
;No free region found. Check if enough address space is free for a new one.
;
        mov     ax,[wP2LIndex] ;current bottom index of P2L mappings
        sub     eax,ecx
        jc      mem5_err    ;linear address space exhausted
        mov     edx,LinearLimit
        shr     edx,12+10
        cmp     ax,dx
        jbe     mem5_err    ;linear address space exhausted

        lea     edi,[edi+eax*4]
;
;Make sure we can get enough pages for PDEs
;
        call    PhysicalGetPages
        add     edx,UnLockedPages
        cmp     edx,ecx
        jc      mem5_err
;
;Put all the page tables into place.
;
        push    edi
mem5_2:                                 ;<--- next PDE
        push    ecx
        mov     edx,es:[edi]
        mov     ecx,edx
        shr     ecx,PT_VCPIBIT          ;shift VCPI bit to bit 0
        and     edx,edx                 ;PT unmapped?
        jnz     mem5_3
        call    PhysicalGetPage         ;get a free physical page in EDX
        jnc     mem5_3
        call    UnMapPhysical           ;get a mapped physical page (EDX, flags in CX)
        jc      mem5_err2               ;none, so "out of memory"
mem5_3:
        @dprintf DOPT_MEM,<"RawMapPhys2Lin, using PDE at %lX, EDX=%lX",10>,edi,edx
        and     ecx,1                   ;put user bits in useful place.
        shl     ecx,PT_VCPIBIT
        and     dx,0F000h               ;clear user bits.
        or      edx,111b                ;present+user+write.
        or      edx,ecx                 ;set use flags.
        mov     es:[edi],edx            ;store this tables address.
        push    edi
        sub     edi,PageDirLinear
        add     edi,PageAliasLinear     ;get alias table address.
        mov     es:[edi],edx            ;setup in alias table as well.
        pop     edi
        ;
        ;Clear this page to 0.
        ;
        push    edi
        sub     edi,PageDirLinear
        shl     edi,10
        add     edi,PTMAPADDR           ;base of page alias's.
        mov     ecx,4096/4
        xor     eax,eax
        cld
        rep     stosd
        pop     edi
        ;
        add     edi,4
        pop     ecx
        dec     ecx
        jnz     mem5_2
        pop     edi
        bts     d es:[edi],PT_PBLKBIT  ;mark block start
;
;Now map specified physical address range into place.
;ESI still holds size of region in pages.
;
        sub     edi,PageDirLinear
        mov     eax,edi
        shr     eax,2
        mov     [wP2LIndex],ax
        shl     edi,10          ;start of first page table
mem5_1:
        push    edi
        add     edi,PTMAPADDR
        or      ebx,111b        ;according to DPMI the page should not be "cacheable"
mem5_4:                         ;<--- next PTE
        mov     es:[edi],ebx
        add     edi,4
        add     ebx,4096
        dec     esi
        jnz     mem5_4
        pop     edi
;
;Return linear address to caller.
;
        shl     edi,12-2
mem5_8:
        pop     ebx
        pop     ecx
        and     di,0f000h
        and     cx,0fffh
        or      cx,di
        shr     edi,16
        mov     bx,di
        clc
        jmp     mem5_11
;
mem5_err2:
        pop     ecx
        pop     edi
mem5_err:
        @dprintf DOPT_MEM,<"RawMapPhys2Lin, error, EAX=%lX",10>,eax
        pop     ebx
        pop     ecx
        stc
mem5_11:
        pop     edi
        pop     esi
        pop     edx
        pop     eax
        pop     es
        pop     ds
        assume ds:nothing
        ret
RawMapPhys2Lin  endp


;-------------------------------------------------------------------------------
;
;Un-Do a physical to linear address mapping.
;DPMI function ax=0801h
;in: BX:CX = linear address
;
RawUnMapPhys2Lin proc near
        push    ds
        push    es
        push    eax
        push    edx
        push    ebx
        push    esi
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero
        mov     es,eax
        shl     ebx,16
        mov     bx,cx
        movzx   eax,[wP2LIndex]
        @dprintf DOPT_MEM,<"RawUnMapPhys2Lin enter, addr=%lX, wP2LIndex=%u",10>,ebx,eax
        shl     eax,10+12
        cmp     ebx,eax           ;below top of mapping stack?
        jc      error
        cmp     ebx,DETMAPADDR    ;details region = end of physical mappings
        jnc     error
        mov     esi,PageDirLinear
        shr     ebx,12+10;convert to PD index
        bt      d es:[esi+ebx*4],PT_PRESBIT
        jnc     error

;--- search start of region, then clear all page tables
;--- and reset present bit in PDEs
nextchk:
        test    d es:[esi+ebx*4],1 shl PT_PBLKBIT
        jnz     start_found
        dec     ebx
        jmp     nextchk
start_found:
        @dprintf DOPT_MEM,<"RawUnMapPhys2Lin, clearing PDE index=%u",10>,ebx
        pushad
        shl     ebx,10
        lea     edi,[ebx*4+PTMAPADDR]
        mov     ecx,1024
        xor     eax,eax
        cld
        rep     stosd    ;clear page table
        popad
        and     d es:[esi+ebx*4],not ((1 shl PT_PRESBIT) or (1 shl PT_PBLKBIT))
        inc     ebx
        cmp     bx,DETINDEX
        jz      @F
        mov     eax,es:[esi+ebx*4]
        and     ax,((1 shl PT_PRESBIT) or (1 shl PT_PBLKBIT))
        cmp     ax,1 shl PT_PRESBIT
        jz      start_found   ;block continues
@@:

;--- update wP2LIndex to free address space
;--- problem: address space manager ExtendLinearMemory() assumes
;--- that a valid PDE implies a valid entry for page Details -
;--- but physical mappings have no page details ...
        movzx   eax,[wP2LIndex]
checknextPDE:
        test    d es:[esi+eax*4],1 shl PT_PRESBIT
        jnz     @F
        inc     eax
        cmp     ax,DETINDEX
        jb      checknextPDE
@@:
        mov     [wP2LIndex],ax
        call    EmuCR3Flush
        @dprintf DOPT_MEM,<"RawUnMapPhys2Lin exit: wP2LIndex=%X",10>,eax
        clc
        jmp     done
error:
        stc
done:
        pop     esi
        pop     ebx
        pop     edx
        pop     eax
        pop     es
        pop     ds
        ret
RawUnMapPhys2Lin endp


;-------------------------------------------------------------------------------
;--- int 31h, ax=600h - lock linear region BX:CX, size SI:DI
;--- inside function, EBX = linear start, ESI = linear end

RawLockMemory   proc    near
        ;
        @dprintf DOPT_MEM,<"RawLockMemory enter, start=%X:%04X size=%X:%04X",10>,ebx,ecx,esi,edi
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
;
        call    RawSwitchStack
        assume ds:GROUP16
        ;
        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        add     esi,ebx
        cmp     esi,ebx
        jbe     mem7_done               ;if end <= start (size 0 or base+size overflows)
        and     bx,0f000h               ;round down to nearest page.
;        mov     [_LM_BlockBase],ebx
;        mov     [_LM_BlockEnd],esi
        ;
        ;Count number of pages we need for this range.
        ;
;        mov     [_LM_Needed],0
        mov     edi,0
        mov     eax,UnLockedPages
        mov     [_LM_Got],eax
        mov     eax,ebx
mem7_04:                        ;<---- next page
        cmp     eax,LinearBase  ;must be in our memory pool.
        jc      mem7_05
        cmp     eax,LinearLimit
        jnc     mem7_05
        call    GetPageStatus
        jc      mem7_err        ;error if page table not present
        test    edx,1 shl PT_PRESBIT
        jz      mem7_005
        call    IsPageLocked    ;locked page? Z if lock cnt==0
        jnz     mem7_05
        dec     [_LM_Got]       ;reduce available pages.
        jmp     mem7_05
mem7_005:
;        inc     [_LM_Needed]
        inc     edi
mem7_05:
        add     eax,4096
        cmp     eax,esi         ;done them all yet?
        jb      mem7_04         ;---->
ifdef _DEBUG
        mov     eax,UnLockedPages
        shl     eax,12
endif
        @dprintf DOPT_MEM,<"RawLockMemory UnLockedPages=%lu (space=%lX), Needed=%lu, Got=%lu",10>,UnLockedPages,eax,edi,_LM_Got
;
;Check if we actually need any more pages to lock this region.
;
;        cmp     [_LM_Needed],0
        cmp     edi,0
        jz      mem7_OK
;        mov     eax,[_LM_Needed]
        mov     eax,edi
;
;If VMM isn't active then pages can always be locked assumeing they exist.
;
        cmp     VMMHandle,0
        jz      @F
;
;VMM is active and pages are required so we need to make sure enough pages are
;left for swapping.
;
        add     eax,16          ;arbitrary safety buffer.
@@:
        cmp     eax,[_LM_Got]
        ja      mem7_err
;
;Enough free pages so lock the region.
;
mem7_OK:
        mov     eax,ebx
mem7_4:                         ;<---- next page
        cmp     eax,LinearBase  ;must be in our memory pool.
        jc      mem7_5
        cmp     eax,LinearLimit
        jnc     mem7_5
        call    GetPageStatus
        jc      mem7_err        ;error if page table isn't present (shouldn't happen)
        test    edx,1 shl PT_PRESBIT ;is page present?
        jnz     mem7_6
        ;
        ;Need to allocate a physical page first.
        ;
        call    UnMapPhysical   ;get physical page in EDX
        jc      mem7_err        ;this shouldn't happen.
        mov     LinearEntry,eax
        shr     LinearEntry,12  ;store page number to allocate at.
        call    MapPhysical     ;map this page in.
mem7_6:
        call    LockPage        ;lock page with eax=linear address
mem7_5:
        add     eax,4096
        cmp     eax,esi         ;done them all yet?
        jb      mem7_4          ;---->
        ;
mem7_10:
        clc
        jmp     mem7_done
        ;
mem7_err:
        @dprintf DOPT_MEM,<"RawLockMemory: error, eax=%lX, edi=%lX, lm_got=%lu",10>,eax,edi,[_LM_Got]
        stc
        ;
mem7_done:
        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
RawLockMemory   endp


;-------------------------------------------------------------------------------
;--- int 31h, ax=601h - unlock linear region BX:CX, size SI:DI

RawUnLockMemory proc near
        ;
        @dprintf DOPT_MEM,<"RawUnLockMemory enter, start=%X:%04X, size=%X:%04X",10>,ebx,ecx,esi,edi
        push    eax
        push    ebx
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ds
        push    es
        ;
        call    RawSwitchStack
        assume ds:GROUP16

        shl     ebx,16
        mov     bx,cx
        shl     esi,16
        mov     si,di
        add     esi,ebx
        cmp     esi,ebx
        jbe     mem8_9          ;if end <= start (size 0 or base+size overflows)
        and     bx,0F000h       ;round down to nearest page.
;        mov     [_LM_BlockBase],ebx
;        mov     [_LM_BlockEnd],esi
        ;
        ;Now run through all pages in this range un-locking them.
        ;
        mov     eax,ebx
mem8_4:                         ;<---- next page
        cmp     eax,LinearBase  ;must be in our memory pool.
        jc      mem8_5
        cmp     eax,LinearLimit
        jnc     mem8_5
        call    GetPageStatus
        jc      mem8_5          ;ignore not present tables (shouldn't happen)
        test    edx,1 shl PT_PRESBIT ;is it present?
        jz      mem8_5
        call    IsPageLocked
        jz      mem8_5
        call    UnLockPage      ;unlock the page.
mem8_5:
        add     eax,4096
        cmp     eax,esi
        jb      mem8_4          ;---->
        clc
mem8_9:
        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     ebx
        pop     eax
        ret
RawUnLockMemory endp


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
RawGetMemoryMax proc near
;
;called by DPMI emulation ax=500h
;Work out biggest memory block remaining.
;out: ebx=max memory
;
        ;
        @dprintf DOPT_MEM or DOPT_STACK,<"RawGetMemoryMax enter, ss:esp=%X:%lX",10>,ss,esp
        push    eax
        push    ecx
        push    edx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
;
        call    RawSwitchStack
        assume ds:GROUP16

        mov     ax,KernalZero
        mov     es,eax
        ;
        ;Look in existing memory first.
        ;
        mov     edx,LinearBase  ;Get starting point.
        mov     ecx,LinearLimit
        sub     ecx,edx         ;Get memory present.
        shr     ecx,12          ;Get pages.
        shr     edx,12          ;Get page number.
        xor     edi,edi         ;Clear flag.
        xor     ebp,ebp         ;Clear biggest so far.
        ;
mem9_l0:
        ;Look for a bigest block of free memory.
        ;
        mov     eax,es:[edx*4+DETMAPADDR] ;Get page details.
        and     eax,MEM_MASK
        cmp     eax,MEM_FREE            ;Free block?
        jnz     mem9_l2
        or      edi,edi         ;Got any yet?
        jnz     mem9_l1
        mov     esi,edx         ;Get base page number.
mem9_l1:
        inc     edi
        cmp     edi,ebp         ;Biggest yet?
        jc      mem9_l3
        mov     ebx,esi         ;Get base.
        mov     ebp,edi         ;Get size.
        jmp     mem9_l3
mem9_l2:
        xor     edi,edi
mem9_l3:
        inc     edx             ;Next page.
        loop    mem9_l0
        ;
        ;See if biggest block found is the last block in the map.
        ;
        xor     edx,edx         ;reset end of chain value.
        or      edi,edi         ;last block free?
        jz      mem9_l4
        cmp     ebx,esi         ;same base?
        jnz     mem9_l4
        mov     edx,ebp         ;setup extra block size.
        mov     ebp,0           ;reset normal block size.
        jmp     mem9_l5
        ;
mem9_l4:
        ;Get size of the last block in the memory map.
        ;
        mov     eax,LinearBase
        mov     esi,LinearLimit
        shr     esi,12
        dec     esi
        shr     eax,12
        mov     ecx,esi
        sub     ecx,eax
mem9_l6:
        jecxz   mem9_l5
        mov     eax,es:[esi*4+DETMAPADDR] ;Get page details.
        and     eax,MEM_MASK
        cmp     eax,MEM_FREE            ;Free block?
        jnz     mem9_l5
        dec     esi
        inc     edx
        loop    mem9_l6
        ;
mem9_l5:
        ;See what extra memory we can get hold of.
        ;
        mov     ebx,edx
        call    PhysicalGetPages        ;Get extra memory available.
        mov     ecx,ebx         ;Save original value.
        add     ebx,edx         ;update extra memory value.
        ;
        ;See how many pages of real memory would be lost to page tables.
        ;
        mov     eax,LinearLimit
        shr     eax,12
        add     eax,edx
        shr     eax,10
        mov     edx,LinearLimit
        shr     edx,12
        dec     edx
        shr     edx,10
        sub     eax,edx
        add     eax,eax         ;Page + Det
        sub     ebx,eax
        ;
        ;See what extra memory the VMM can get hold of.
        ;
        cmp     VMMHandle,0
        jz      mem9_l8

        mov     esi,SDAaddress
        cmp     es:[esi].SDA.bInDos,0
        jnz     mem9_l8

        mov     ebx,ecx
        push    ebx
        push    ebp
        ;
        ;Get free disk space remaining.
        ;
        mov     dl,VMMName      ;get drive letter for this media.
        sub     dl,'A'          ;make it real.
        inc     dl              ;adjust for current type select.
        mov     ah,36h          ;get free space.
        int     21h             ;/
        xor     edx,edx
        cmp     ax,-1           ;invalid drive?
        jz      mem9_l7
        mul     cx              ;Get bytes per cluster.
        mul     bx              ;Get bytes available.
        shl     edx,16
        mov     dx,ax
mem9_l7:
        ;
        ;Get current swap file size.
        ;
        push    edx
        mov     bx,VMMHandle
        mov     ax,4202h
        xor     cx,cx
        mov     dx,cx
        int     21h
        shl     edx,16
        mov     dx,ax
        pop     eax
        add     edx,eax
;       and     edx,not 65535    ;clear DX
        xor     dx,dx 
        shr     edx,12
        pop     ebp
        pop     ebx
        ;
        ;Work out how much of the VMM space is extra.
        ;
        mov     eax,LinearLimit
        sub     eax,LinearBase
        shr     eax,12
        sub     edx,eax
        add     ebx,edx

        ;
mem9_l8:
        ;Check which block is bigger and exit.
        ;
        push    ecx
        mov     eax,ebx
        shl     eax,12
        mov     ecx,LinearLimit
        sub     ecx,LinearBase
        sub     eax,ecx
        js      mem9_l89
        cmp     eax,MaxMemLin
        jc      mem9_l89
        mov     ebx,MaxMemLin
        sub     ebx,ecx
        shr     ebx,12
mem9_l89:
        pop     ecx

        cmp     ebx,ebp
        jnc     mem9_l9
        mov     ebx,ebp
mem9_l9:
        shl     ebx,12
        clc

        @RawRestoreStack eax
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     edx
        pop     ecx
        pop     eax
        ret
RawGetMemoryMax endp


;-------------------------------------------------------------------------------
;
;Extend linear address space by allocating physical memory if available or
;virtual memory if not, or even a combination of the two.
;Called by RawGetMemory & RawResizeMemory.
;
;On Entry:
;
;ECX    - Pages to extend by.
; DS    - GROUP16
; ES    - FLAT
;
;On Exit:
;
;Carry set on error else memory map extended.
;modified [LinearLimit], [TotalPages], [LinearEntry]
;
;This function may call int 21h, ah=36h.
;
ExtendLinearMemory proc near
        pushad
;        push    ds
        push    es
;        mov     ax,KernalDS
;        mov     ds,eax
        assume ds:GROUP16
;        mov     ax,KernalZero
;        mov     es,eax
        mov     ebp,ecx

        mov     eax,ecx
        shl     eax,12
        dec     eax
        add     eax,LinearLimit

;--- overflow (C set) should be an error;
;--- it may happen if the amount to allocate and the amount already allocated
;--- exceed 4 GB

        jc      mem10_error

        dec     eax
        sub     eax,LinearBase
        cmp     eax,MaxMemLin   ;MAXMEM setting (default 1021 shl 20)
        jnc     mem10_error

;--- check if lower limit of Phys2linear mappings is reached;
;--- then address space is exhausted as well.
        mov     eax,LinearLimit
        shr     eax,12
        add     eax,ecx
        shr     eax,10   ;convert to PDE index
        cmp     ax,[wP2LIndex]
        jae     mem10_error
        ;
        ;Try extending using physical memory first.
        ;
mem10_f0:                               ;<---- next page
        mov     eax,LinearLimit         ;get new entry number.
        shr     eax,12                  ;page number.
        mov     LinearEntry,eax
        shr     eax,10                  ;/1024 for page dir entry.
        mov     edi,PageDirLinear       ;get page table address.
        mov     eax,es:[edi+eax*4]      ;get PDE
        test    eax,1                   ;page table present?
        jnz     mem10_f1                ;if yes
        call    PhysicalGetPages        ;get free physical pages in EDX
if 0
;
;--- v5.0: The PDE may have been allocated for physical region mappings.
;---       Then the PDE is != 0, and there's no details page yet!
;
        and     eax,eax
        jz      @F
        cmp     edx,0
        jz      mem10_Virtual
        call    PhysicalGetPage ;get a page.
        jc      mem10_error     ;it lied.
        call    MapPhysical     ;use this page for page details
        or      d es:[edi+eax*4],1 shl PT_PRESBIT
        jmp     mem10_f1
@@:
endif
;No page table so make sure we can get 2 pages (page and det)
        cmp     edx,2
        jc      mem10_Virtual
;
;both pages available so go to it.
;
;mem10_f2:
        call    PhysicalGetPage ;get a page.
        jc      mem10_error     ;it lied.
        mov     eax,LinearLimit ;get new entry number.
        shr     eax,12          ;page number.
        mov     LinearEntry,eax
        push    eax
        call    MapPhysical     ;use this page as page table
        call    PhysicalGetPage ;get a page.
        pop     LinearEntry
        jc      mem10_error     ;it lied.
        call    MapPhysical     ;use this page as page table for page DETails
        ;
mem10_f1:
        call    PhysicalGetPage ;get a page.
        jc      mem10_Virtual   ;use virtual memory.
        mov     eax,LinearLimit ;get new entry number.
        shr     eax,12          ;page number.
        mov     LinearEntry,eax
        call    MapPhysical     ;use this page for page table.
        ;
        ;Update details.
        ;
        mov     eax,LinearLimit
        shr     eax,12
        mov     d es:[eax*4+DETMAPADDR],0
        inc     FreePages
;       dec     AllocedPages
        inc     TotalPages
        add     LinearLimit,4096;bump up the end of the memory map.
        dec     ebp             ;update the counter.
        jnz     mem10_f0        ;---->
        clc
        jmp     mem10_Exit      ;All physical so exit.
        ;
mem10_Virtual:
        ;Virtual memory will be needed so see if we can provide any.
        ;
        cmp     VMMHandle,0     ;Virtual memory active?
        jz      mem10_error
        ;
        ;Find out how much disk space is left for swap file to grow into.
        ;
        mov     dl,VMMName      ;get drive letter for this media.
        sub     dl,'A'          ;make it real.
        inc     dl              ;adjust for current type select.
        mov     ah,36h          ;get free space.
        push    ebp
        int     21h             ;/
        pop     ebp
        cmp     ax,-1           ;invalid drive?
        jz      mem10_error
        mul     cx              ;Get bytes per cluster.
        mul     bx              ;Get bytes available.
        shl     edx,16
        mov     dx,ax           ;make 32 bit.
;       and     edx,not 65535   ;round down to 64kb
        xor     dx,dx
        add     edx,SwapFileLength ;add existing size.
        mov     eax,LinearLimit
        sub     eax,LinearBase  ;get current real memory.
        sub     edx,eax
        shr     edx,12          ;get it as pages.
        cmp     edx,ebp         ;Enough pages?
        jc      mem10_error
        ;
        ;Find out how many un-locked pages we currently have.
        ;
        mov     edx,UnLockedPages
        ;
        ;Enough page's for minimum requirement?
        ;
        cmp     edx,16          ;un-locked pages < 16?
        jc      mem10_error     ;force minimum of 16.
        sub     edx,16
        ;
        ;Work out how many new page/det tables are required.
        ;
        mov     eax,LinearLimit
        shr     eax,12
        mov     ebx,eax
        dec     eax             ;Last definatly valid page table.
        add     ebx,ebp         ;New end page.
        shr     eax,10
        shr     ebx,10          ;Get page table units (4Meg)
        sub     ebx,eax         ;get the differance.
        jz      mem10_DoneTables
        add     ebx,ebx         ;Page + Det
        mov     ecx,ebp
        cmp     ebx,edx
        jc      mem10_OK
        jz      mem10_OK
        jmp     mem10_error
        ;
mem10_OK:
        ;Allocate new page tables.
        ;
        mov     esi,PageDirLinear ;get page directory address.
        mov     edx,LinearLimit
        shr     edx,12
mem10_v2:
        mov     eax,edx         ;get new entry number.
        shr     eax,10          ;/1024 for page dir entry.
        test    d es:[esi+eax*4],1 shl PT_PRESBIT;this page present?
        jnz     mem10_v3
        ;
        ;get DET page.
        ;
        push    ecx
        push    edx
        call    UnMapPhysical   ;get a physical page in EDX, flags in CL
        mov     eax,edx
        mov     ebx,ecx
        pop     edx
        pop     ecx
        jc      mem10_error     ;not enough physical memory to support virtual memory.
        push    ecx
        push    edx
        mov     LinearEntry,edx ;set logical page address.
        mov     edx,eax         ;get physical address again.
        mov     ecx,ebx
        call    MapPhysical     ;use this to add a new page table.
        pop     edx
        pop     ecx
        ;
        ;And again for page table.
        ;
        push    ecx
        push    edx
        call    UnMapPhysical   ;get a physical page in EDX, flags in CL
        mov     eax,edx
        mov     ebx,ecx
        pop     edx
        pop     ecx
        jc      mem10_error     ;not enough physical memory to support virtual memory.
        push    ecx
        push    edx
        mov     LinearEntry,edx ;set logical page address.
        mov     edx,eax         ;get physical address again.
        mov     ecx,ebx
        call    MapPhysical     ;use this to add a new page table.
        pop     edx
        pop     ecx
mem10_v3:
        inc     edx
        dec     ecx
        jnz     mem10_v2

mem10_DoneTables:
        ;
        ;Now mark all the new pages as un-locked/free
        ;
        mov     ecx,ebp
        mov     edi,LinearLimit
        shr     edi,12
        lea     edi,[edi*4+DETMAPADDR]
        xor     eax,eax
        cld
        rep     stosd
        ;
        ;Extend the swap file.
        ;
        mov     ecx,LinearLimit ;current end position.
        sub     ecx,LinearBase  ;length.
        mov     eax,ebp         ;extension needed in pages.
        shl     eax,12
        add     ecx,eax         ;New extremity desired.
        cmp     ecx,SwapFileLength
        jc      mem10_Extended
        add     ecx,65535
;       and     ecx,not 65535
        xor     cx,cx
        push    ecx
        mov     dx,cx
        shr     ecx,16
        mov     bx,VMMHandle    ;get swap file handle.
        mov     ax,4200h
        mov     edi,offset PageInt
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        push    ds
        pop     es
        call    RawSimulateInt  ;move to right place.
        mov     ah,40h
        mov     bx,VMMHandle    ;get swap file handle.
        xor     ecx,ecx         ;write 0 bytes
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     RealRegsStruc.Real_EDX[edi],edx  ;DS:DX doesn't matter if size == 0
        mov     bl,21h
        call    RawSimulateInt  ;move to right place.
        pop     ecx
        test    RealRegsStruc.Real_FlagsL[edi],1
        jnz     mem10_Disk_Error
        mov     SwapFileLength,ecx
        ;
        xor     cx,cx
        xor     dx,dx
        mov     ax,4201h
        mov     bx,VMMHandle
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     bl,21h
        call    RawSimulateInt  ;move to right place.
        test    RealRegsStruc.Real_FlagsL[edi],1
        jnz     mem10_Disk_Error
        mov     edx,RealRegsStruc.Real_EDX[edi]
        mov     eax,RealRegsStruc.Real_EAX[edi]
        shl     edx,16
        mov     dx,ax
        cmp     edx,SwapFileLength
        jnz     mem10_Disk_Error
        ;
mem10_Extended:
        ;Update the end of the memory map.
        ;
        add     FreePages,ebp
;       sub     AllocedPages,ebp
        shl     ebp,12
        add     LinearLimit,ebp
        clc
        jmp     mem10_Exit
        ;
mem10_Disk_Error:
;        jmp     mem10_error
mem10_error:
        stc
mem10_Exit:
        pop     es
;        pop     ds
        assume ds:nothing
        popad
        ret
        ;
ExtendLinearMemory endp


;-------------------------------------------------------------------------------
;
;Map physical page supplied into logical address space. If a new page table is needed, the page
;supplied will become a page table.
;called by ExtendLinearMemory(), RawLockMemory() and VirtualFault()
;
;On Entry:
;LinearEntry - linear index ( linear address >> 12 )
;CL          - Use flags 0-3. 1 being used for VCPI memory to allow release later.
;EDX         - Physical address to map.
;
;On Exit:
;LinearEntry - incremented if a PTE was written (not for PDEs or Page details)
;
;all registers preserved.
;
MapPhysical     proc    near
        pushad
        push    ds
        push    es
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     ax,KernalZero   ;make everything addresable.
        mov     es,eax
;        @dprintf DOPT_MEM,<"MapPhysical: LinearEntry=%lX, edx=%lX",10>,LinearEntry, edx
        ;
        and     ecx,1           ;put user bits in useful place.
        shl     ecx,PT_VCPIBIT
        and     dx,0F000h       ;lose user bits.
        mov     eax,LinearEntry ;get new entry number.
        shr     eax,10          ;/1024 for page dir entry.
        ;
        mov     esi,PageDirLinear ;get page directory
        test    d es:[esi+eax*4],1 shl PT_PRESBIT;PDE for the page present?
        jnz     mem11_AddPage   ;if yes
        ;
        mov     esi,PageDETLinear ;get page details
        cmp     esi,0           ;DET in use yet? (should always be != NULL)
        jz      mem11_AddTable
        test    d es:[esi+eax*4],1 shl PT_PRESBIT;DET page present?
        jnz     mem11_AddTable
        ;
;mem11_AddDET:
        ;Need a new DET page.
        ;
        or      edx,111b              ;present+user+write.
        or      edx,ecx               ;set use flags.
        mov     es:[esi+eax*4],edx    ;store this tables address.
        ;
        ;Clear this page to locked.
        ;
        mov     eax,LinearEntry       ;get the entry number again.
        lea     edi,[eax*4+DETMAPADDR]
        mov     ecx,4096/4
        mov     eax,MEM_FILL
        cld
        rep     stosd
        ;
        jmp     mem11_Finished
        ;
mem11_AddTable:
        ;Need a new page table.
        ;
        mov     eax,LinearEntry         ;get new entry number.
        shr     eax,10                  ;/1024 for page dir entry.
        mov     esi,PageDirLinear       ;get page table address.
        or      edx,111b                ;present+user+write.
        or      edx,ecx                 ;set use flags.
        mov     es:[esi+eax*4],edx      ;store this tables address.
        mov     esi,PageAliasLinear     ;get alias table address.
        mov     es:[esi+eax*4],edx      ;setup in alias table as well.
        ;
        ;Clear this page to 0.
        ;
        mov     eax,LinearEntry         ;get the entry number again.
        lea     edi,[eax*4+PTMAPADDR]
        mov     ecx,4096/4
        xor     eax,eax
        cld
        rep     stosd
        ;
        jmp     mem11_Finished
        ;
mem11_AddPage:
        ;Update recent page usage stack.
        ;
;        @dprintf DOPT_MEM,<"MapPhysical: AddPage",10>
        push    ecx
        push    es
        push    ds
        pop     es
        mov     edi,offset RecentMapStack + 4*PAGESTACKSIZE - 4
;        mov     esi,offset RecentMapStack + 4*PAGESTACKSIZE - 8
        lea     esi,[edi-4]
        mov     ecx,PAGESTACKSIZE - 1
        mov     eax,LinearEntry
        shl     eax,12
        std
        rep     movsd
        stosd
        pop     es
        pop     ecx
        cld
        ;
        ;Add this to the relevant page table.
        ;
        mov     eax,LinearEntry       ;get the entry number again.
        mov     esi,PTMAPADDR         ;base of page alias's.
        mov     ebx,es:[esi+eax*4]    ;get current details.
        and     ebx,1 shl PT_SWAPBIT
        or      edx,ebx
        or      edx,111b              ;present+user+write.
        or      edx,ecx               ;set use flags.
        mov     es:[esi+eax*4],edx    ;write PTE
        ;
        cmp     PageDETLinear,0
        jz      mem11_NoLocking
;        mov     eax,LinearEntry      ;eax still holds LinearEntry
;        shl     eax,12
;        call    RawClearPageLock      ;clear page locking for this entry.
        and     d es:[eax*4+DETMAPADDR],not (MEM_LOCK_MASK shl MEM_LOCK_SHIFT)    ;un-lock it.
        ;
        ;Update number of un-locked physical pages present.
        ;
        inc     UnLockedPages
        ;
mem11_NoLocking:
        ;Check if this page needs fetching from swap space.
        ;
        cmp     VMMHandle,0
        jz      mem11_NoRead
        ;
        test    ebx,1 shl PT_SWAPBIT
        jz      mem11_NoRead
        ;
        mov     esi,SDAaddress

;--- added for v5.0: no DOS access if indos flag is set
        cmp     es:[esi].SDA.bInDos,0
        jnz     mem11_NoRead

        mov     al,es:[esi].SDA.bBreak
        mov     es:[esi].SDA.bBreak,0
        push    eax
        ;
        mov     ecx,LinearEntry ;get page number.
        shl     ecx,12          ;get linear address.
        sub     ecx,LinearBase
        mov     dx,cx
        shr     ecx,16
        mov     bx,VMMHandle
        mov     ax,4200h
        mov     edi,offset PageInt
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        push    es
        push    ds
        pop     es
        call    RawSimulateInt          ;move to right place.
        mov     edi,offset PageInt
        mov     ax,VMMHandle
        mov     RealRegsStruc.Real_EBX[edi],eax
        mov     ax,PageBufferReal
        mov     RealRegsStruc.Real_DS[edi],ax
        mov     RealRegsStruc.Real_AX[edi],3f00h
        mov     RealRegsStruc.Real_DX[edi],0
        mov     RealRegsStruc.Real_CX[edi],4096
;        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt          ;read it from disk.
        pop     es
        mov     esi,SDAaddress
        pop     eax
        mov     es:[esi].SDA.bBreak,al
        test    RealRegsStruc.Real_FlagsL[edi],1
        jz      mem11_ok
;--- Set STC here? Actually, the carry flag is never checked after a call of MapPhysical
        jmp     mem11_Finished2
        ;
mem11_ok:
        ;
        mov     esi,PageBufferLinear
        mov     edi,LinearEntry
        shl     edi,12          ;get linear address again.
        mov     ecx,4096/4
        push    ds
        push    es
        pop     ds
        cld
        rep     movsd           ;copy back into place.
        pop     ds
        ;
        mov     eax,LinearEntry ;get new entry number.
        mov     esi,PTMAPADDR   ;base of page alias's.
        and     d es:[esi+eax*4],not (3 shl 5) ;clear accessed & dirty bits.
        call    EmuCR3Flush
        ;
mem11_NoRead:
        inc     LinearEntry     ;update counter.
        ;
mem11_Finished:
        clc
        ;
mem11_Finished2:
        pop     es
        pop     ds
        assume ds:nothing
        popad
        ret
MapPhysical     endp


;-------------------------------------------------------------------------------
;
;Retrieve the address of the most un-needed physical memory and mark its current page table entry
;as not present after writing to disk if needed.
;
;On Entry:-
;
;On Exit:-
;
;EDX    - Physical address of page.
;CL     - User flags.
;EAX,EBX,ESI,EDI,EBP unchanged.
;
UnMapPhysical   proc    near
        push    eax
        push    ebx
        push    esi
        push    edi
        push    ebp
        push    ds
        push    es
        push    fs
        ;
        mov     ax,KernalDS
        mov     ds,eax
        assume ds:GROUP16
        mov     es,eax
        mov     ax,KernalZero
        mov     fs,eax
        ;
        mov     ProposedPresentFlag,0
        mov     ProposedRecentFlag,0
        mov     CompareCount,0
        ;
        mov     esi,PageDirLinear
        mov     edi,PTMAPADDR           ;base of page table mappings
        mov     ebp,DETMAPADDR
        mov     ecx,PageingPointer      ;get current position.
        mov     ebx,LinearLimit         ;maximum size of scan we can do.
        sub     ebx,LinearBase
        shr     ebx,12
        inc     ebx
        cld
        ;
mem12_ScanLoop:
        dec     ebx
        jnz     mem12_80                ;shit, we've been all the way round.
        ;
        cmp     ProposedPresentFlag,0
        jnz     mem12_UseProposed
        jmp     mem12_8
        ;
mem12_80:
        cmp     ecx,LinearBase
        jnc     mem12_80_0
        mov     ecx,LinearBase
        sub     ecx,4096
mem12_80_0:
        add     ecx,4096
        cmp     ecx,LinearLimit ;End of memory map yet?
        jc      mem12_NoWrap
        mov     ecx,LinearBase
        ;
mem12_NoWrap:
        mov     eax,ecx
        shr     eax,12          ;get page number.
        test    d fs:[edi+eax*4],1 shl PT_PRESBIT ;page present?
        jz      mem12_ScanLoop
        test    d fs:[ebp+eax*4],MEM_LOCK_MASK shl MEM_LOCK_SHIFT
        jnz     mem12_ScanLoop
        ;
        inc     CompareCount
        ;
        ;Check against recent stack.
        ;
        push    ecx
        push    edi
        mov     eax,ecx
        mov     edi,offset RecentMapStack
        mov     ecx,PAGESTACKSIZE
        repnz   scasd
        pop     edi
        pop     ecx
        jz      mem12_IsRecent
        shr     eax,12          ;get page number again.
        test    d fs:[edi+eax*4],1 shl PT_DIRTYBIT
        jz      mem12_GotPage
        ;
        cmp     ProposedPresentFlag,0
        jz      mem12_SetProposed
        cmp     ProposedRecentFlag,0
        jz      mem12_UseProposed?
mem12_SetProposed:
        mov     ProposedPresentFlag,-1
        mov     ProposedPage,ecx
        mov     CompareCount,0
        mov     ProposedRecentFlag,0
        jmp     mem12_ScanLoop
        ;
mem12_UseProposed?:
        mov     eax,UnLockedPages
        shr     eax,2
        cmp     eax,4096
        jc      mem12_UP0
        mov     eax,4096
mem12_UP0:
        cmp     CompareCount,eax
        jc      mem12_UseProposed
        jmp     mem12_ScanLoop
        ;
mem12_IsRecent:
        cmp     ProposedPresentFlag,0
        jnz     mem12_ProposedRecent?
        mov     ProposedPresentFlag,-1
        mov     ProposedPage,ecx
        mov     ProposedRecentFlag,-1
        mov     CompareCount,0
        jmp     mem12_ScanLoop
        ;
mem12_ProposedRecent?:
        cmp     ProposedRecentFlag,0
        jnz     mem12_LookedEnough?
        mov     eax,UnLockedPages
        shr     eax,2
        cmp     eax,4096
        jc      mem12_PR0
        mov     eax,4096
mem12_PR0:
        cmp     CompareCount,eax
        jnc     mem12_UseProposed
        jmp     mem12_ScanLoop
        ;
mem12_LookedEnough?:
        mov     eax,UnLockedPages
        cmp     CompareCount,eax
        jnc     mem12_UseProposed
        jmp     mem12_ScanLoop
        ;
mem12_UseProposed:
        mov     ecx,ProposedPage
        ;
mem12_GotPage:
        mov     PageingPointer,ecx
        ;
        mov     eax,ecx
        shr     eax,12          ;get page number again.
        shl     eax,2
        add     edi,eax
        ;
        ;Check if it needs to go to the swap file.
        ;
        test    d fs:[edi],1 shl PT_DIRTYBIT   ;is it dirty?
        jz      mem12_5         ;no need to write it if not.
        ;
        ;Flush this page to disk.
        ;
        mov     esi,SDAaddress

;--- v5.0: no DOS access if indos flag is set
        cmp     fs:[esi].SDA.bInDos,0
        jnz     mem12_8

        push    edi
        mov     al,fs:[esi].SDA.bBreak
        mov     fs:[esi].SDA.bBreak,0
        push    eax
        ;
        sub     edi,PTMAPADDR   ;get page table entry number.
        shr     edi,2           ;page number.
        shl     edi,12          ;get linear address.
        push    edi
        mov     esi,edi
        push    ds
        push    es
        mov     edi,PageBufferLinear    ;copy it to somewhere we can deal with it.
        mov     ax,KernalZero
        mov     ds,eax
        mov     es,eax
        mov     ecx,4096/4
        rep     movsd
        pop     ds
        pop     es
        pop     edi
        ;
        sub     edi,LinearBase
        mov     ebp,edi
        mov     dx,di
        shr     edi,16
        mov     cx,di
        mov     ax,4200h
        mov     bx,VMMHandle
        mov     edi,offset PageInt
        mov     RealRegsStruc.Real_EBX[edi],ebx
        mov     RealRegsStruc.Real_EDX[edi],edx
        mov     RealRegsStruc.Real_ECX[edi],ecx
        mov     RealRegsStruc.Real_EAX[edi],eax
        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        push    ds
        pop     es
        call    RawSimulateInt          ;move to right place.
        ;
        test    RealRegsStruc.Real_FlagsL[edi],1
        stc
        jnz     mem12_error_anyway
        mov     edx,RealRegsStruc.Real_EDX[edi]
        mov     eax,RealRegsStruc.Real_EAX[edi]
        shl     edx,16
        mov     dx,ax
        cmp     edx,ebp
        jnz     mem12_force_error
        ;
;        mov     edi,offset PageInt
        mov     ax,VMMHandle
        mov     RealRegsStruc.Real_EBX[edi],eax
        mov     ax,PageBufferReal
        mov     RealRegsStruc.Real_DS[edi],ax
        mov     RealRegsStruc.Real_AX[edi],4000h
        mov     RealRegsStruc.Real_DX[edi],0
        mov     RealRegsStruc.Real_CX[edi],4096
;        mov     RealRegsStruc.Real_SSSP[edi],0
        mov     bl,21h
        call    RawSimulateInt          ;write it to disk.
        test    RealRegsStruc.Real_FlagsL[edi],1
        stc
        jnz     mem12_error_anyway
        mov     eax,RealRegsStruc.Real_EAX[edi]
        cmp     ax,4096
        jz      mem12_error_anyway
mem12_force_error:
        stc
mem12_error_anyway:
        mov     esi,SDAaddress
        pop     eax
        mov     fs:[esi].SDA.bBreak,al
;mem12_error_indos:
        ;
        pop     edi
        jc      mem12_8
        or      d fs:[edi],1 shl PT_SWAPBIT     ;signal it living on disk.
        ;
mem12_5:
        ;Now remove it from the page table and exit.
        ;
        and     d fs:[edi],not (1 shl PT_PRESBIT);mark as not present.
        mov     edx,fs:[edi]            ;get page entry.
        mov     ecx,edx
;        and     edx,0FFFFFFFFh-4095     ;lose flag bits.
        and     dx,0F000h       ;lose flag bits.
        shr     ecx,PT_VCPIBIT
        and     ecx,1           ;preserve user flags.
        call    EmuCR3Flush
        ;
        ;Update number of un-locked physical pages present.
        ;
        dec     UnLockedPages
        ;
        clc
        jmp     mem12_9
        ;
mem12_8:
        stc                     ;failed to find free page.
        ;
mem12_9:
        pop     fs
        pop     es
        pop     ds
        assume ds:nothing
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
        pop     eax
        ret
UnMapPhysical   endp


;-------------------------------------------------------------------------------
;
;Lock page indicated.
;
;On Entry:-
;
;EAX    - Linear address to lock.
;
LockPage     proc    near
        assume ds:GROUP16
        push    eax
        push    ebx
        push    esi
        push    es
        mov     bx,KernalZero
        mov     esi,DETMAPADDR  ;base of page details
        shr     eax,12          ;get page number.
        mov     es,ebx
        mov     ebx,es:[esi+eax*4]
        shr     ebx,MEM_LOCK_SHIFT
        and     ebx,MEM_LOCK_MASK
        jnz     mem13_WasLocked
        ;
        ;Update number of un-locked physical pages present.
        ;
        dec     UnLockedPages
        ;
mem13_WasLocked:
        cmp     ebx,MEM_LOCK_MASK
        jz      mem13_0
        add     d es:[esi+eax*4],1 shl MEM_LOCK_SHIFT   ;lock it.
mem13_0:
        pop     es
        pop     esi
        pop     ebx
        pop     eax
        ret
        assume ds:nothing
LockPage     endp


;-------------------------------------------------------------------------------
;
;Un-lock a linear page.
;called by RawUnlockMemory
;
;On Entry:-
;DS = GROUP16
;
;EAX    - Linear address of page to unlock.
;
UnLockPage   proc    near
        assume ds:GROUP16
        push    eax
        push    ebx
        push    esi
        push    es
        mov     bx,KernalZero
        mov     esi,DETMAPADDR     ;base of page details
        shr     eax,12
        mov     es,ebx
        sub     d es:[esi+eax*4],1 shl MEM_LOCK_SHIFT   ;un-lock it.
        mov     eax,es:[esi+eax*4]
        shr     eax,MEM_LOCK_SHIFT
        and     eax,MEM_LOCK_MASK
        jnz     mem15_NotUnLocked
        ;
        ;Update number of un-locked physical pages present.
        ;
        inc     UnLockedPages
        ;
mem15_NotUnLocked:
        pop     es
        pop     esi
        pop     ebx
        pop     eax
        ret
        assume ds:nothing
UnLockPage   endp


;-------------------------------------------------------------------------------
;
;Check if page lock count is zero.
;
;On Entry:-
;
;EAX    - Linear page address.
;
;On Exit:-
;
;Zero set if page count=0.
;
IsPageLocked   proc    near
        push    eax
        push    ebx
        push    es
        mov     bx,KernalZero
        mov     es,ebx
        shr     eax,12          ;get page number.
        mov     ebx,es:[eax*4+DETMAPADDR]
        shr     ebx,MEM_LOCK_SHIFT
        and     ebx,MEM_LOCK_MASK
        pop     es
        pop     ebx
        pop     eax
        ret
IsPageLocked   endp


;-------------------------------------------------------------------------------
;
;Return page table entry for a particular linear address.
;
;On Entry:-
;
;EAX    - linear address.
;
;On Exit:-
;
;Carry set if page table not present, else:
;
;EDX    - Page table entry.
;
GetPageStatus   proc    near
        push    eax
        push    esi
        push    ds
        push    es
        mov     dx,KernalDS
        mov     ds,edx
        assume ds:GROUP16
        mov     dx,KernalZero
        mov     es,edx
        push    eax
        mov     esi,PageDirLinear
        shr     eax,12+10       ;get PDE index
        test    d es:[esi+eax*4],1 shl PT_PRESBIT ;page table present?
        pop     eax
        jz      mem17_8
        mov     esi,PTMAPADDR   ;base of page table mapping region
        shr     eax,12          ;get page number.
        mov     edx,es:[esi+eax*4]      ;get page details.
        clc
        jmp     mem17_9
        ;
mem17_8:
        xor     edx,edx
        stc
mem17_9:
        pop     es
        pop     ds
        assume ds:nothing
        pop     esi
        pop     eax
        ret
GetPageStatus   endp


;-------------------------------------------------------------------------------
;
;The Exception 14 handler. This gets a shout whenever a not present page table is encountered by
;the processor. We check if the faulting address is within our range, passing to old handler if
;its not, re-aranging memory if it is.
;
VirtualFault    proc    far
        push    eax
        push    ecx
        push    edx
        push    ds
        mov     cx,KernalDS
        mov     ds,ecx
        assume ds:GROUP16
        mov     eax,ExceptionCR2
        cmp     eax,LinearBase  ;address below our memory space?
        jc      mem18_OldExit
        cmp     eax,LinearLimit ;address above our memory space?
        jnc     mem18_OldExit
        ;
        call    UnMapPhysical   ;get a physical page in EDX, flags in CL
        jc      mem18_Disk_Error;This should only happen on disk errors.
        shr     eax,12
        push    LinearEntry
        mov     LinearEntry,eax ;setup linear address we want to map.
        call    MapPhysical     ;map new page into faulting linear address space.
        pop     LinearEntry
        jc      mem18_Disk_Error;This should only happen on disk errors.
        ;
        pop     ds
        assume ds:nothing
        pop     edx
        pop     ecx
        pop     eax
        test    BYTE PTR cs:DpmiEmuSystemFlags,SF_16BIT    ;check exit size.
        jz      mem18_Use32Bit3
        retw                    ;16 bit exit.
mem18_Use32Bit3:
        retf
        ;
mem18_Disk_Error:
        jmp     mem18_OldExit
        ;
mem18_OldExit:
        pop     ds
        pop     edx
        pop     ecx
        pop     eax
        jmp     cs:[OldExcep14]       ;32 bit chaining.

VirtualFault    endp

