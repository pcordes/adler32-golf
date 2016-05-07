;;; yasm -felf32 -Worphan-labels -gdwarf2 adler32-i386.asm  && g++ -std=gnu++11 -m32 -O1 -g -Wall -Wextra -o test-adler32-i386  adler32-i386.o  test-adler32.cpp -lz && ./test-adler32-i386 && readelf -s ./adler32-i386.o | grep 'i386_v[0-9]*$'
;;; readelf | grep shows function sizes, since we use  SIZE  directives to set ELF metadata
;;; objdump -Mintel -drw adler32-i386.o

CPU 686

	;; bits 32
ADLER_MODULO equ 0xFFF1   ; 65521

section .text

ALIGN 32
global adler32_i386wrapper
adler32_i386wrapper:
	;; handle the custom calling convention (more scratch regs)
	;; ret addr at [esp], len at [esp+4], buf at [esp+8]
	push	ebx
	push	esi
	push	edi
	;; xor	eax,eax		; zero for easier debugging
	;; xor	ebx,ebx
	;; xor	edx,edx
	;; xor	edi,edi

	mov	ecx, [esp+16]
	mov	esi, [esp+20]
	call	adler32_i386_v7

	pop	edi
	pop	esi
	pop	ebx
	ret

global adler32_i386_v1
adler32_i386_v1:   ; (int dummy, const char *buf, int dummy, uint32_t len)

        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax	        ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        ;; cdq / inc if we were using edx for low
        lea     edi, [eax+1]    ; edi: low=1
	;lea	bx, [edx - (65536 - ADLER_MODULO)]
        ;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
.byteloop:
        lodsb                   ; upper 24b of eax stays zeroed (no partial-register stall on Intel P6/SnB-family CPUs, thanks to the xor-zeroing)
        add     edi, eax        ; low += zero_extend(buf[i])
        add     edx, edi        ; high += low
        loop   .byteloop
.end:
        ;; exit when ecx = 0, eax = last byte of buf
        ;; lodsb at this point would load the terminating 0 byte, conveniently leaving eax=0

        mov     cx, ADLER_MODULO   ; ecx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
        ;sub    cx, (65536 - ADLER_MODULO) ; the immediate is small enough to use the imm8 encoding.  No saving over mov, though, since this needs a mod/rm byte

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq                     ; could be removed if we could arrange things so the loop ended with a load of the 0 byte

        div     ecx             ; div instead of idiv to fault instead of returning wrong answers if high has overflowed to negative.  (-1234 % m is negative)
        xchg    eax, edx        ; eax = high%m, edx=garbage(quotient)
        xchg    eax, edi        ; edi = high%m, eax=low

        cdq
        div     ecx             ; edx = low%m

        sal     edi, 16
        lea     eax, [edi+edx]  ; lower half of edi is zero (from shift), upper half of edx is zero (remainder < ADLER_MODULO).  Saves an insn, but no code-size
;       or      edx, edi
;       xchg    eax, edx        ; another 3B alternative:  xchg eax,edi / xchg ax,dx

        ;; If we give up on the standard calling convention altogether, we could save a byte by using edx as the return value
        ret
adler32_i386_end_v1:
size adler32_i386_v1 adler32_i386_end_v1 - adler32_i386_v1

a32_mod: dd ADLER_MODULO


;; **Cheaper ways to get ADLER_MODULO** (no ideas that worked):

;; If we had `dd ADLER_MODULO` (4 byte constant) at address `0`, we could `f7 32  div DWORD [edx]` for both `div`s.  We'd have to count the 4B of data as part of our code, though, and `mov cx, ADLER_MODULO` is also 4B.  (I think it's possible to specify the address for your data, in an ELF binary.)

;; If ADLER_MODULO was in thread-local storage at address `fs:0`,  `64 f7 32                div  DWORD fs:[edx]` would work.

;; None of these ways save any bytes unless we require the caller to put the constant somewhere.  If we do that, we might as well make the adler32 modulo constant a function parameter.  The 5th arg goes in `r8`, so each `div` would require a REX prefix (1B), for a total savings of 2B by offloading the constant to the caller.  If we go all-in with the custom calling convention, then we'd require that arg in `ebx` or `ebp` for a total saving of 4B.

;; ---


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; push16 / push16 / pop32 to merge even more cheaply than amd64
ALIGN 32
	;; deferred modulo means we can use div and just push the result, instead of setting up for next iter
	; div instead of idiv to fault instead of returning wrong answers if high has overflowed to negative.  (-1234 % m is negative)
adler32_i386_v3:   ; (int dummy, const char *buf, int dummy, uint32_t len)

        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        lea     edi, [edx+1]    ; edi: low=1
        ;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
.byteloop:
        lodsb                   ; upper 24b of eax stays zeroed (no partial-register stall on Intel P6/SnB-family CPUs, thanks to the xor-zeroing)
        add     edi, eax        ; low += zero_extend(buf[i])
        add     edx, edi        ; high += low
        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf
        ;; lodsb at this point would load the terminating 0 byte, conveniently leaving eax=0

	;; doing modulo-reduction inside the loop takes an extra register, and requires getting results back into regs for the next iteration instead of just pushing
        mov     cx, ADLER_MODULO       ; ecx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
        ;sub    cx, (65536 - ADLER_MODULO) ; the immediate is small enough to use the imm8 encoding.  No saving over mov, though, since this needs a mod/rm byte

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq                     ; could be removed if we could arrange things so the loop ended with a load of the 0 byte

        div     ecx
        push    dx		; push16 high%m

        xchg    eax, edi        ; eax=low
        cdq
        div     ecx             ; edx = low%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
	;; 32bit trick: two 16b pushes => one 32b pop
        push    dx              ; push16 low%m
        pop     eax             ; pop32  high%m << 16 | low%m   (x86 is little-endian)

        ret
adler32_i386_end_v3:
size adler32_i386_v3 adler32_i386_end_v3 - adler32_i386_v3


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALIGN 32

	;;  BROKEN
	;; use stack space as scratch.  [esp] requires a SIB byte, so this sucks
	;; use a 16bit addressing mode for cheaper ebx = 0xFFF1?
adler32_i386_v4:   ; (int dummy, const char *buf, int dummy, uint32_t len)

        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
;        lea     edi, [edx+1]    ; edi: low=1
	push	1		; low on the stack

	;; FIXME: requires edi=1, or some other reg that's valid in a 16b EA
	lea     ebx, [di-1 - 65536 + ADLER_MODULO - 1]  ; ebx = m = adler32 magic constant.
	;mov	ebx, ADLER_MODULO
.byteloop:
        lodsb
        add     [esp], eax        ; low += zero_extend(buf[i])
        add     edx, [esp]        ; high += low

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq
        div     ebx             ; edx = high%m, already in the right place.  high bytes of eax=0
        loop   .byteloop
        ;; exit when ecx = 0
	pop	eax

        push    dx             ; push high%m and 2B of zero padding

        cdq
        div     ebx             ; edx = low%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
        push    dx              ; push low%m with no padding
        pop     eax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

        ret
adler32_i386_end_v4:
size adler32_i386_v4  adler32_i386_end_v4 - adler32_i386_v4




;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do low modulo inside the loop with cmp/sub
	;; high modulo could be inside the loop with basically no change
ALIGN 32
adler32_i386_v5:   ; (int dummy, const char *buf, int dummy, uint32_t len)
        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        lea     edi, [edx+1]    ; edi: low=1
				;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
	lea     ebx, [di-1 - 65536 + ADLER_MODULO]  ; ebx = m = adler32 magic constant.
        ;lea     ebx, [word ADLER_MODULO]       ; ebx = m = adler32 magic constant.  32bit code: 1B shorter?

.byteloop:
        lodsb
        add     edi, eax        ; low += zero_extend(buf[i])

	cmp	edi, ebx
	jb     .low_mod_m_done
	sub	edi, ebx
.low_mod_m_done:

        add     edx, edi        ; high += low
        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq                     ; could be removed if we could arrange things so the loop ended with a load of the 0 byte

        div     ebx
	;; shl	edx,16
	;; ;;lea	eax, [edi + edx]
	;; or	edx, edi
	;; xchg	eax, edx

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
	push	dx
        push    di              ; push low%m with no padding
        pop     eax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

        ret
adler32_i386_end_v5:
size adler32_i386_v5 adler32_i386_end_v5 - adler32_i386_v5


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; keep  high  on the stack, low in edx
	;; sets up for the push16 / pop32 trick, but not the better 2x push16 version
	;; downside: no 4B option for getting ADLER_MODULO into ebx
ALIGN 32
adler32_i386_v6:   ; (int dummy, const char *buf, int dummy, uint32_t len)
        ;; args: len in ecx,  const char *buf in esi
        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ;
	inc	edx		; edx: low=1
        push	eax		; high=0

				;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
        mov     ebx, ADLER_MODULO       ; ebx = m = adler32 magic constant.  32bit code: 1B shorter?  no 16b-address register with known value

.byteloop:
        lodsb
        ;; add     edx, eax        ; low += zero_extend(buf[i])
	;; cmp	edx, ebx
	;; jb     .low_mod_m
	;; sub	edx, ebx
	add	eax, edx	; eax = low+buf[i]
	cdq
	div	ebx
;.low_mod_m:			; edx = low%=m

	;; 10B  for  high+=low;  high%=m;
	pop	edi		; edi=high
	;; add	[esp], edx
	add	edi, edx	; high += low
	cmp	edi, ebx
	jb     .high_mod_m
	sub	edi, ebx
.high_mod_m:
	push	edi

	;; 10B  for  high+=low;  high%=m;
	;; pop	eax		; eax = high
	;; push	edx		; [esp] = low%m
        ;; add     eax, edx        ; eax = high + low
	;; cdq
	;; div	ebx		; eax = 0x000000XX,  edx = high%m
	;; xchg	edx, [esp]	;; [esp] = high%m,  edx = low%m

        loop   .byteloop
        ;; exit when ecx = 0
	;; [esp] = high, edx = low

        ;; push    dx
	;; pop	eax             ; overlapping   high%m << 16 | low%m   (x86 is little-endian)
        ;; pop     dx              ; add esp, 2 to restore the stack pointer
	pop	eax
	shl	eax,16
	xchg	ax,dx

        ret
adler32_i386_end_v6:
size adler32_i386_v6 adler32_i386_end_v6 - adler32_i386_v6


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; low in edx, high in edi
	;; in-loop modulo
ALIGN 32
adler32_i386_v7:   ; (int dummy, const char *buf, int dummy, uint32_t len)
        ;; args: len in ecx,  const char *buf in esi
        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ;
	inc	edx		; edx: low=1
        xor	edi,edi		; edi: high=0

				;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
	lea     ebx, [di-0 - 65536 + ADLER_MODULO]  ; ebx = m = adler32 magic constant.  4B

.byteloop:
        lodsb
	add	eax, edx	; eax = low+buf[i]
	cdq
	div	ebx
;.low_mod_m:			; edx = low%=m

	;; 8B  for  high+=low;  high%=m;
	add	edi, edx	; high += low
	cmp	edi, ebx
	jb     .high_mod_m
	sub	edi, ebx
.high_mod_m:

	;; 8B  for  high+=low;  high%=m;
	;; add	edi, edx
	;; xchg	eax, edx
	;; xchg	eax, edi	; save edx(low%m) in edi while we div
	;; cdq			; in-loop modulo makes this safe
	;; div	ebx		; eax = 0x000000XX,  edx = high%m
	;; xchg	edi, edx

        loop   .byteloop
        ;; exit when ecx = 0

	;; only works in 32bit, not 64bit:  5B
        push    di
	push	dx
	pop	eax             ; overlapping   high%m << 16 | low%m   (x86 is little-endian)

	;; xchg	eax, edi
	;; shl	eax,16
	;; xchg	ax,dx

        ret
adler32_i386_end_v7:
size adler32_i386_v7 adler32_i386_end_v7 - adler32_i386_v7




;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; in-loop modulo to handle large inputs
ALIGN 32
adler32_i386_v8:   ; (int dummy, const char *buf, int dummy, uint32_t len)

        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
	;; inc	edx
	;; xor	edi,edi
        lea     edi, [edx+1]    ; edi: low=1
	lea     ebx, [di-1 - 65536 + ADLER_MODULO]  ; ebx = m = adler32 magic constant.  4B
        ;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
.byteloop:
        lodsb
        add    	edi, eax
	cmp	edi, ebx
	jb     .low_mod_m_done
	sub	edi, ebx
.low_mod_m_done:

	lea	eax, [edi + edx] ; eax = high+low
        cdq
        div     ebx		; eax = 0x00XX, edx = high%m

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

        push    dx		; push16 high%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
	;; 32bit trick: two 16b pushes => one 32b pop
        push    di              ; push16 low%m
        pop     eax             ; pop32  high%m << 16 | low%m   (x86 is little-endian)

        ret
adler32_i386_end_v8:
size adler32_i386_v8 adler32_i386_end_v8 - adler32_i386_v8

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; in-loop modulo for high
	;; deferred modulo for low
ALIGN 32
adler32_i386_v9:   ; (int dummy, const char *buf, int dummy, uint32_t len)

        ;; args: len in ecx,  const char *buf in esi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        lea     edi, [edx+1]    ; edi: low=1
	lea     ebx, [di-1 - 65536 + ADLER_MODULO]  ; ebx = m = adler32 magic constant.  4B
        ;jecxz  .end            ; handle len=0?  unlike rep, loop only checks ecx after decrementing
.byteloop:
        lodsb
        add    	edi, eax	 ; low += buf[i]
	lea	eax, [edi + edx] ; eax = high+low
        cdq
        div     ebx		; eax = 0x00XX, edx = high%m

        loop   .byteloop
        ;; exit when ecx = 0

        push    dx		; push16 high%m

	;; 4B vs. cmp/jcc/sub in the loop = 6B
	cdq			; high bytes of eax still zero from div in the loop
	xchg	eax, edi
	div	ebx

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
	;; 32bit trick: two 16b pushes => one 32b pop
        push    dx              ; push16 low%m
        pop     eax             ; pop32  high%m << 16 | low%m   (x86 is little-endian)

        ret
adler32_i386_end_v9:
size adler32_i386_v9 adler32_i386_end_v9 - adler32_i386_v9



;; ALIGN 32
;; global adler32_int


;; 	;; 32bit: BITS 32, with a non-standard ABI
;;         ;; As an inline-asm code-fragment, it would work in ABIs without a red zone, e.g. 32bit.  (change eax to eax, etc.)
;; adler32_int:   ; (int dummy, const int *buf, int dummy, int64_t len)

;;         ;; args: len in ecx,  *buf in esi

;;         xor	eax,eax		; eax: high=0
;;         ;cdq
;; 	;inc	edx		; edx: low=1
;;         lea     edi, [eax+1]    ; edi: low=1
;; ;	add	esi, ecx	; x32: esi
;; ;	neg	ecx		; then use [esi + ecx] / inc ecx / jnz
;; .byteloop:
;;         add     edi, [esi]	; low += buf[i]
;;         add     eax, edi        ; high += low
;; 	int3 ; inc esi is 1B in 32bit mode
;;         loop   .byteloop
;; .end:
;;         ;; exit when ecx = 0, eax = last byte of buf
;;         ;; lodsb at this point would load the terminating 0 byte, conveniently leaving eax=0

;;         mov     cx, ADLER_MODULO       ; ecx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
;;         ;sub    cx, (65536 - ADLER_MODULO) ; the immediate is small enough to use the imm8 encoding.  No saving over mov, though, since this needs a mod/rm byte

;; 	;; high is in eax already, and we can assume its sign bit is 0
;;         cdq
;;         div     ecx             ; div instead of idiv to fault instead of returning wrong answers if high has overflowed to negative.  (-1234 % m is negative)
;;         push    edx             ; push high%m with 2B of zero padding

;;         xchg    eax, edi        ; eax = low
;;         cdq
;;         div     ecx             ; edx = low%m

;;         ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
;;         push    dx              ; push low%m with no padding
;;         pop     eax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

;;         pop     dx              ; add esp, 2 to restore the stack pointer

;;         ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
;;         ret
;; adler32_int_end:


;; ALIGN 32

;;         ;; since we push/pop, this is no longer safe for inline asm in the SysV ABI (with a red zone)
;;         ;; As an inline-asm code-fragment, it would work in ABIs without a red zone, e.g. 32bit.  (change eax to eax, etc.)
;; global	adler32_int2
;; adler32_int2:   ; (int dummy, const int *buf, int dummy, int64_t len)

;;         ;; args: len in ecx,  *buf in esi

;;         lodsd			; every byte is zero-padded to 4B
;;         cdq			; edx: high=0
;;         lea     edi, [eax+1]    ; edi: low=1
;; ;	add	esi, ecx	; x32: esi
;; ;	neg	ecx
;; .byteloop:
;;         add     edi, eax	; low += buf[i]
;;         add     edx, edi        ; high += low
;; 	lodsd
;;         loop   .byteloop
;; .end:
;;         ;; exit when ecx = 0, eax = 0 from terminating 0 element

;;         mov     cx, ADLER_MODULO       ; ecx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
;;         ;sub    cx, (65536 - ADLER_MODULO) ; the immediate is small enough to use the imm8 encoding.  No saving over mov, though, since this needs a mod/rm byte

;; 	xchg	eax, edx
;;         ;; cdq  ; edx = 0 already
;;         div     ecx             ; div instead of idiv to fault instead of returning wrong answers if high has overflowed to negative.  (-1234 % m is negative)
;;         push    edx             ; push high%m with 2B of zero padding

;;         xchg    eax, edi        ; eax = low
;;         cdq
;;         div     ecx             ; edx = low%m

;;         ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
;;         push    dx              ; push low%m with no padding
;;         pop     eax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

;;         pop     dx              ; add esp, 2 to restore the stack pointer

;;         ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
;;         ret
;; adler32_int2_end:

;; ALIGN 32
