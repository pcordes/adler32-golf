;;; yasm -felf64 -Worphan-labels -gdwarf2 adler32-golf.asm  && g++ -std=gnu++11 -O1 -g -Wall -Wextra -o test-adler32  adler32-golf.o  test-adler32.cpp -lz && ./test-adler32
;;;	&& objdump -Mintel -drw adler32-golf.o


ADLER_MODULO equ 0xFFF1   ; 65521

section .text

ALIGN 32
global golfed_adler32_amd64     ; put this label on the one we want to test


        ;; tricks: xchg eax, r32 is one byte; cheaper than mov.  8086 needed data in ax for a lot more stuff than >= 386
        ;; if we assume that our numbers haven't wrapped to negative, we can zero edx with a cdq insn (one byte shorder than xor-zeroing)


global golfed_adler32_amd64_v1
golfed_adler32_amd64_v1:   ; (int dummy, const char *buf, int dummy, uint64_t len)

        ;; args: len in rcx,  const char *buf in rsi
        ;; Without dummy args, (unsigned len, const char *buf),  mov ecx, edi is the obvious solution, costing 2 bytes

        xor     eax,eax	        ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        ;; cdq / inc if we were using edx for low
        lea     edi, [rax+1]    ; edi: low=1
	;lea	bx, [rdx - (65536 - ADLER_MODULO)]
        ;jrcxz  .end            ; handle len=0?  unlike rep, loop only checks rcx after decrementing
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
        lea     eax, [rdi+rdx]  ; lower half of edi is zero (from shift), upper half of edx is zero (remainder < ADLER_MODULO).  Saves an insn, but no code-size
;       or      edx, edi
;       xchg    eax, edx        ; another 3B alternative:  xchg eax,edi / xchg ax,dx

        ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
        ;; If we give up on the standard calling convention altogether, we could save a byte by using edx as the return value
        ret
golfed_adler32_amd64_end_v1:
size golfed_adler32_amd64_v1 golfed_adler32_amd64_end_v1 - golfed_adler32_amd64_v1

a32_mod: dd ADLER_MODULO


;; **Cheaper ways to get ADLER_MODULO** (no ideas that worked):

;; If we had `dd ADLER_MODULO` (4 byte constant) at address `0`, we could `f7 32  div DWORD [rdx]` for both `div`s.  We'd have to count the 4B of data as part of our code, though, and `mov cx, ADLER_MODULO` is also 4B.  (I think it's possible to specify the address for your data, in an ELF binary.)

;; If ADLER_MODULO was in thread-local storage at address `fs:0`,  `64 f7 32                div  DWORD fs:[rdx]` would work.

;; None of these ways save any bytes unless we require the caller to put the constant somewhere.  If we do that, we might as well make the adler32 modulo constant a function parameter.  The 5th arg goes in `r8`, so each `div` would require a REX prefix (1B), for a total savings of 2B by offloading the constant to the caller.  If we go all-in with the custom calling convention, then we'd require that arg in `ebx` or `ebp` for a total saving of 4B.

;; ---


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; push64 / push16 / overlapping pop64 to merge more cheaply
ALIGN 32

        ;; since we push/pop, this is no longer safe for inline asm in the SysV ABI (with a red zone)
        ;; As an inline-asm code-fragment, it would work in ABIs without a red zone, e.g. 32bit.  (change rax to eax, etc.)
	; div instead of idiv to fault instead of returning wrong answers if high has overflowed to negative.  (-1234 % m is negative)
golfed_adler32_amd64_v3:   ; (int dummy, const char *buf, int dummy, uint64_t len)

        ;; args: len in rcx,  const char *buf in rsi
        ;; Without dummy args, (unsigned len, const char *buf),  mov ecx, edi is the obvious solution, costing 2 bytes

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        ;; cdq / inc if we were using edx for low
        lea     edi, [rdx+1]    ; edi: low=1
        ;jrcxz  .end            ; handle len=0?  unlike rep, loop only checks rcx after decrementing
.byteloop:
        lodsb                   ; upper 24b of eax stays zeroed (no partial-register stall on Intel P6/SnB-family CPUs, thanks to the xor-zeroing)
        add     edi, eax        ; low += zero_extend(buf[i])
        add     edx, edi        ; high += low
        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf
        ;; lodsb at this point would load the terminating 0 byte, conveniently leaving eax=0

        mov     cx, ADLER_MODULO       ; ecx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
        ;sub    cx, (65536 - ADLER_MODULO) ; the immediate is small enough to use the imm8 encoding.  No saving over mov, though, since this needs a mod/rm byte

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq                     ; could be removed if we could arrange things so the loop ended with a load of the 0 byte

        div     ecx
        push    rdx             ; push high%m and 6B of zero padding

        xchg    eax, edi        ; eax=low
        cdq
        div     ecx             ; edx = low%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
        push    dx              ; push low%m with no padding
        pop     rax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

        pop     dx              ; add rsp, 2 to restore the stack pointer

        ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
        ret
golfed_adler32_amd64_end_v3:
size golfed_adler32_amd64_v3 golfed_adler32_amd64_end_v3 - golfed_adler32_amd64_v3


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALIGN 32

	;;  BROKEN
	;; use stack space as scratch.  [esp] requires a SIB byte, so this sucks
	;; use a 16bit addressing mode for cheaper ebx = 0xFFF1?
bits 32
adler32_i386_v4:   ; (int dummy, const char *buf, int dummy, uint64_t len)

        ;; args: len in rcx,  const char *buf in rsi
        ;; Without dummy args, (unsigned len, const char *buf),  mov ecx, edi is the obvious solution, costing 2 bytes

	push ebx
        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
;        lea     edi, [rdx+1]    ; edi: low=1
	push	1		; low on the stack

	;; FIXME: requires edi=1, or some other reg that's valid in a 16b EA
	lea     ebx, [di - 65536 + ADLER_MODULO - 1]  ; ebx = m = adler32 magic constant.  (upper 16b of ecx is zero from the loop exit condition.  This saves 1B over mov r32,imm32)
	;mov	ebx, ADLER_MODULO
.byteloop:
        lodsb
        add     [esp], eax        ; low += zero_extend(buf[i])
        add     edx, [esp]        ; high += low

        xchg    eax, edx        ; eax = high,  edx = buf[last_byte]
        cdq
        div     ebx             ; edx = high%m, already in the right place.  high bytes of eax=0
        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf
	pop	eax

        push    edx             ; push high%m and 6B of zero padding

        cdq
        div     ebx             ; edx = low%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
        push    dx              ; push low%m with no padding
        pop     eax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

        pop     dx              ; add rsp, 2 to restore the stack pointer

        ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
	pop ebx
        ret
adler32_i386_end_v4:
size adler32_i386_v4  adler32_i386_end_v4 - adler32_i386_v4
	;; - 2 : don't count push/pop
bits 64





;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; do low modulo with cmp/sub
ALIGN 32
golfed_adler32_amd64_v5:   ; (int dummy, const char *buf, int dummy, uint64_t len)
        ;; args: len in rcx,  const char *buf in rsi

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ; edx: high=0
        lea     edi, [rdx+1]    ; edi: low=1
				;jrcxz  .end            ; handle len=0?  unlike rep, loop only checks rcx after decrementing
        mov     ebx, ADLER_MODULO       ; ebx = m = adler32 magic constant.  32bit code: 1B shorter?

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
	shl	edx,16
	;;lea	eax, [edi + edx]
	or	edx, edi
	xchg	eax, edx

        ;; push    rdx             ; push high%m and 6B of zero padding

        ;; ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
        ;; push    di              ; push low%m with no padding
        ;; pop     rax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

        ;; pop     di              ; add rsp, 2 to restore the stack pointer

        ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
        ret
golfed_adler32_amd64_end_v5:
size golfed_adler32_amd64_v5 golfed_adler32_amd64_end_v5 - golfed_adler32_amd64_v5
	;; -1B in 32bit mode


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; keep  high  on the stack
ALIGN 32
golfed_adler32_amd64_v6:   ; (int dummy, const char *buf, int dummy, uint64_t len)
golfed_adler32_amd64:
        ;; args: len in rcx,  const char *buf in rsi
	push	rbx

        xor     eax,eax         ; zero upper bytes for lodsb
        cdq                     ;
	inc	edx		; edx: low=1
        push	rax		; high=0

				;jrcxz  .end            ; handle len=0?  unlike rep, loop only checks rcx after decrementing
        mov     ebx, ADLER_MODULO       ; ebx = m = adler32 magic constant.  32bit code: 1B shorter?

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

	;; 10B
	pop	rdi		; edi=high
	;; add	[rsp], edx
	add	edi, edx	; high += low
	cmp	edi, ebx
	jb     .high_mod_m
	sub	edi, ebx
.high_mod_m:
	push	rdi

	;; 10B
	;; pop	rax		; eax = high
	;; push	rdx		; [rsp] = low%m
        ;; add     eax, edx        ; eax = high + low
	;; cdq
	;; div	ebx		; eax = 0x000000XX,  edx = high%m
	;; xchg	edx, [rsp]	;; [rsp] = high%m,  edx = low%m
        loop   .byteloop
        ;; exit when ecx = 0
	;; [rsp] = high, edx = low

        push    dx
	pop	rax             ; overlapping   high%m << 16 | low%m   (x86 is little-endian)
        pop     dx              ; add rsp, 2 to restore the stack pointer

	pop	rbx
        ret
golfed_adler32_amd64_end_v6:
size golfed_adler32_amd64_v6 golfed_adler32_amd64_end_v6 - golfed_adler32_amd64_v6
	;; 32bit:  -2B save/restore rbx, -1B inc, -1B constant



;; ALIGN 32
;; global golfed_adler32_int


;; 	;; 32bit: BITS 32, with a non-standard ABI
;;         ;; As an inline-asm code-fragment, it would work in ABIs without a red zone, e.g. 32bit.  (change rax to eax, etc.)
;; golfed_adler32_int:   ; (int dummy, const int *buf, int dummy, int64_t len)

;;         ;; args: len in ecx,  *buf in rsi

;;         xor	eax,eax		; eax: high=0
;;         ;cdq
;; 	;inc	edx		; edx: low=1
;;         lea     edi, [rax+1]    ; edi: low=1
;; ;	add	rsi, rcx	; x32: esi
;; ;	neg	ecx		; then use [esi + ecx] / inc ecx / jnz
;; .byteloop:
;;         add     edi, [rsi]	; low += buf[i]
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
;;         push    rdx             ; push high%m with 6B of zero padding

;;         xchg    eax, edi        ; eax = low
;;         cdq
;;         div     ecx             ; edx = low%m

;;         ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
;;         push    dx              ; push low%m with no padding
;;         pop     rax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

;;         pop     dx              ; add rsp, 2 to restore the stack pointer

;;         ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
;;         ret
;; golfed_adler32_int_end:


;; ALIGN 32

;;         ;; since we push/pop, this is no longer safe for inline asm in the SysV ABI (with a red zone)
;;         ;; As an inline-asm code-fragment, it would work in ABIs without a red zone, e.g. 32bit.  (change rax to eax, etc.)
;; global	golfed_adler32_int2
;; golfed_adler32_int2:   ; (int dummy, const int *buf, int dummy, int64_t len)

;;         ;; args: len in ecx,  *buf in rsi

;;         lodsd			; every byte is zero-padded to 4B
;;         cdq			; edx: high=0
;;         lea     edi, [rax+1]    ; edi: low=1
;; ;	add	rsi, rcx	; x32: esi
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
;;         push    rdx             ; push high%m with 6B of zero padding

;;         xchg    eax, edi        ; eax = low
;;         cdq
;;         div     ecx             ; edx = low%m

;;         ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
;;         push    dx              ; push low%m with no padding
;;         pop     rax             ; pop  high%m << 16 | low%m   (x86 is little-endian)

;;         pop     dx              ; add rsp, 2 to restore the stack pointer

;;         ;; outside of 16bit code, we can't justify returning the result in the dx:ax register pair
;;         ret
;; golfed_adler32_int2_end:

;; ALIGN 32
