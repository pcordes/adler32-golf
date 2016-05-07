;;; 16bit code can return the 32bit result in dx:ax, saving instructions to do that
;;; My 32/64bit versions spend about 6 bytes on the push rdx, push dx, pop rax, pop dx
;;; Getting 65521 into a register is slightly cheaper, too.

;; 65521 (0xFFF1) is more than half 65536, so modulo-reduction on carry doesn't lose information


;;; Test by building the code in 32bit mode and calling from C.
;;; Operation of instructions with explicit operand size (implied by registers) is unchanged
;;; lodsb and loop will use the default operand size and address size
;;; This lets us test with 32bit pointers

;; yasm -felf32 -Worphan-labels -gdwarf2 adler32-x86-16.asm -o adler32-x86-16+32.o && g++ -DTEST_16BIT -m32 -std=gnu++11 -O1 -g -Wall -Wextra -o test-adler32-x16  adler32-x86-16+32.o  test-adler32.cpp -lz && ./test-adler32-x16


;; SIZEs: uncomment bits16 and run:
;bits 16   ;  assemble into an ELF object with bits 16 so we can use readelf -s to get symbol sizes.
;; yasm -felf32 -Worphan-labels -gdwarf2 adler32-x86-16.asm && readelf -s adler32-x86-16.o | grep '_v[0-9]*$'
;;; objdump -drw -Mintel -Maddr16,data16 adler32 adler32-x86-16.o

;;; 16bit asm dump.  (no labels, so bits 16 into an ELF file is nicer)
;; yasm -fbin adler32-x86-16.asm  && objdump -Mintel -Drw   -b binary -mi386 -Maddr16,data16 adler32-x86-16

	;; idea:
	;; if (low >= m) low -= m;
	;;if (high >= 65536) high -= m;
	;; high %= m; at the end.  (low is good from the loop)

ADLER_MODULO equ 0xFFF1   ; 65521

section .text
CPU 386,undocumented intelnop	; we use setcc, so this won't run on 8086.  salc is undocumented


	;; 32bit SysV / cdecl wrapper for calling 16bit functions (assembled for 32bit, and using lodsb which will assemble to the default address size)
global adler32_16wrapper
adler32_16wrapper:
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
	call	adler32_x16_v8
	shl	edx, 16
	movzx	eax, ax
	or	eax, edx	; mov  dx, ax /  mov eax, edx

	pop	edi
	pop	esi
	pop	ebx
	ret

ALIGN 32


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; low %= m;  high %= m; at the end
	; -= m on carry inside the loop
adler32_x16_v3:   ; (const char *buf /*ds:si*/, uint16_t len /*cx*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     ax,ax           ; zero upper half for lodsb
        cwd                     ; dx: high=0
        ;lea     di, [dx+1]	; di: low=1
	mov	di, 1

        mov     bx, ADLER_MODULO       ; bx = m = adler32 magic constant.

	;;  len=0 case unhandled
.byteloop:
        lodsb

        add     di, ax        ; low += zero_extend(buf[i])
	jnc	.nocarry_low
	sub	di, bx
.nocarry_low:

        add     dx, di        ; high += low
	jnc	.nocarry_high
	sub	dx, bx
.nocarry_high:

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf
        ;; lodsb at this point would load the terminating 0 byte, conveniently leaving eax=0

        xchg    ax, dx        	; ax = high
        xor	dx,dx		;cwd isn't safe
        div     bx
        push    dx             	; push high%m

        xchg    ax, di        ; eax=low
        xor	dx,dx		;cwd isn't safe
        div     bx             ; edx = low%m

        ;; concatenate the two 16bit halves of the result by putting them in contiguous memory
	xchg	ax, dx	     ; ax = low%m
        pop     dx	     ; dx = high%m
	;; dx:ax = result
        ret
.end:
adler32_x16_v3_end:
size adler32_x16_v3 adler32_x16_v3.end - adler32_x16_v3

	;; obvious separator between functions, since no symbols make it to a flat binary
	;; palignr	xmm0, [gs: eax + ebx*4 + 0x1234], 3
	;; palignr	xmm0, [bx + 0x1234], 3
ALIGN 32



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; modulo-reduce low on carry
	;; modulo-reduce high every time, correct for carry out of the low16
adler32_x16_v4:   ; (uint16_t len /*cx*/, const char *buf /*ds:si*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     ax,ax           ; zero upper half for lodsb
        cwd                     ; dx: high=0
	mov	di, 1		; di: low=1

        mov     bx, ADLER_MODULO       ; bx = m = adler32 magic constant.
	;;  len=0 case unhandled
.byteloop:
        lodsb			; use native address-size

        add     di, ax		; low += zero_extend(buf[i])
	jnc	.nocarry_low
	sub	di, bx
.nocarry_low:

        add     dx, di		; high += low
;	jnc	.nocarry_high	; handled with setc / div
;	sub	dx, bx
;.nocarry_high:
	xchg	ax, dx		; dx = 0x00XX  (garbage limited to low byte)
	setc	dl   		; handle carry with setc to create the right 32bit dividend
	;; sbb	dl,dl  		; 0 or -1, but we need 0 or 1
	;; xor	dx,dx
	div	bx		; high %= m (in dx).  ax = 0 or 1: high byte will always be zero.

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

	;; The 1040 times '?' test-case produces low = ADLER_MODULO exactly.
	cmp	di, bx
	jb     .low_mod_m_done
	sub	di, bx

	;; push	   dx
        ;; xchg    ax, di         ; ax=low
        ;; xor     dx,dx
        ;; div     bx             ; dx = low%m
	;; xchg    ax, dx	  ; ax = low%m
	;; pop     dx
.low_mod_m_done:

	xchg    ax, di
	;; dx:ax = result
        ret
adler32_x16_v4_end:
size adler32_x16_v4 adler32_x16_v4_end - adler32_x16_v4



ALIGN 32

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; handle carry out of the low16 by using some 32bit regs
	;; saves 4B (jnc/sub in the loop), costs 7B (operand size prefixes and constant-generation)
;; untested
adler32_x16_v5:   ; (uint16_t len /*cx*/, const char *buf /*ds:si*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     eax,eax           ; zero upper half for lodsb
        cwd                     ; dx: high=0
;	mov	di, 1		; di: low=1

	lea     ebx, [word ADLER_MODULO]       ; bx = m = adler32 magic constant.
	;; mov	ebx, ADLER_MODULO	; 1B longer than using lea to zero-extend a [disp16]
	lea	edi, [bx + (65536 - ADLER_MODULO) + 1 ] ; low edi = 1 in 4 bytes, by wrapping a 16bit addressing mode.  bx instead of no reg so we can use a disp8 instead of disp16
	;; xor	edi, edi
	;; inc	di		; di: low=1

	;;  len=0 case unhandled
.byteloop:
        lodsb			; use native address-size

        add     edi, eax	; low += zero_extend(buf[i])
	cmp	edi, ebx
	jb     .low_mod_m_done
	sub	edi, ebx
.low_mod_m_done:

        add     dx, di		; high += low
;	jnc	.nocarry_high	; handled with setc / div
;	sub	dx, bx
;.nocarry_high:
	xchg	ax, dx		; dx = 0x00XX  (garbage limited to low byte)
	setc	dl   		; handle carry with setc to create the right 32bit dividend
	div	bx		; high %= m (in dx).  ax = 0 or 1: high byte will always be zero.

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

	xchg    ax, di
	;; dx:ax = result
        ret
adler32_x16_v5_end:
size adler32_x16_v5 adler32_x16_v5_end - adler32_x16_v5


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ALIGN 32
	;; modulo-reduce low in the loop on carry or > m
	;; combines the in-loop reduce on carry with the post-loop reduce on >=
adler32_x16_v6:   ; (uint16_t len /*cx*/, const char *buf /*ds:si*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     ax,ax           ; zero upper half for lodsb
        cwd                     ; dx: high=0
	mov	di, 1		; di: low=1

        mov     bx, ADLER_MODULO       ; bx = m = adler32 magic constant.
	;;  len=0 case unhandled
.byteloop:
        lodsb			; use native address-size

        add     di, ax		; low += zero_extend(buf[i])
	jc	.carry_low
	cmp	di, bx
	jb     .low_mod_m_done
.carry_low:
	sub	di, bx
.low_mod_m_done:

        add     dx, di		; high += low
	;; Store the carry=0 or 1 in ax, for a 32bit dividend (after xchg)
	;; xor	ax,ax		; not needed: garbage limited to low byte
	setc	al   		; handle carry with setc to create the right 32bit dividend
	;; salc (undocumented) / neg al  ;  instead of sbb or setc
	;; sbb	ax,ax  		; 0 or -1, but we need 0 or 1.  neg ax is another 2B, and  inc ax  emulates setnc
	xchg	ax, dx
	div	bx		; high %= m (in dx).  ax = 0 or 1: high byte will always be zero.

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

	xchg    ax, di
	;; dx:ax = result
        ret
adler32_x16_v6_end:
size adler32_x16_v6 adler32_x16_v6_end - adler32_x16_v6



;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ALIGN 32
	;; modulo-reduce low and low in the loop with setc / div
	;; push / pop for data movement, minimal cost zeroing of dx before setcc / div
adler32_x16_v7:   ; (uint16_t len /*cx*/, const char *buf /*ds:si*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     ax,ax           ; zero upper half for lodsb
        cwd                     ; dx: high=0
	mov	di, 1		; di: low=1

        mov     bx, ADLER_MODULO       ; bx = m = adler32 magic constant.
	;;  len=0 case unhandled
.byteloop:
        lodsb			; use native address-size

	push 	dx
	cwd
        add     ax, di		; zero_extend(buf[i]) += low
	setc	dl
	div	bx
	xchg	ax, dx		; dx=0x00XX
	xchg	ax, di		; di = low%m.   ax= old low
;	mov	di, dx ; (doesn't leave dh == 0)
.low_mod_m_done:
	pop	ax		; ax = high
        add     ax, di		; high += low
	setc	dl   		; handle carry with setc to create the right 32bit dividend
	div	bx		; high %= m (in dx).  ax = 0 or 1: high byte will always be zero.

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

	xchg    ax, di
	;; dx:ax = result
        ret
adler32_x16_v7_end:
size adler32_x16_v7 adler32_x16_v7_end - adler32_x16_v7


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ALIGN 32
	;; v6 but with low in dx, high in di.  This requires an extra xchg after the loop
	;; sbb/neg is 4B, but we save an xchg, so no net savings inside the loop
adler32_x16_v8:   ; (uint16_t len /*cx*/, const char *buf /*ds:si*/)
	;; clobbers ax,cx,dx, bx,si,di

        xor     ax,ax           ; zero upper half for lodsb
        cwd                     ; dx: low=1
	inc	dx
	xor	di,di		; di: high=0

        mov     bx, ADLER_MODULO       ; bx = m = adler32 magic constant.
	;;  len=0 case unhandled
.byteloop:
        lodsb			; use native address-size

        add     ax, dx		; high += low
	;;setc	al   		; handle carry with setc to create the right 32bit dividend
	;; xchg ax, dx
	;; salc (undocumented) / neg al  ;  instead of sbb or setc
	sbb	dx,dx  		; 0 or -1
	neg	dx		; 0 or  1
	div	bx		; high %= m (in dx).  ax = 0 or 1: high byte will always be zero.

        add     di, dx		; high += low
	jc	.carry_high
	cmp	di, bx
	jb     .high_mod_m_done
.carry_high:
	sub	di, bx
.high_mod_m_done:

        loop   .byteloop
        ;; exit when ecx = 0, eax = last byte of buf

	xchg    ax, di
	xchg	dx, ax
	;; dx:ax = result
        ret
adler32_x16_v8_end:
size adler32_x16_v8 adler32_x16_v8_end - adler32_x16_v8
