@ arm-linux-gnueabi-as --gen-debug -mimplicit-it=always -mfloat-abi=soft -mthumb adler32-arm.S
// arm-linux-gnueabi-g++ -Wa,-mimplicit-it=always -g -static -std=gnu++14 -Wall -Wextra -Os -march=armv6t2 -mthumb -mfloat-abi=soft test-adler32.cpp -fverbose-asm adler32-arm.S -o test-adler32

// There's no directive to enable implicit-it=always

// gcc uses compiler uses these in its output
.syntax unified
.arch armv8-a
.fpu softvfp

.thumb          @ aka .code 16

.p2align 4
.globl adler32arm_golf    @ put this label on the one we want to test

/*
.thumb_func
adler32arm_golf1:   @ (uint8_t buf[], uint32_t len)
        @ r0 = buf
        @ r1 = len
        push    {r4, r5, r6, lr}
        @add    r1, r0          @ r1 = endp
        movs    r2, #1          @ r2: low
        movs    r3, #0          @ r3: high
                                @ r4 = tmp for loading bytes
        movw    r5, #65521      @ r5: modulo constant

.byteloop:
        ldrb    r4, [r0], #1    @ *(buf++) post-increment addressing.  4B encoding
        @ldrb    r4, [r0, r1]   @ 2B encoding, but using it is a problem.

        add     r2, r2, r4      @ low += tmp
        add     r3, r3, r2      @ high += low

        @sub    r1, #1          @ 4B encoding: sub.w
        subs    r1, #1          @ len-- and set flags.  2B encoding
        @cmp    r4, #0          @ null-termination check. 2B encoding
        bne     .byteloop

        udiv    r0, r2, r5
        mls     r0, r5, r0, r2          @ r0 = low % m

        udiv    r4, r3, r5
        mls     r4, r5, r4, r3          @ r4 = high % m

        PKHBT   r0, r0, r4, lsl #16     @ 4B   r0 = high%m <<16 | low%m
        @orr     r0, r0, r4, lsl #16    @ 4B
        @orr     r0, r0, r4             @ 4B
        @add     r0, r0, r4, lsl #16    @ 4B
        @add     r0, r0, r4             @ 2B
        pop     {r4, r5, r6, pc}
adler32arm_end_golf1:
*/

.p2align 4
adler32arm_golf:

.thumb_func
adler32arm_golf2:   @ (uint8_t buf[], uint32_t len)
        @ r0 = buf
        @ r1 = len
        push    {r4, r5, r6, lr}   @ even number of regs keeps the stack aligned.  Good style since there's no code-size saving

        movs    r2, #1          @ r2: low
        movs    r3, #0          @ r3: high
                                @ r4 = tmp for loading bytes
        movw    r5, #65521      @ r5: modulo constant

adler32arm_golf2.byteloop2:
        ldrb    r4, [r0], #1    @ *(buf++) post-increment addressing.  4B encoding
        @ldrb    r4, [r0, r1]   @ 2B encoding, but unless we make the caller pass us buf+len and -len, it needs extra code somewhere else
	@ldmia   r0!, {r4}      @ int[] version:  r4 = [r0]; r0+=4;  post-increment addressing.  2B encoding.


        add     r2, r2, r4      @ low += tmp
        add     r3, r3, r2      @ high += low   // I think it's safe to do this before the modulo range-reduction for low, but it would certainly work to put it after.

        cmp     r2, r5
        subhs   r2, r5          @ if(low>=m) low-=m;


        cmp     r3, r5
        subhs   r3, r5          @ if(high>=m) high-=m;  @ 6B total for %.  predicated instructions require an IT insn in thumb2

        @sub    r1, #1          @ 4B encoding: sub.w to not set flags with immediate
        subs    r1, #1          @ len-- and set flags.  2B encoding
        @cmp    r4, #0          @ null-termination check. 2B encoding
        bne     adler32arm_golf2.byteloop2

@        udiv    r0, r2, r5
@        mls     r2, r5, r0, r2         @ r2 = low % m.  8B total for %

        PKHBT   r0, r2, r3, lsl #16     @ 4B   r0 = [ high%m <<16  |   low%m  ]
        @orr     r0, r0, r4, lsl #16    @ 4B
        @orr     r0, r0, r4             @ 4B
        @add     r0, r2, r3, lsl #16    @ 4B
        @add     r0, r0, r4             @ 2B
        pop     {r4, r5, r6, pc}
adler32arm_end_golf2:

        bx lr
        bl      __aeabi_uidivmod
.p2align 4

.thumb_func
adler32arm_int1:   @ (unsigned int *buf, uint32_t len)  // len = element count, not byte count
        @ r0 = buf
        @ r1 = len
        push    {r4, r5, r6, lr}
        @add    r1, r0          @ r1 = endp
        movs    r2, #1          @ r2: low
        movs    r3, #0          @ r3: high
                                @ r4 = tmp for loading bytes
        movw    r5, #65521      @ r5: modulo constant
.intloop:
        @ does this work?  I thought thumb mode had implicit SP for push/pop
        ldmia   r0!, {r4}       @ r4 = [r0]; r0+=4;  post-increment addressing, works in Thumb mode
        @ldmia  r1!, {r4}       @ yup, has a different encoding
        @ldr    r4, [r0], #4    @ 4B encoding, vs. 2 for ldmia

        add     r2, r2, r4      @ low += tmp
        add     r3, r3, r2      @ high += low

        subs    r1, #1          @ while(--len)
        bne     .intloop

        udiv    r0, r2, r5              @ TODO: 6B conditiona-sub
        mls     r0, r5, r0, r2          @ r0 = low % m

        udiv    r4, r3, r5
        mls     r4, r5, r4, r3          @ r4 = high % m

        orr     r0, r0, r2, lsl #16     @ no 2B alternative?
        pop     {r4, r5, r6, pc}
adler32arm_end_int1:




@       mul     r3, r3,r3  @ unpredictable result with src1 = src2? http://www.davespace.co.uk/arm/introduction-to-arm/multiply.html

@.p2align 4



@@         ldrb    r2, [r0]        @ zero_extendqisi2
@@ .L3:
@@         add     r2, r2, r1
@@         sub     r1, r2, #65280
@@         subs    r1, r1, #241
@@         cmp     r2, r4
@@         it      ls
@@         movls   r1, r2
@@         add     r3, r3, r1
@@         sub     r5, r3, #65280
@@         ldrb    r2, [r0, #1]!   @ zero_extendqisi2
@@         subs    r5, r5, #241
@@         cmp     r3, r4
@@         it      hi
@@         movhi   r3, r5
@@         cmp     r2, #0
@@         bne     .L3
@@         orr     r0, r1, r3, lsl #16
@@         pop     {r4, r5, lr}
@@ @        bx      lr

