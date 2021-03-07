; #if POS == 1
; This is obsolete code from the non-dur player. We won't use colour RAM
; anyway (since it adds tons of extra AND/ORA and we don't benefit from it).
; Therefore the macros are demangled so the assembler wouldn't accept them.
; They're here to show you how I used to split bytes into two subsequent
; nybbles unless you already realized how to do it...	/Anders Carlsson
;
; mac c     : .byte (1+({1}-1)*12)/16,(1+({1}-1)*12)%16 : endm
; mac quiet : .byte 0,0                                 : endm
; mac bd    : .byte 2,9 (9+2*16 = 41)			: endm
; mac t	    : .byte {1},{2},{3},{4},{5},{6},{7},{8}     : endm
;
; #else
; Duration-based macros, optimized for size.

; Track bytes
; %11111111 => end-of-track
; %1111YYYY => rept = YYYY
; %XXXXYYYY => transp = XXXX, block# = YYYY

; Once the player has repeated the subsequent block enough times, the repeat 
; counter remains zero and the 'rept' instruction would have to be given 
; again to repeat another block in the track list.

; Maximum 14 repeats per block. Maximum 14 steps transpose per block.

	mac rept
	.byte 240+({1}-1)
	endm

	mac block
	.byte {1}+({2}*16)
	endm

	mac tend
	.byte 255
	endm

	mac portaspeed
	.byte {1}+1,{2}+1,{3}+1,{4}+1			; sounds better
	.byte (1*SPD)-1,(2*SPD)-1,(3*SPD)-1,(4*SPD)-1
	.byte (5*SPD)-1,(6*SPD)-1,(7*SPD)-1,(8*SPD)-1
	endm

; Block bytes
; %11111111 => block end
; %YYYXXXXX => dur = YYY, note = XXXXX

; Total 31 playable notes (c1 - e3 plus high hat and silence)
; Maximum duration = 7

        mac c		
        .byte  1+({1}-1)*12 + (({2}-1)*32)
        endm
        mac c#
        .byte  2+({1}-1)*12 + (({2}-1)*32)
        endm
        mac d
        .byte  3+({1}-1)*12 + (({2}-1)*32)
        endm
        mac d#
        .byte  4+({1}-1)*12 + (({2}-1)*32)
        endm
        mac e
        .byte  5+({1}-1)*12 + (({2}-1)*32)
        endm
        mac f
        .byte  6+({1}-1)*12 + (({2}-1)*32)
        endm
        mac f#
        .byte  7+({1}-1)*12 + (({2}-1)*32)
        endm
        mac g
        .byte  8+({1}-1)*12 + (({2}-1)*32)
        endm
        mac g#
        .byte  9+({1}-1)*12 + (({2}-1)*32)
        endm
        mac a
        .byte 10+({1}-1)*12 + (({2}-1)*32)
        endm
        mac a#
        .byte 11+({1}-1)*12 + (({2}-1)*32)
        endm
        mac h
        .byte 12+({1}-1)*12 + (({2}-1)*32)
        endm

        mac quiet     
        .byte  0 + (({1}-1)*32)
        endm
	mac bd
	.byte  1 + (({1}-1)*32)
	endm
	mac sd
	.byte 25 + (({1}-1)*32)
	endm
	mac hh
	.byte 30 + (({1}-1)*32)
	endm

; heh - this happens to be the same bit pattern as for tend, but for clarity
; we create two different macros (and macros are free on the target machine)
	mac bend
	.byte 255	; all bits set - we'll check for this first of all
	endm

; #endif
