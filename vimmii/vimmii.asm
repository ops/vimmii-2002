
SYSTEMSEL = $31a	; 0 for NTSC, 1 for PAL
AUTO      = $31b	; 0 for manual, 1 for automatic control
LOADER    = $31c	; ..$3e3


  processor 6502


tPtr  = $00	; 0 & 1
sixtimes = $200	; $200..$220
; codeorg	; $220..$2d8
; $1000-11e0	video matrix
; $11e0-1400	code, scrolltext
; $1400-1ca4	charfont
; $1ca4-2000	code, music

NTSC	= 1
PAL	= 2

COLUMNS = 3*8	; 24 of 29/26	must be even
PAL_ROWS = 20
NTSC_ROWS = 17

#if SYSTEM & NTSC
TOPPOS = 8	; minimum = 8
#else 
TOPPOS = 14
#endif

PAL_LINES = 312
PAL_CYCLES_PER_LINE = 71
PAL_SCRCENTER = 34

NTSC_LINES = 261
NTSC_CYCLES_PER_LINE = 65
NTSC_SCRCENTER = 26

PAL_TIMER_VALUE = PAL_LINES * PAL_CYCLES_PER_LINE - 2
NTSC_TIMER_VALUE = NTSC_LINES * NTSC_CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

PAL_RASTER = PAL_LINES/2-6
NTSC_RASTER = NTSC_LINES/2-6


; The BASIC line - Note: it is overwritten by the video matrix quite quickly

	.org $1001	; for the unexpanded Vic-20
basic:
	.word 0$	; link to next line
	.word 2002	; line number
	.byte $9E	; SYS token

; SYS digits

	.if (start) / 10000
	.byte $30 + (start) / 10000
	.endif
	.if (start) / 1000
	.byte $30 + (start) % 10000 / 1000
	.endif
	.if (start) / 100
	.byte $30 + (start) % 1000 / 100
	.endif
	.if (start) / 10
	.byte $30 + (start) % 100 / 10
	.endif
	.byte $30 + (start) % 10
0$	.byte 0,0,0	; end of BASIC program

	.org $1010
start
	lda #1
	sta AUTO

	; detect the system
	lda #0
	sta $9002
	sta SYSTEMSEL
	sta paladd+1
0$	lda $9004
	cmp #1
	bne 0$		; wait for line 1
1$	lda $9004
	beq 2$		; line 0 reached -> NTSC
	cmp #NTSC_LINES/2+2
	bne 1$
	inc SYSTEMSEL
	lda #PAL_ROWS*COLUMNS-NTSC_ROWS*COLUMNS
	sta paladd+1
2$	; system detected: 0 for NTSC, 1 for PAL

	sei

	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

	jsr setcolmem
	jsr inittext

	lda SYSTEMSEL
	beq 3$
	lda #PAL_ROWS*COLUMNS-NTSC_ROWS*COLUMNS
	sta paladd+1
3$

	;jsr player_init

;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b

	ldx #NTSC_RASTER+1	; wait for this raster line (times 2)
	lda SYSTEMSEL
	beq wait$
	ldx #PAL_RASTER+1	; wait for this raster line (times 2)
wait$:	cpx $9004
	bne wait$


sync:	ldx #NTSC_RASTER	; wait for this raster line (times 2)
	lda SYSTEMSEL
	beq 0$
	ldx #PAL_RASTER		; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed

	; No cycle-exact needed for this part..

	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	ldy #<NTSC_TIMER_VALUE
	ldx #>NTSC_TIMER_VALUE
	lda SYSTEMSEL
	beq 1$
	ldy #<PAL_TIMER_VALUE
	ldx #>PAL_TIMER_VALUE
1$	sty $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	;lda #$82
	;sta $911e	; enable Restore key

	lda #0
	tax
	jmp continit


setcolmem:
	ldx #0
	lda #8+COL3
1$	sta $9400,x
	sta $9500,x
	dex
	bne 1$

	ldx #32-1
11$	txa
	asl
	sta add$+1
	asl
	clc
add$	adc #0
	sta sixtimes,x
	dex
	bpl 11$

	ldx #0
12$	lda codesrc,x
	sta codeorg,x
	inx
	cpx #codeend-codesrc
	bne 12$

	lda #TOPPOS
	sta $9001	; vertical centering

	ldx #NTSC_SCRCENTER-COLUMNS	; centered
	lda SYSTEMSEL
	beq 0$

	ldx #PAL_SCRCENTER-COLUMNS	; centered
0$	stx $9000	; horizontal centering
	ldx #NTSC_ROWS*2+1	; rows & 16x8 chars
	lda SYSTEMSEL
	beq 2$
	ldx #PAL_ROWS*2+1	; rows & 16x8 chars
2$	stx $9003
	lda #$cd	; matrix at $1000, chars at $1400..
	sta $9005
	lda $900e
	and #$0f
	ora #COL1*16	; brown auxcol
	sta $900e
	lda #COL2*16+8+COL0	; light brown background, black border
	sta $900f
	rts


codesrc
#rorg $220
codeorg
COL0 = 0	; 0  0  0  0
COL1 = 5	; 2  6  8  4
COL2 = 13	;10  3  9 12
COL3 = 1	; 1  1  1  1

COLO1:	dc.b $80, $40, $20, $50
COLO2:	dc.b $98, $c8, $a8, $d8

CR	lda (tPtr),y
	bne lrts

	ldx AUTO
	beq inittext
	inc KEYWAIT+1

inittext:
	lda #<text
	sta tPtr+0
	lda #>text
	sta tPtr+1
chtextcol:
0$	lda #0
	and #3
	tay
	inc 0$+1
	lda $900e
	and #$0f
	ora COLO1,y
	sta $900e
	lda COLO2,y
	sta $900f
lrts	rts


puttextline:
	ldy #0		; calculate line length
	ldx #0
0$	lda (tPtr),y
	iny
	cmp #13
	beq 2$
	cmp #14
	beq 2$
	cmp #"I"
	beq 1$
	cmp #" "
	beq 1$
	inx
1$	inx
	inx
	bne 0$		; jump always
2$	stx 3$+1
	lda #COLUMNS
	sec
3$	sbc #0
	lsr
	clc
paladd	adc #0
	tax		; start column

4$	ldy #0		; plot line
	lda (tPtr),y
	inc tPtr+0
	bne 5$
	inc tPtr+1
5$	cmp #13
	bne 6$
	jmp CR
6$	cmp #14
	bne 8$
	jmp CR
8$	cmp #" "
	beq 7$		; space, no need to update anything
	and #31
	tay
	sty 66$+1
	lda sixtimes,y
	tay
	lda map+0,y
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+0,x
	lda map+1,y
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+0,x
	lda map+2,y
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+1,x
	lda map+3,y
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+1,x
66$	lda #0
	cmp #9	; "I"
	beq 7$
	cmp #0	; " "
	beq 7$
	lda map+4,y
	sta $1000+NTSC_ROWS*COLUMNS-2*COLUMNS+2,x
	lda map+5,y
	sta $1000+NTSC_ROWS*COLUMNS-1*COLUMNS+2,x
	inx
7$	inx
	inx
	bne 4$		; jump always

#rend
codeend

	.org $1000 + COLUMNS*PAL_ROWS

continit:
1$	sta $1000,x
	sta $1000+COLUMNS*PAL_ROWS/2,x
	inx
	cpx #COLUMNS*PAL_ROWS/2
	bne 1$

	cli
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory

KEYWAIT	lda #0
	beq KEYWAIT

	; Fix loader code to return instead of running the code
	lda #3
	sta $034b	; ldx #15 -> ldx #3 to only clear $9000..$9003
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389
	sei
	lda #<irq2
	sta $0314
	lda #>irq2
	sta $0315
	cli
	jmp continue


text:
	dc.b "ALBERT",13
	dc.b "OF PU[<]",13
	dc.b "PRESENTS",13
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "VICI",13
	dc.b "ITERUM",13
	dc.b "MMII",13
	dc.b 13
	dc.b "THE",13
	dc.b "VIC DEMO",13
	dc.b "AD[OO[",13
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "PRESS X",13
	dc.b "FOR",13
	dc.b "MANUAL",13
	dc.b "MODE",13
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b "RUNSTOP",13
	dc.b 13
	dc.b "TO SKIP",13
	dc.b "PARTS IN",13
	dc.b 13
	dc.b "MANUAL",13
	dc.b "MODE",13
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b "REMEMBER",13
	dc.b "THESE",13
	dc.b "FACTS",13
	dc.b "ABOUT",13
	dc.b "VIC [O",13
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "VIC [O",13
	dc.b "HAS",13
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 13
	dc.b "I MHZ",13
	dc.b "EIGHTBIT",13
	dc.b "CPU",13
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b "FIVE KB",13
	dc.b "OF",13
	dc.b "GRAPHICS",13
	dc.b "MEMORY",13
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b "FIVE KB",13
	dc.b "OF TOTAL",13
	dc.b "MEMORY",13
	dc.b "IN THIS", 13
	dc.b "DEMO",13
	dc.b 14
	dc.b 13
	dc.b 128
	dc.b 14
	dc.b "NO",13
	dc.b "SPRITES",13
	dc.b 13
	dc.b "NO",13
	dc.b "SMOOTH",13
	dc.b "SCROLL",13
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 13
	dc.b "NO",13
	dc.b "BITMAP",13
	dc.b "MODES",13
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "THREE",13
	dc.b "PULSE",13
	dc.b "VOICES",13
	dc.b 13
	dc.b "AND",13
	dc.b "NOISE",13
	dc.b 13
	dc.b "NO ADSR",13
#if SYSTEM & PAL
	dc.b 14
#else
	dc.b 13
#endif
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b "NOW",13
	dc.b "SUMTHING",13
	dc.b "OLD",13
	dc.b 13
	dc.b "THEN",13
	dc.b "SUMTHING",13
	dc.b "BLUE",13
	dc.b 14
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b "SUMTHING",13
	dc.b "BORROWED",13
	dc.b 13
	dc.b "AND",13
	dc.b "SUMTHING",13
	dc.b "OF MY",13
	dc.b "OWN",13
#if SYSTEM & PAL
	dc.b 14
#else
	dc.b 13
#endif
	dc.b 13
	dc.b 128
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 14
	dc.b 13
	dc.b 0


	org $1400

	incbin "packed.bin"
map:
	incbin "map.bin"

scroll:
	lda #TOPPOS
	sta $9001

	ldx #COLUMNS-1
1$	lda $1000+1*COLUMNS,x	;4
	sta $1000+0*COLUMNS,x	;5
	lda $1000+2*COLUMNS,x
	sta $1000+1*COLUMNS,x
	lda $1000+3*COLUMNS,x
	sta $1000+2*COLUMNS,x
	lda $1000+4*COLUMNS,x
	sta $1000+3*COLUMNS,x
	dex
	bpl 1$		;24*41 = 984 = ~14 lines

	ldx #COLUMNS-1
2$	lda $1000+5*COLUMNS,x
	sta $1000+4*COLUMNS,x
	lda $1000+6*COLUMNS,x
	sta $1000+5*COLUMNS,x
	lda $1000+7*COLUMNS,x
	sta $1000+6*COLUMNS,x
	lda $1000+8*COLUMNS,x
	sta $1000+7*COLUMNS,x
	lda $1000+9*COLUMNS,x
	sta $1000+8*COLUMNS,x
	dex
	bpl 2$

	ldx #COLUMNS-1
3$	lda $1000+10*COLUMNS,x
	sta $1000+9*COLUMNS,x
	lda $1000+11*COLUMNS,x
	sta $1000+10*COLUMNS,x
	lda $1000+12*COLUMNS,x
	sta $1000+11*COLUMNS,x
	lda $1000+13*COLUMNS,x
	sta $1000+12*COLUMNS,x
	lda $1000+14*COLUMNS,x
	sta $1000+13*COLUMNS,x
	dex
	bpl 3$

	ldx #COLUMNS-1
4$
	lda $1000+15*COLUMNS,x
	sta $1000+14*COLUMNS,x
	lda $1000+16*COLUMNS,x
	sta $1000+15*COLUMNS,x
;	lda SYSTEMSEL
;	beq 11$

	lda $1000+17*COLUMNS,x
	sta $1000+16*COLUMNS,x
	lda $1000+18*COLUMNS,x
	sta $1000+17*COLUMNS,x
	lda $1000+19*COLUMNS,x
	sta $1000+18*COLUMNS,x
	lda #0
	sta $1000+PAL_ROWS*COLUMNS-COLUMNS,x	; clear the last line
;	beq 12$

;11$	lda #0
;	sta $1000+NTSC_ROWS*COLUMNS-COLUMNS,x	; clear the last line
12$	dex
	bpl 4$

5$	lda #0
	and #1
	beq 0$

	ldy #0
	lda (tPtr),y
	bne 8$
	jmp CR

8$	bpl 9$
	sta swait+1
	bmi 10$
9$	cmp #13
	bne 6$
	; if an empty line, just one empty line
10$	inc tPtr+0
	bne 7$
	inc tPtr+1
7$	rts

6$	jsr puttextline
0$	inc 5$+1
	rts

nextFile:
	dc.b "3K-VICPI"		; 8 significant chars

irq:	; No stable raster needed in this part!
	lda $9124	; ack?

swait	lda #0
	beq 0$

	dec swait+1
	jmp reti

0$	dec $9001
	lda SYSTEMSEL	;4
	;beq 1$		;2/3
	dec $9001	;6	; 2 lines/field for PAL
1$	lda $9001
	cmp #TOPPOS-8
	bne reti

	jsr scroll
reti
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$	and #2
	bne 97$
	; x
	ldx AUTO
	beq 97$
	dec AUTO	; disable automatic control
	jsr chtextcol
97$
irq2	jsr player_update
	jsr player_update
#if SYSTEM & NTSC
	jsr player_update
#endif

FADEOUT	lda #128	; fade out music, continue the scroll..
	bmi 0$
	lsr
	lsr
	lsr
	lsr
	sta ora$+1
	lda $900e
	and #$f0
ora$	ora #0
	sta $900e
	dec FADEOUT+1
	dec FADEOUT+1
0$	jmp $eb15	; return from IRQ


continue:
	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER+19
	bcs *

	; then fade sound
	dec FADEOUT+1
1$	lda FADEOUT+1
	bpl 1$

#if 0	; no need to fix because the next part will insert the same patch
	; Fix loader code
	lda #15
	sta $034b	; ldx #15
	lda #$b0	; bcs
	sta $0389	; $0389	bcs $0389
#endif
#if 1
	sei
	lda #$15
	sta $0314
	lda #$eb
	sta $0315
#endif
	jmp $100d



#if 0
	org $1e00
MUS
#include "../durplayer/sng-jack.dur"
#include "../durplayer/player-dur.a65"
#else
;player_init = $1ed9
player_update = $1f90
#endif

	org $2000


