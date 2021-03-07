
SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3


  processor 6502


; This part by Albert of Pu-239 from the original idea by Marko Mäkelä

; $1000-1143	video matrix 0			(COLUMNS*ROWS = $144)
; $1144-11ff	PU-logo, code
; $1200-1343	video matrix 1			(COLUMNS*ROWS = $144)
; $1344-13ff	PU-logo routine
; $1400-19c0	code
; $19c0-1a00	PU-logo chars
; $1a00-1bbf	charmem 0			(28*16 bytes = $1c0)
; $1bc0-1dc0	music
; $1dc0-1e00	PU-logo chars
; $1e00-1fbf	charmem 1 (charmem 0 inverted)	(28*16 bytes = $1c0)
; $1fc0-1fff	irq code



NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
#else
LINES = 261
CYCLES_PER_LINE = 65
#endif

#if SYSTEM & PAL
SCRCENTER = 34
#else
SCRCENTER = 26
#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

#if SYSTEM & PAL
RASTER	= 28	;24		; effect at RASTER + 10 (+ 1)
#else
RASTER	= 20			; effect at RASTER + 10 (+ 1)
#endif

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

masktab = 0	; $01..$81 max
table    = $0100	; $0100..$0180
twotimes = $0200	; $0200..$0240
shekkimap = $0240	; $0240..$02c0

hmask	EQU $f7
d1hor	EQU $f8
d2hor	EQU $f9
d1tmp	EQU $fa
d2tmp	EQU $fb
mask	EQU $fc		;d1	EQU $fc
;d2	EQU $fd
;count	EQU $fe

#if SYSTEM & PAL
COLUMNS		EQU 27		; Columns for the checkerboard(s)
ROWS		EQU 3*4		; Huom! Kolmella jaollinen.
#else
COLUMNS		EQU 23
ROWS		EQU 3*3		; Huom! Kolmella jaollinen.
#endif

PIXELLINES	EQU 16*ROWS/3
charmem1	EQU $1a00	; base $1800, char codes 32..63
charmem2	EQU $1e00	; base $1c00, char codes 32..63


#if >masktab
#echo "fatal: masktab must be in zero page"
#err
#endif
#if >(masktab+16*ROWS/3)
#echo "fatal: masktab must be fully in zero page - end wraps"
#err
#endif


setcolmem:
	sei	; to prevent keyboard scan to overwrite some data
	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)
	lda #$00	; disable Timer A free run
	sta $912b

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315

	ldx #0
5$	lda #$ea
	sta table,x
	lda shekkimaps,x
	sta shekkimap,x
	inx
	bpl 5$

	ldx #63
6$	txa
	asl
	sta twotimes,x
	dex
	bpl 6$


	lda #0
	ldx #16*ROWS/3
4$	sta masktab-1,x
	dex
	bne 4$

	ldx #0
	lda #0
0$	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 0$

	lda #RASTER+11
	sta $9001	; vertical centering
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	lda #0*2+1	;ROWS*2+1	; rows+2 & 16x8 chars
	sta $9003
	lda #$ce
	sta $9005
	jmp player_init
	;rts

#if SYSTEM & PAL
	;align 128,0
shekkimaps:	; for COLUMNS=27
#if COLUMNS = 27
	dc.b $00,$00,$00,$01,$07,$00,$07,$09,$00,$10,$13,$01,$08,$15,$07,$18
	dc.b $0c,$00,$1b,$11,$07,$28,$20,$18,$10,$08,$00,$31,$2b,$25,$1f,$19
	dc.b $13,$0d,$07,$01,$44,$40,$3c,$38,$34,$30,$2c,$28,$24,$20,$1c,$18
	dc.b $14,$10,$0c,$08,$04,$00,$6b,$69,$67,$65,$63,$61,$5f,$5d,$5b,$59
	dc.b $57,$55,$53,$51,$4f,$4d,$4b,$49,$47,$45,$43,$41,$3f,$3d,$3b,$39
	dc.b $37,$35,$33,$31,$2f,$2d,$2b,$29,$27,$25,$23,$21,$1f,$1d,$1b,$19
	dc.b $17,$15,$13,$11,$0f,$0d,$0b,$09,$07,$05,$03,$01,$d8,$d8,$d8,$d8
	dc.b $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8
#endif
#if COLUMNS = 23
	dc.b $00,$00,$04,$01,$04,$05,$03,$09,$04,$05,$08,$11,$03,$10,$04,$19
	dc.b $0f,$05,$20,$18,$10,$08,$00,$29,$23,$1d,$17,$11,$0b,$05,$3c,$38
	dc.b $34,$30,$2c,$28,$24,$20,$1c,$18,$14,$10,$0c,$08,$04,$00,$5b,$59
	dc.b $57,$55,$53,$51,$4f,$4d,$4b,$49,$47,$45,$43,$41,$3f,$3d,$3b,$39
	dc.b $37,$35,$33,$31,$2f,$2d,$2b,$29,$27,$25,$23,$21,$1f,$1d,$1b,$19
	dc.b $17,$15,$13,$11,$0f,$0d,$0b,$09,$07,$05,$03,$01,$b8,$b8,$b8,$b8
	dc.b $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
	dc.b $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
#endif

#endif

	org $1000+ROWS*COLUMNS

#if SYSTEM & NTSC
shekkimaps:	; for COLUMNS=27
	dc.b $00,$00,$00,$01,$07,$00,$07,$09,$00,$10,$13,$01,$08,$15,$07,$18
	dc.b $0c,$00,$1b,$11,$07,$28,$20,$18,$10,$08,$00,$31,$2b,$25,$1f,$19
	dc.b $13,$0d,$07,$01,$44,$40,$3c,$38,$34,$30,$2c,$28,$24,$20,$1c,$18
	dc.b $14,$10,$0c,$08,$04,$00,$6b,$69,$67,$65,$63,$61,$5f,$5d,$5b,$59
	dc.b $57,$55,$53,$51,$4f,$4d,$4b,$49,$47,$45,$43,$41,$3f,$3d,$3b,$39
	dc.b $37,$35,$33,$31,$2f,$2d,$2b,$29,$27,$25,$23,$21,$1f,$1d,$1b,$19
	dc.b $17,$15,$13,$11,$0f,$0d,$0b,$09,$07,$05,$03,$01,$d8,$d8,$d8,$d8
	dc.b $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8
#endif

#if SYSTEM & PAL
logo0	dc.b $0,$0,$0,$0,29,28,28,28,28,30,$0,$0,$0,$0
logo1	dc.b $0,$0,$0,29,28,28,28,28,28,28,30,$0,$0,$0
logo2	dc.b $0,$0,$0,$0,29,28,28,28,28,30,$0,$0,$0,$0
logo3	dc.b $0,$0,$0,$0,$0,31,$0,$0,31,$0,$0,$0,$0,$0
logo4	dc.b 29,28,28,28,30,$0,29,30,$0,29,28,28,28,30
logo5	dc.b 29,28,28,28,28,30,$0,$0,29,28,28,28,28,30
logo6	dc.b $0,29,28,28,30,$0,$0,$0,$0,29,28,28,30,$0
logo7	dc.b $0,$0,29,30,$0,$0,$0,$0,$0,$0,29,30,$0,$0
#endif

	; $4c bytes free
KEYWAIT	lda #0
	beq KEYWAIT

	sei
	lda #0
	sta $900f
	sta $9002
	sta $9003

	lda #<irq2	; set the raster IRQ routine pointer
	sta $314
	lda #>irq2
	sta $315
	cli
	; Fix loader code to return instead of running the code
	lda #3
	sta $034b	; ldx #15 -> ldx #3 to only clear $9000..$9003
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389

	ldx #<nextFile
	ldy #>nextFile
	jmp jumpout	; can load upto $1bc0 - crediz is now $1001-$1a15

nextFile:
	dc.b "3K-CREDI"	; 8 significant chars


	org $1200
	; video matrix 1

	org $1200+ROWS*COLUMNS

#if SYSTEM & PAL
	; $60 bytes free

div8	dc.b 80

putlogo
	dec div8
	bmi switch$
	rts
switch$	lda #0
	inc switch$+1
	bpl off$
	and #127
	cmp #14
	bcc on$
	lda #4		; if not plotting, slow down (on less than off)
	sta div8
	rts

#if SYSTEM & PAL
LROW	= 2
LCOL	= 6
#else
LROW	= 2;1
LCOL	= 6;5
#endif
START	= LROW*COLUMNS+LCOL

off$	cmp #14
	bcc off0$
	lda #10		; if not removing, slow down (off more than on)
	sta div8
	rts
off0$	tax
	;txa
	clc
	adc #32+LCOL

	ldy #8
off1$	sta $1000+0*COLUMNS+START,x
	sta $1200+0*COLUMNS+START,x
	pha
	txa
	clc
	adc #COLUMNS
	tax
	pla
	dey
	bne off1$
	rts

on$	tax
	tay
	lda #8
	sta d1tmp

on0$	lda logo0,y
	beq l1$
	sta $1000+0*COLUMNS+START,x
	sta $1200+0*COLUMNS+START,x
l1$	tya
	clc
	adc #logo1-logo0
	tay
	txa
	clc
	adc #COLUMNS
	tax
	dec d1tmp
	bne on0$
	rts
#endif



	org $1400
start
	jsr setvideomem

	jsr plotline
	;jsr volumeaux


;synchronize with the screen

sync:	ldx #RASTER	; wait for this raster line (times 2)
0$:	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$:	ldx $9004
	txa
	bit $24
#if SYSTEM & PAL
	ldx #24
#endif
#if SYSTEM & NTSC
	bit $24
	ldx #21
#endif
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$
	; now it is fully synchronized
	; 6 cycles have passed since last $9004 change
	; and we are on line 2(28+9)=74

;initialize the timers
timers:
	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126

#if SYSTEM & PAL
	nop		; make a little delay to get the raster effect to the
	nop		; right place
#endif
	bit $ea

	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	cli
	jmp KEYWAIT



; colors
col1	EQU 0	;0 blk	;0 blk		; background color
col2	EQU 2	;1 wht	;2 red		; 1st chessboard color (bottom board)
col3	EQU 6	;6 blu	;6 blu		; 2nd chessboard color (top board)
col4	EQU 4	;3 cyn	;4 pur		; mixed color (1st & 2nd together)

;	/* 00 - back, 01 - border, 10 - character, 11 - aux */
; Can not change character color between 4 possibilities -- only 2 color mems!
; Inverse video only works in non-multicolor mode.
; The only possibility is to use two character sets with inverted patterns.
;		col1        col3        col2        col4
;		col2        col4        col1        col3
;		col2        col4        col1        col3	inverted
;		col1        col3        col2        col4	inverted

;actual:	col1        col3        col2        col4
;		col2        col4        col1        col3	<->12 34
;		col3        col1        col4        col2	<->13 42
;		col4        col2        col3        col1	<->14 23


;now:		col1        col3        col2        col4
;		col3        col1        col4        col2	inv
;		col3        col1        col4        col2
;		col1        col3        col2        col4	inv



	;    original		swap 1/2, 3/4	original	swap 1/2, 3/4
bg:	dc.b 8,			8,		8,		8
aux:	dc.b 7,			7,		7,		7
cmem:	; col2/col1 + multicolor bit	; 9th bit of video matrix/colmem
	dc.b COLUMNS,		128+COLUMNS,	128+COLUMNS,	COLUMNS
	;    col2		col4		col4		col2
char:	dc.b $ce,		$cf,		$ce,		$cf


STARTD	EQU 120

c1tab	dc.b	8+0, 8+2, 8+0, 8+1, 8+0, 255
c1tab4	dc.b	$00, $20, $00, $10, $00
c2tab	dc.b	8+2, 8+4, 8+1, 8+3, 8+7
c2tab4	dc.b	$27, $47, $17, $37, $77
c3tab	dc.b	8+6, 8+5, 8+6, 8+7, 8+3
c3tab4	dc.b	$60, $50, $60, $70, $30
c4tab	dc.b	8+4, 8+3, 8+3, 8+5, 8+1
c4tab4	dc.b	$47, $37, $37, $57, $17
colorcount dc.b 0		; 0 == color change active
endcount	dc.b 3

chcolmem:
	ldx #0
	ldy #0
	cpx #COLUMNS
	bne 0$
	; X == COLUMNS
	;sty currentpat
	lda c1tab4,y
	ora c3tab,y
	sta bg+0
	sta bg+3

	lda c3tab4,y
	ora c1tab,y
	sta bg+1
	sta bg+2

	lda c4tab4,y
	;ora #15
	sta aux+0
	sta aux+3

	lda c2tab4,y
	;ora #15
	sta aux+1
	sta aux+2

	dec endcount
	bne 11$
	lda AUTO
	beq 11$
	inc KEYWAIT+1

11$	lda #0
	sta chcolmem+1
	inc colorcount
	iny
	lda c1tab,y
	bpl norst$
	ldy #0
norst$	sty chcolmem+3
	rts	; 90+12

0$	inc chcolmem+1
	bcc 1$		; X < COLUMNS
	rts

1$	lda c2tab,y
	sta $9400+0*COLUMNS,x
	sta $9400+1*COLUMNS,x
	sta $9400+2*COLUMNS,x
	sta $9400+3*COLUMNS,x
	sta $9400+4*COLUMNS,x
	sta $9400+5*COLUMNS,x
	sta $9400+6*COLUMNS,x
	sta $9400+7*COLUMNS,x
	sta $9400+8*COLUMNS,x
#if ROWS >= 10
	sta $9400+9*COLUMNS,x
#endif
#if ROWS >= 11
	sta $9400+10*COLUMNS,x
#endif
#if ROWS >= 12
	sta $9400+11*COLUMNS,x
#endif

	lda c4tab,y
	sta $9600+0*COLUMNS,x
	sta $9600+1*COLUMNS,x
	sta $9600+2*COLUMNS,x
	sta $9600+3*COLUMNS,x
	sta $9600+4*COLUMNS,x
	sta $9600+5*COLUMNS,x
	sta $9600+6*COLUMNS,x
	sta $9600+7*COLUMNS,x
	sta $9600+8*COLUMNS,x
#if ROWS >= 10
	sta $9600+9*COLUMNS,x
#endif
#if ROWS >= 11
	sta $9600+10*COLUMNS,x
#endif
#if ROWS >= 12
	sta $9600+11*COLUMNS,x
#endif
	rts	; 146+12 cycles




plotline:
	lda $9111	; VIA#1 port A
	and #$20	; fire ?
	bne auto$	; Fire not pressed -> automatic control

	; Check Joystick
	lda #$7f
	sta $9122	; DDR for port B
	lda $9120	; VIA#2 port B
	bmi 0$
	dec d1trg
0$	lda #$ff
	sta $9122	; Restore DDR
	lda $9111	; VIA#1 port A
	tay
	and #4
	bne 1$
	inc d2trg
1$	tya
	and #8
	bne 2$
	dec d2trg
2$	tya
	and #16
	bne 3$
	inc d1trg
3$	jmp 4$

auto$	ldy #0
wait$	lda #0
	beq nowait$
	dec wait$+1
	jmp 4$
nowait$	lda d1src,y
	bpl norst$
	lda d2src,y
	bmi reset$
	sta wait$+1
	inc auto$+1
	jmp 4$

reset$	lda #0
	sta colorcount	; start color change
	sta auto$+1	; restart
	beq auto$

norst$	cmp d1trg
	beq d1ok$
	bcc d1dec$	; target - current < 0  --> target < current
	inc d1trg
	jmp d1nook$
d1dec$	dec d1trg
d1nook$	lda d2src,y
	cmp d2trg
	beq 4$
	bcc d2dec$
	inc d2trg
	jmp 4$
d2dec$	dec d2trg
	jmp 4$

d1ok$	lda d2src,y
	cmp d2trg
	bne d1nook$
	inc auto$+1	; next target..
4$
	lda d1trg
	sta d1+1
	lda d2trg
	sta d2+1
        jmp plotloop


d1src:	dc.b 127,$ff,120,120,  0,120,120,  0,  6, $ff,$ff, 25, 120,120,  4,  4,  4,  4,  4,120,120,$ff
d2src:	dc.b 127,$10,  0,120,120,120,  1,  1,  2, $7f,$7f,  1, 120,100,  1, 60,  1, 60,  1,  1,120,$ff



	; doublechess by albert of pu
	; ---------------------------

	align 256,0
sinus7f:
	dc.b $7f,$7f,$7f,$7e,$7e,$7d,$7c,$7b,$7a,$79,$78,$76,$74,$73,$71,$6f
        dc.b $6c,$6a,$68,$65,$63,$60,$5d,$5b,$58,$55,$52,$4f,$4c,$49,$46,$43
        dc.b $40,$3d,$3a,$37,$34,$31,$2e,$2b,$28,$25,$23,$20,$1d,$1b,$18,$16
        dc.b $14,$11,$0f,$0d,$0c,$0a,$08,$07,$06,$05,$04,$03,$02,$02,$01,$01
        dc.b $01,$01,$01,$02,$02,$03,$04,$05,$06,$07,$08,$0a,$0c,$0d,$0f,$11
        dc.b $14,$16,$18,$1b,$1d,$20,$23,$25,$28,$2b,$2e,$31,$34,$37,$3a,$3d
        dc.b $40,$43,$46,$49,$4c,$4f,$52,$55,$58,$5b,$5d,$60,$63,$65,$68,$6a
        dc.b $6c,$6f,$71,$73,$74,$76,$78,$79,$7a,$7b,$7c,$7d,$7e,$7e,$7f,$7f

        dc.b $7f,$7f,$7f,$7e,$7e,$7d,$7c,$7b,$7a,$79,$78,$76,$74,$73,$71,$6f
        dc.b $6c,$6a,$68,$65,$63,$60,$5d,$5b,$58,$55,$52,$4f,$4c,$49,$46,$43
        dc.b $40,$3d,$3a,$37,$34,$31,$2e,$2b,$28,$25,$23,$20,$1d,$1b,$18,$16
        dc.b $14,$11,$0f,$0d,$0c,$0a,$08,$07,$06,$05,$04,$03,$02,$02,$01,$01
        dc.b $01,$01,$01,$02,$02,$03,$04,$05,$06,$07,$08,$0a,$0c,$0d,$0f,$11
        dc.b $14,$16,$18,$1b,$1d,$20,$23,$25,$28,$2b,$2e,$31,$34,$37,$3a,$3d
        dc.b $40,$43,$46,$49,$4c,$4f,$52,$55,$58,$5b,$5d,$60,$63,$65,$68,$6a
        dc.b $6c,$6f,$71,$73,$74,$76,$78,$79,$7a,$7b,$7c,$7d,$7e,$7e,$7f,$7f


	align 256,0
irq:
; irq (event)	; > 7 + at least 2 cycles of last instruction (9 to 16 total)
; pha		; 3
; txa		; 2
; pha		; 3
; tya		; 2
; pha		; 3
; tsx		; 2
; lda $0104,x	; 4
; and #xx	; 2
; beq 		; 3
; jmp ($314)	; 5
		; ---
		; 38 to 45 cycles delay at this stage

	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
	sec
	sbc $9124	; 46 to 53 cycles delay at this stage
			; 90..83/23..16 in $9124 for PAL/NTSC
	; A = 0..7	0=wait 7 cycles .. 7=wait 0 cycles
	sta *+4
	bne *+2
	nop
	lda #$a9
	lda #$a9
	lda #$a9
	bit $ea
	; now we are synchronized 18 cycles from the IRQ

	samepage irq

#if SYSTEM & NTSC
	pha
	pla
	pha
	pla
#endif

	lda #ROWS*2+1	; rows+2 & 16x8 chars
	sta $9003

	pha
	pla
	nop
	bit $ea


#if SYSTEM & PAL
	lda sinptr+1	;4
	sec		;2
	sbc #1		;2
	and #127	;2
	sta sinptr+1	;4
	lda #0		;2
	sta acc+1	;4=20
#else
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla
#endif
	lda #$ea
	sta cmd4
d2	lda #0
	and #$7f
	sta d2_1+1
	sta d2_2+1
d1	lda #0
	and #$7f
	sta d1_1+1
	sta d1_2+1
	tax
	lda shekkimap,x	; move the origo
	lsr
	sta d1hor
	lda #0
	rol
	sta hmask

soff_2	lda #0
	sta d1tmp
soff_1	lda #0
	sta d2tmp
	lda #0
	sta d2hor
	sta count+1
smask	lda #2		; starting mask
	sta mask

	; The start of the 3-line loop (213 cycles in PAL, 195 in NTSC)
rloop

#if SYSTEM & PAL
	bit $ea		;3

acc	lda #0		;2
sinptr	adc sinus7f,x	;4
	sta acc+1	;4
	lda mask	;3
	bcs ok$		;2/3
	ldy d1tmp	;3
	jmp ok2$	;3
ok$	ldy d1tmp	;3
	iny		;2
ok2$
	;24
#else
	lda mask	;3	; First handle the vertical patterns
	ldy d1tmp	;3
	; 6
#endif

	beq d1old	;2/3
	dey		;2
	jmp ov11	;3
d1old	ldy #STARTD	;2
	eor #2		;2
ov11	sty d1tmp	;3=13

	ldy d2tmp	;3
cmd4	nop		;2	nop/inx
	beq d2old	;2/3
	dey		;2
	jmp ov12	;3
d2old	ldy #STARTD	;2
	eor #1		;2
ov12	sty d2tmp	;3=15

	samepage rloop

	sta mask	;3	; Remember the current mask
	tax		;2	; Use the current mask
	lda aux,x	;4
	sta 3$+1	;4
	ldy cmem,x	;4
	lda bg,x	;4
3$	ldx #0		;2
	sty $9002	;4	; colmem & columns
	sta $900f	;4	; screen & border
	lda mask	;3
	ora #$ce	;2	; 0->ce, 1->cf, 2->ce, 3->cf
	sta $9005	;4	; charmem
	stx $900e	;4	; aux color & volume
	; 17 cycles time to change the registers for PAL (71-2*27) -- 17 cycles used here
	; 19 cycles time to change the registers for NTSC
	; (from the last cycle of the first store to the last cycle of the last store)
	; border color change should occur in the middle (when border not visible)
	; character memory and color memory must be valid before the first fetch!
	; but auxiliary color can be updated just on time!

	; Then handle the horizontal patterns -- calculate 2 pixels


	lda hmask	;3	; starting mask for horizontal plotting
	ldx d1hor	;3
horiz	beq d1_1	;2/3
	dex		;2
	jmp ov13	;3
d1_1	ldx #STARTD	;2
	eor #1		;2
ov13
	ldy d2hor	;3
cmd0	nop		;2	; nop/iny
	beq ov14_1	;2/3
	dey		;2
	jmp ov14	;3
ov14_1	eor #2		;2
d2_1	ldy #STARTD	;2
ov14	sta newmsk+1	;4

cmd1	nop		;2	; nop/iny
	beq d2_2	;2/3	; Y flags
	dey		;2
	jmp ov16	;3
d2_2	ldy #STARTD	;2
	eor #2		;2
ov16	sty d2hor	;3

#if 1
	cpx #0		;2
	beq d1_2	;2/3
	dex		;2
	jmp ov15	;3
d1_2	ldx #STARTD	;2
	eor #1		;2
ov15	stx d1hor	;3=12/12
#else
	dex	;2
	bmi d1_2	;2/3
	nop	;2
	jmp ov15	;3
d1_2	ldx #STARTD	;2
	eor #1	;2
ov15	stx d1hor	;3=12/12
#endif
	sta hmask	;3

	samepage horiz
	; Makes certain above conditional jumps don't cross the page boundary


	; Oh my.. These 25 cycles were squeezed out from the above code
	; (rloop). The thing is: I don't have anything else I could do
	; here, so the PAL mode doesn't gain anything, unlike originally.
	; Of course it is nice that PAL and NTSC both gain the 4.4 lines
	; (the work done here).

	; 24 more cycles saved.. It seems that 4 more are needed to make
	; a working horizontal flag-wave thingy.

	; 4 more saved .. now enough for waves
	; Second wave also fits, but generating two tables is too slow.
	; Would need 2 cycles more for static tables. (for NTSC one table)
	; Static tables don't work so well.. no 'antialias'.
	; So, have the other wave on both axis instead.

	; 4 more cycles saved by rearranging color tables and using
	; the twotimes-table. 3 more saved by rearranging the color/memory
	; changes -- now there is no safety margin for PAL. Damn, an error
	; has been made in the NTSC version.. 2 cycles too much added, thus
	; I'm still 1 cycle short of getting one 2-D wave for NTSC too!
	; NTSC has 18 cycles per 3 lines less time than PAL (and not 16)!
	; 1 cycle saved by placing masktable to zeropage -> 2D in NTSC also.

count	ldx #0		;2	; line counter
	lda masktab-1,x	;4	; 2 previous pixels (4 bits)
	asl		;2
	asl		;2
newmsk	ora #0		;2	; add one pixel (2 bits)
	asl		;2
	asl		;2
	ora hmask	;3	; add another pixel (2 bits)
	sta masktab,x	;4	; every second one is used in the plotting routine
				; XXAB ABCD CDEF EFGH GHIJ IJKL KLMN
; masktab to zeropage -> 1 cycle saved on sta masktab,x

	ldy twotimes,x	;4
	lda table,y	;4
	sta cmd0	;4
	lda table+1,y	;4
	sta cmd1	;4
	lda table,x	;4
	sta cmd4	;4

	inx			;2
	stx count+1		;4
	cpx #PIXELLINES		;2
	beq 9$		; 2/3
	jmp rloop	;3

9$:	lda d1_1+1
	sta d1old+1
	lda d2_1+1
	sta d2old+1

#if SYSTEM & PAL
	ldx #11
#else
	ldx #9
#endif
	dex
	bne *-1

	ldx #8
	lda #0	;COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	stx $900f

	jsr plotline


	lda colorcount
	bne 10$			; 0 == color change active
	jsr chcolmem	; 158	; 316 = 4.5 lines
	jmp 11$
10$
#if SYSTEM & PAL
	jsr putlogo
#endif
11$
	lda #SCRCENTER-COLUMNS	; centered
	sta $9000	; horizontal centering
	lda #COLUMNS
	sta $9002	; columns + 9th bit of video matrix/color memory
	;lda #$ce
	;sta $9005	; base $1800

	;jsr volumeaux

;createtable:
	ldx #COLUMNS-1
	lda #$ea
0$
	sta table+0*COLUMNS,x
	sta table+1*COLUMNS,x
	sta table+2*COLUMNS,x
	sta table+3*COLUMNS,x
	dex
	bpl 0$			;=20*27 = 540 = 7.6 lines

	inc cnt$+1
cnt$	lda #0
	and #$7f
	sta s1$+1
	sta s2$+1

	ldx #0
	txa	;lda #0
	clc
2$
s1$	adc sinus7f,x	;4
	bcc 31$		;2/3
	tay		;2
	lda #$c8	;2	inx = $e8 iny = $c8
	sta table,x	;5
	tya		;2
31$	inx		;2=9

s2$	adc sinus7f,x	;4
	bcc 32$		;2/3
	tay		;2
	lda #$c8	;2	inx = $e8 iny = $c8
	sta table,x	;5
	tya		;2
32$	inx		;2=9

	cpx #4*COLUMNS	;2
	bne 2$		;=5+4*9 = 1242 =~ 17.5 lines
			; 7.6 + 18.6 =~ 26.2 lines	originally 44
			; +10 cycles for each iny	27 = 1/4 -> 3.8 lines


100$
	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$
	;dec $900f
	;inc $900f

#if SYSTEM & PAL
	jsr player_update
	jsr player_update
#else
	jsr player_update
#endif
	jmp $eb18	; return from IRQ




plotloop:
	; This plotting loop could be reversed, but it would only gain 28 cycles
	ldy #0
	ldx #0
	clc
loop2:
	lda masktab+1,y
	sta charmem1+0,x
	sta charmem1+1,x
	sta charmem1+2,x
	sta charmem1+3,x
	sta charmem1+4,x
	sta charmem1+5,x
	sta charmem1+6,x
	sta charmem1+7,x
	sta charmem1+8,x
	sta charmem1+9,x
	sta charmem1+10,x
	sta charmem1+11,x
	sta charmem1+12,x
	sta charmem1+13,x
	sta charmem1+14,x
	sta charmem1+15,x
	eor #$ff
	sta charmem2+0,x
	sta charmem2+1,x
	sta charmem2+2,x
	sta charmem2+3,x
	sta charmem2+4,x
	sta charmem2+5,x
	sta charmem2+6,x
	sta charmem2+7,x
	sta charmem2+8,x
	sta charmem2+9,x
	sta charmem2+10,x
	sta charmem2+11,x
	sta charmem2+12,x
	sta charmem2+13,x
	sta charmem2+14,x
	sta charmem2+15,x

	lda masktab+28+1,y
	sta charmem1+16*14+0,x
	sta charmem1+16*14+1,x
	sta charmem1+16*14+2,x
	sta charmem1+16*14+3,x
	sta charmem1+16*14+4,x
	sta charmem1+16*14+5,x
	sta charmem1+16*14+6,x
	sta charmem1+16*14+7,x
	sta charmem1+16*14+8,x
	sta charmem1+16*14+9,x
	sta charmem1+16*14+10,x
	sta charmem1+16*14+11,x
	sta charmem1+16*14+12,x
	sta charmem1+16*14+13,x
	sta charmem1+16*14+14,x
	sta charmem1+16*14+15,x
	eor #$ff
	sta charmem2+16*14+0,x
	sta charmem2+16*14+1,x
	sta charmem2+16*14+2,x
	sta charmem2+16*14+3,x
	sta charmem2+16*14+4,x
	sta charmem2+16*14+5,x
	sta charmem2+16*14+6,x
	sta charmem2+16*14+7,x
	sta charmem2+16*14+8,x
	sta charmem2+16*14+9,x
	sta charmem2+16*14+10,x
	sta charmem2+16*14+11,x
	sta charmem2+16*14+12,x
	sta charmem2+16*14+13,x
	sta charmem2+16*14+14,x
	sta charmem2+16*14+15,x

	txa
	;clc		; C clear
	adc #16
	tax
	iny
	iny
#if SYSTEM & PAL
	cpy #28		;COLUMNS
#else
	cpy #28
#endif
	beq *+5
	jmp loop2	; 399 cycles/loop  14* -> 5586 cycles = 79 lines
	rts

d1trg:
	dc.b STARTD
d2trg:
	dc.b STARTD

#if 0
volumeaux
	; Music routine may change volume - 900e
	lda $900e
	and #15		; just the volume
	sta 1$+1
	ldx #3
0$	lda aux,x
	and #$f0	; strip off volume
1$	ora #0		; ORA volume
	sta aux,x
	dex
	bpl 0$
	rts
#endif




	; logo chars
	org $1800+$1c0

	;00 01 10 11	$1b
	;01 00 11 10	$4e

	dc.b $11,$ee,$bb,$ee,$bb,$ee,$bb,$ee
	dc.b $bb,$ee,$bb,$ee,$bb,$ee,$bb,$44

	dc.b $11,$6e,$3b,$6e,$3b,$6e,$3b,$6e
	dc.b $3b,$6e,$3b,$6e,$3b,$6e,$3b,$44

	dc.b $11,$ec,$b9,$ec,$b9,$ec,$b9,$ec
	dc.b $b9,$ec,$b9,$ec,$b9,$ec,$b9,$44

	dc.b $11,$6c,$39,$6c,$39,$6c,$39,$6c
	dc.b $39,$6c,$39,$6c,$39,$6c,$39,$44



	org $1a00
	; Open space - character mem 0

setvideomem:
	jsr setcolmem		; ** Overwritten by setvideomem/plotline

	ldx #COLUMNS-1
1$:	txa
	ora #32
	sta $1000+0*COLUMNS,x	; video matrix 0
	sta $1000+1*COLUMNS,x
	sta $1000+2*COLUMNS,x
	sta $1000+3*COLUMNS,x
	sta $1000+4*COLUMNS,x
	sta $1000+5*COLUMNS,x
	sta $1000+6*COLUMNS,x
	sta $1000+7*COLUMNS,x
	sta $1000+8*COLUMNS,x
	sta $1000+9*COLUMNS,x
	sta $1000+10*COLUMNS,x
	sta $1000+11*COLUMNS,x

	sta $1200+0*COLUMNS,x	; video matrix 1
	sta $1200+1*COLUMNS,x
	sta $1200+2*COLUMNS,x
	sta $1200+3*COLUMNS,x
	sta $1200+4*COLUMNS,x
	sta $1200+5*COLUMNS,x
	sta $1200+6*COLUMNS,x
	sta $1200+7*COLUMNS,x
	sta $1200+8*COLUMNS,x
	sta $1200+9*COLUMNS,x
	sta $1200+10*COLUMNS,x
	sta $1200+11*COLUMNS,x
	dex
	bpl 1$

	rts


	org $1a00+$1c0


	;org $1c00


MUS
#if SYSTEM & PAL
SPD eqm 3	;2					; song speed
#else
SPD eqm 2					; song speed
#endif

#include "../durplayer/sng-tetris.dur"
#include "../durplayer/player-dur.a65"



	; logo chars
	org $1c00+$1c0

	dc.b $11,$ee,$bb,$ee,$bb,$ee,$bb,$ee
	dc.b $bb,$ee,$bb,$ee,$bb,$ee,$bb,$44

	dc.b $11,$6e,$3b,$6e,$3b,$6e,$3b,$6e
	dc.b $3b,$6e,$3b,$6e,$3b,$6e,$3b,$44

	dc.b $11,$ec,$b9,$ec,$b9,$ec,$b9,$ec
	dc.b $b9,$ec,$b9,$ec,$b9,$ec,$b9,$44

	dc.b $11,$6c,$39,$6c,$39,$6c,$39,$6c
	dc.b $39,$6c,$39,$6c,$39,$6c,$39,$44


	org $1e00
	; Open space - character mem 1


	org $1e00+$1c0

#if 0
KEYWAIT	lda #0
	beq KEYWAIT

	ldx #<nextFile
	ldy #>nextFile
	jmp LOADER

nextFile:
	dc.b "3K-CREDI"	; 8 significant chars

#else

irq2	lda $9124
#if SYSTEM & PAL
	jsr player_update
#endif
	jsr player_update

FADEOUT	lda #128	; fade out music, continue the scroll..
	bmi 0$
	lsr
	lsr
	lsr
	lsr
	sta $900e
	dec FADEOUT+1
	dec FADEOUT+1
0$	jmp $eb18	; return from IRQ


jumpout
	jsr LOADER+19
0$	bcs 0$		; failed to load!

	; then fade sound
	dec FADEOUT+1

1$	lda FADEOUT+1
	bpl 1$

	sei
	lda #$7f
	sta $912e	; disable and acknowledge (timer) interrupts
	sta $912d	; because we don't set the system IRQ vector
	; Fix loader code
	; We loaded the last part, so don't bother after all..
	;lda #15
	;sta $034b	; ldx #15
	;lda #$b0	; bcs
	;sta $0389	; $0389	bcs $0389
	jmp $100d
#endif

