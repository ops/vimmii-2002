
SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3


  processor 6502


offset = $fa

;plotplasma		; $100..$1d2
plasmacols = $200	; $200..$22e
;irq + matrix0/1	; $230..$2f5
; $1000-1200	Video matrix 0
; $1200-1400	Video matrix 1
; $1400-1600	plasma chars, 32 16-line chars
; $1600-1800	plasma chars, lower half is +1
; $1800-1a00	plasma chars, lower half is -1
; $1a00-1b80	sine tables
; $1b80-2000	code, music


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34
RASTER	= 14	; TOPPOS

COLUMNS = 28
ROWS = 18	; 504

#else
LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26
RASTER  = 8;4	; TOPPOS

COLUMNS = 26
ROWS = 15	; 390

#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm



; The BASIC line - Note: it is overwritten by later

	.org $1001	; for the un-expanded Vic-20
basic	.word 0$	; link to next line
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

start
	lda #0
	sta offset
	sta $900f
	;sta $9002	; 0 columns
	;sta $9003	; 0 rows
	;lda #15
	;sta $900e
	lda #$cd	; video matrix $1000, chars $1400
	sta $9005

	lda #RASTER+4
	sta $9001
	lda #SCRCENTER-COLUMNS
	sta $9000

	lda #0
	tax
1$	sta $9400,x
	sta $9500,x
	sta $9600,x
	sta $9700,x
	dex
	bne 1$

	; 00 - background, 01 - border, 11 - aux, 10 - character
	ldx #8*16
0$	lda plasmaimg+0 -1,x		;00->01
	sta plasmaimg+4*8*16-1,x
	sta plasmaimg+8*8*16-1,x
	clc
	adc #$55
	and #$aa
	clc
	adc #$55
	sta plasmaimg+1*8*16 -1,x	;01->11		00>01>00>01, 01>10>10>11
	sta plasmaimg+5*8*16-1,x
	sta plasmaimg+9*8*16-1,x

	lda plasmaimg+0 -1,x
	clc
	adc #$aa
	eor #$55
	sta plasmaimg+2*8*16 -1,x	;	00>10>11, 01>11>10
	sta plasmaimg+6*8*16-1,x
	sta plasmaimg+10*8*16-1,x

	lda plasmaimg+0 -1,x
	asl
	eor #$aa
	sta plasmaimg+3*8*16 -1,x	;	00>00>10, 01>10>00
	sta plasmaimg+7*8*16-1,x
	sta plasmaimg+11*8*16-1,x

	dex
	bne 0$

	ldx #8*16		; does not need to be prefilled
2$	lda plasmaimg+0*8*16 +7,x	; copy from 8 bytes upwards ->
	sta plasmaimg+4*8*16 -1,x	; first 8 bytes of character are +-0
	lda plasmaimg+1*8*16 +7,x	; last 8 bytes +1
	sta plasmaimg+5*8*16 -1,x
	lda plasmaimg+2*8*16 +7,x
	sta plasmaimg+6*8*16 -1,x	; note: the last character will have
	lda plasmaimg+3*8*16 +7,x	; random data in the last 8 bytes, but
	sta plasmaimg+7*8*16 -1,x	; this character is not used on-screen
	dex
	bne 2$

#if 1
	ldx #0		; needs to be prefilled
3$	lda plasmaimg+0*8*16 -8,x	; copy from 8 bytes down
	sta plasmaimg+8*8*16 +8,x	; first 8 bytes of character are +-0
	lda plasmaimg+1*8*16 -8,x	; last 8 bytes are -1
	sta plasmaimg+9*8*16 +8,x
	lda plasmaimg+2*8*16 -8,x
	sta plasmaimg+10*8*16+8,x	; note: the first character will have
	lda plasmaimg+3*8*16 -8,x	; random data in the last 8 bytes, but
	sta plasmaimg+11*8*16+8,x	; this character is not used on-screen
	inx
	txa
	and #7
	bne 4$
	txa
	clc
	adc #8
	tax
4$	cpx #8*16
	bne 3$

	ldx #8-1
5$	lda plasmaimg+8*8*16-16,x
	sta plasmaimg+8*8*16-8,x	; fix the last 8 bytes
	lda plasmaimg+8*8*16+8,x
	sta plasmaimg+8*8*16,x		; fix the first 8 bytes
	dex
	bpl 5$
#endif

	sei
	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

;synchronize with the screen
	; If the timer is running, wait for it to finish..
;	lda $912b
;	and #$40
;	beq sync
	lda #0		; disable Timer A free run
	sta $912b

#if RASTER < 10
sync	ldx #LINES/2+RASTER-10	; wait for this raster line (times 2)
#else
sync	ldx #RASTER-10	; wait for this raster line (times 2)
#endif
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$	ldx $9004
	txa
	bit $24
#if SYSTEM & PAL
	ldx #24
#else
	bit $24
	ldx #21
#endif
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$


	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126
	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change
pointers:
	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	;lda #$82
	;sta $911e	; enable Restore key

	ldx #46-1
0$	lda plasmacolss,x
	sta plasmacols,x
	dex
	bpl 0$

	ldx #0
1$	lda matrixsrc,x
	sta matrix0,x
	inx
	cpx #matrixsize
	bne 1$

	ldx #0
2$	lda plotsrc,x
	sta plotplasma,x
	inx
	cpx #plotsize
	bne 2$

	jsr player_init

	lda #0
	tax
	jmp continit

plasmacolss:
	dc.b $0,$0,$0,$2,$4,$c,$1,$c,$4,$2,$0,$6,$e,$3,$1,$1
	dc.b $3,$e,$6,$0,$5,$d,$3,$b,$1,$b,$3,$d,$5,$0,$0,$2
	dc.b $8,$4,$9,$1,$1,$1,$9,$4,$8,$2
	dc.b $0,$0,$0

matrixsrc:
#rorg $230		; $230..$2f5
matrix0
	ldx #127
2$	sta $9400,x
	sta $9480,x
	sta $9500,x
	sta $9580,x
	dex
	bpl 2$

	lda #>$1000
	bne psin	; jump always


matrix1
	ldx #127
2$	sta $9600,x
	sta $9680,x
	sta $9700,x
	sta $9780,x
	dex
	bpl 2$	; 3200 cycles = 45 lines (vs double size 40.5 lines)

	lda #>$1200
psin	pha

rlen1	ldx #2*ROWS-1
rstrt1	ldy #10
rlp1	lda sin7,y
	sta rowsin,x
	tya
	clc
rows1	adc #2
	tay
	dex
	bpl rlp1

	ldx #0
0$	lda rowsin,x
	cmp rowsin+1,x
	beq 1$
	bcc 2$		; next row larger -> +1*32
	; next row smaller -> +2*32
	adc #31		; +C
2$	adc #32
	sta rowsin,x
1$	inx
	inx
	cpx #2*ROWS	; forwards so that we don't overwrite the data
	bne 0$		; before processing it!


looppi	lda #0
	sta trg$+1
	pla
	sta trg$+2	; MATRIX hi

	ldx #0
outer$	lda offset	;3
	clc		;2
	adc rowsin,x	;4
	sta rowoff$+1	;4

	ldy #COLUMNS-1	;2
loop$	lda colsin,y		;4
	clc			;2
rowoff$	adc #0			;2
trg$	sta $1000+0*COLUMNS,y	;5	; larger.vs.smaller test should be here
	dey			;2	; but can't be done for lack of time..
	bpl loop$		;3=18  *28 = 504

	lda trg$+1	;4
	clc		;2
	adc #COLUMNS	;2
	sta trg$+1	;4
	bcc noo1$	;3
	inc trg$+2
noo1$
	inx		;2
	inx
	cpx #2*ROWS	;2
	bne outer$	;3 = 543  *18 = 138 lines
	rts



irq	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
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

	lda $9002
	and #$80
	sta $9002	; 0 columns
	lda #0
	sta $900f	; black border

xpos	lda #SCRCENTER-COLUMNS
	sta $9000
	lda $900e
	and #15
aux	ora #0
	sta $900e

	nop
	jmp dostuff

#rend
matrixsize = . - matrixsrc

plotsrc:
#rorg $100			; $100..$1d2
plotplasma
	; Update plasma
	lda $9111	; VIA#1 port A
	and #$10	; sw2?
	beq rotplasma

cntr$	lda #0
	inc cntr$+1
	and #127
	tay
	lda sin15,y	; 0..15
	lsr
	clc
	adc #1
	sta rows1+1

cntc$	lda #$30
	inc cntc$+1
	bne not0$
	lda #7		; slightly different speed than row counter
	sta cntc$+1
not0$	lsr		; 0..15 only
	tay
	lda sin15,y
	clc
	adc sin7,y
	sta cols1+1

	lda cstrt1+1
	clc
	adc #4
	sta cstrt1+1

	lda rstrt1+1
	clc
	adc #3
	sta rstrt1+1

	ldx #COLUMNS-1
cstrt1	ldy #10
clp1	lda sin7,y
	sta colsin,x
	tya
	clc
cols1	adc #3
	tay
	dex
	bpl clp1

rotplasma
	; Update pointers
	lda $9111	; VIA#1 port A
	and #$20	; fire?
	beq noupdate$

	inc offset
	lda offset
	and #7
	bne noupdate$
	ldy colcount
	iny
	cpy #42
	bcc ok$

	dec count
	bne 11$
	ldy AUTO
	beq 11$
	inc KEYWAIT+1
11$
	ldy #0
ok$	sty colcount
noupdate$
	lda offset
	and #7
	sta offset

	ldy colcount
	lda plasmacols+1,y
	ora plasmacols+3,y
	and #8
	bne swap$

	lda plasmacols+0,y	; background
	asl
	asl
	asl
	asl
	ora plasmacols+1,y	; border
	sta back+1
	lda plasmacols+2,y	; aux
	ldx plasmacols+3,y	; character
	jmp chk$

swap$	lda offset		; border or char colors >7, use images 24..47
	clc
	adc #8
	sta offset

	lda plasmacols+3,y	; background
	asl
	asl
	asl
	asl
	ora plasmacols+0,y	; border
	sta back+1
	lda plasmacols+1,y	; aux
	ldx plasmacols+2,y	; character

chk$	asl
	asl
	asl
	asl
	sta aux+1
	txa
	ora #8
	;sta m0col+1
	;sta m1col+1

	ldx $9002
	bmi chk1$
	jmp matrix1	; m1col in A
chk1$	jmp matrix0	; m0col in A
#rend
plotsize = . - plotsrc

	.org $1400
plasmaimg:
	dc.b $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
	dc.b $40,$00,$04,$00,$40,$00,$04,$00, $40,$00,$04,$00,$40,$00,$04,$00
	dc.b $44,$00,$11,$00,$44,$00,$11,$00, $44,$00,$11,$00,$44,$00,$11,$00
	dc.b $11,$04,$11,$40,$11,$04,$11,$40, $11,$04,$11,$40,$11,$04,$11,$40
	dc.b $11,$44,$11,$44,$11,$44,$11,$44, $11,$44,$11,$44,$11,$44,$11,$44
	dc.b $11,$45,$11,$54,$11,$45,$11,$54, $11,$45,$11,$54,$11,$45,$11,$54
	dc.b $11,$55,$44,$55,$11,$55,$44,$55, $11,$55,$44,$55,$11,$55,$44,$55
	dc.b $15,$55,$51,$55,$15,$55,$51,$55, $15,$55,$51,$55,$15,$55,$51,$55

	;dc.b $00,$00,$00,$00,$00,$00,$00,$00, $40,$00,$04,$00,$40,$00,$04,$00
	;...

	;dc.b $40,$00,$04,$00,$40,$00,$04,$00, $00,$00,$00,$00,$00,$00,$00,$00
	;...

	.org $1400+3*$200

sin7	dc.b $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
	dc.b $05,$05,$05,$05,$05,$05,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	dc.b $06,$06,$06,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
	dc.b $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
	dc.b $08,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
	dc.b $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$06,$06
	dc.b $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$05,$05,$05,$05,$05
	dc.b $05,$05,$05,$05,$05,$05,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
	dc.b $04,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$02,$02,$02,$02,$02
	dc.b $02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	dc.b $01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01
	dc.b $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02
	dc.b $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03

sin15   dc.b $00,$00,$00,$01,$01,$01,$02,$02,$03,$03,$03,$04,$04,$04,$05,$05
	dc.b $05,$06,$06,$06,$07,$07,$07,$08,$08,$08,$09,$09,$09,$0a,$0a,$0a
	dc.b $0a,$0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0d,$0e,$0e
	dc.b $0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e
	dc.b $0e,$0e,$0e,$0d,$0d,$0d,$0d,$0d,$0c,$0c,$0c,$0c,$0b,$0b,$0b,$0b
	dc.b $0a,$0a,$0a,$0a,$09,$09,$09,$08,$08,$08,$07,$07,$07,$06,$06,$06
	dc.b $05,$05,$05,$04,$04,$04,$03,$03,$03,$02,$02,$01,$01,$01,$00,$00

rowsin	ds.b 2*ROWS+2
colsin	ds.b COLUMNS


count:
	dc.b 10


dostuff:
#if SYSTEM & PAL
	ldx #113
0$	dex
	bne 0$
#else
	ldx #108	;+52
0$	dex
	bne 0$
	nop
#endif

	lda $9002
	and #$80
	eor #COLUMNS+$80
back	ldx #0
	sta $9002
	stx $900f

	lda #2*ROWS+1
	sta $9003

#if SYSTEM & NTSC
	ldx #10
0$	dex
	bne 0$
#endif

	; Update pointers
	lda $9111	; VIA#1 port A
	and #$08
	beq freeze$

	ldx #2*ROWS
	lda #RASTER
	cmp $9001
	bne 2$
freeze$	lda #RASTER+4
	ldx #2*ROWS-1
2$	sta $9001
	stx rlen1+1


	jsr plotplasma
	jsr putlogo

	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$
#if SYSTEM & PAL
	jsr player_update
#endif
	jsr player_update
	jmp $eb18



colcount:
	dc.b 0


stash = $fb
cnt   = $fc

logomem	ds.b ROWS
logotext
	dc.b "     PU-239 PRESENTS PLASMA "
	dc.b $ff

putlogo

plot$	lda #ROWS
	sta cnt

logor$	ldx #0		; start at this source line

	lda #$94
	ldy $9002
	bmi c1$
	lda #$96
c1$	sta trg$+2
	lda #9
	sta trg$+1

outer$	lda logomem,x
	sta stash

	inx
	cpx #ROWS
	bne in1$
	ldx #0
in1$
	lda back+1
	and #7

	ldy #7
inner$	ror stash
	bcc 0$
trg$	sta $9400+9,y
0$	dey
	bpl inner$

	lda trg$+1
	clc
	adc #COLUMNS
	sta trg$+1
	bcc same$
	inc trg$+2
same$
	dec cnt
	bne outer$

	lda $9001
	cmp #RASTER+4
	beq noupd$

row$	ldy #0
src$	lda $8000+8*32,y
	sta logomem,x
	lsr
	ora logomem,x
	sta logomem,x

	inx
	cpx #ROWS
	bne in2$
	ldx #0
in2$	stx logor$+1

	iny
	cpy #8
	bne notnext$

ptr$	ldx #0
	inx
	lda logotext-1,x
	bpl asc$
	ldx #0
	lda #32
asc$	stx ptr$+1
	asl
	asl
	asl
	sta src$+1
	lda #$40
	rol
	sta src$+2
	ldy #0
notnext$
	sty row$+1
noupd$	rts


	; 00 - background, 01 - border, 11 - aux, 10 - character


	org $1cca


nextFile:
	dc.b "3K-SPLIT"	; 8 significant chars

continit:
1$	sta $1000,x
	sta $1100,x
	sta $1200,x
	sta $1300,x
	inx
	bne 1$

	lda $9124
	cli
KEYWAIT	lda #0
	beq KEYWAIT

#if 1
	lda LOADER
	cmp #$78
10$	bne 10$
#endif
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
	lda #15
	sta $034b	; ldx #15
	lda #$b0	; bcs
	sta $0389	; $0389	bcs $0389
	jmp $100d


irq2
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
0$	jmp $eb15	; return from IRQ, ack timer interrupt



	org $1d50
MUS
;#include "../durplayer/dur-axelf.sng"
;#include "../durplayer/dur-action.sng"
;#include "../durplayer/dur-enjoy.sng"
;#include "../durplayer/dur-pacster.sng"
#if SYSTEM & PAL
SPD eqm 3					; song speed
#else
SPD eqm 2					; song speed
#endif

#include "../durplayer/mule.dur"
#include "../durplayer/player-dur.a65"

