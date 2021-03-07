; example mus $90(page-aligned), player $d0(if mus in zero page)

WITHVICE = 0	; put non-zero for VICE-compatible NTSC version

SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3


  processor 6502


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

COL0 = $100	; align 16, size 16
COL1 = $110
COL2 = $120
COL3 = $130
COL4 = $140
COL5 = $150
COL6 = $160
COL7 = $170
COL8 = $180

xmod = $200	; $200..$250


VMATRIX	= $1e00
CMATRIX	= $9600



#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34

COLUMNS = 29
ROWS = 14

COLS2 = 30

#else

LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26

COLUMNS = 26

#if SYSTEM & PAL
ROWS = 12
#else
ROWS = 10
#endif

COLS2 = 26;27

#endif

RASTER	= 24	;32

TIMER_VALUE = LINES * CYCLES_PER_LINE - 2	; P $5685 N $4245


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm


	ORG $1001
	DC.B $0b,$10,$ef,0	; '239 SYS4109'
	DC.B $9e,$34,$31,$30
	DC.B $39,0,0,0

	jmp start

contstart
	jsr matrixinit

	sei
	lda #$7f
	sta $913e	; disable and acknowledge interrupts / NMIs
	sta $912d
	;sta $911e	; disable NMIs (Restore key)


;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b


sync	ldx #RASTER-10	; wait for this raster line (times 2)
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

	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	;lda #$82
	;sta $911e	; enable Restore key

	cli

KEYWAIT	lda #0
	beq KEYWAIT

	; then fade scroll
	dec FADEOUT0+1
1$	lda FADEOUT0+1
	bpl 1$

	jmp contwait


nextFile:
	dc.b "3K-SHEKK"	; 8 significant chars


	; 00 - background, 01 - border, 11 - aux, 10 - character

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

#if SYSTEM & PAL
	ldy #5				; -->
wait0	dey
	bne wait0
	samepage wait0
#else
#endif

#if SYSTEM & PAL
	lda #SCRCENTER-COLUMNS
#else
	lda #SCRCENTER-COLUMNS+1	; force it!
#endif
	sta $9000
	lda #COLUMNS+128
	sta $9002
#if SYSTEM & PAL
	lda #2*ROWS+1
#else
	lda #2*ROWS+1
#endif
	sta $9003
	lda #$fc	; video matrix $1c00($1e00), chars $1000
	sta $9005

#if SYSTEM & PAL
	nop
	nop
	bit $ea
xcpos	ldx #1

	ldy #ROWS
	jmp lopo1
#else
#if WITHVICE
	ldy #8
#else
	ldy #8+13
#endif
wait0	dey
	bne wait0
	samepage wait0

	bit $ea

xcpos	ldx #1
	lda aux+14,x	;4
	sta 0		;3
	ldy #ROWS
	jmp lopo1
#endif


doscroll
	lda #1
	sta shcnt+1		; initialize counter
	clc
	adc #SCRCENTER-1-COLS2
	sta shift+1

	ldx #0
scr$	lda VMATRIX+1+ROWS*COLUMNS,x
	sta VMATRIX+0+ROWS*COLUMNS,x
	inx
	cpx #COLS2-1
	bne scr$

cnt$	lda text
	inc cnt$+1
	bne next0$
	inc cnt$+2
next0$	tax	;cmp #0
	bpl next$
	lda #<text
	sta cnt$+1
	lda #>text
	sta cnt$+2

	dec count
	bne 11$
	lda AUTO
	beq 11$
	inc KEYWAIT+1
11$
	lda #32
next$	and #$3f
0$	sta VMATRIX+ROWS*COLUMNS+COLS2-1
	rts

count:
	dc.b 3


	align 128,0
aux:
#if SYSTEM & PAL
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$98,$88,$28,$28
	dc.b $08, $28,$48,$18,$48,$28,$08,$28,$48,$58,$78,$18,$38,$58,$48,$28

	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
#else
	dc.b $08, $28,$48,$18,$48,$28,$08,$28,$48,$58,$78,$18,$38,$58,$48,$28
	dc.b $08, $28,$48,$18,$48,$28,$08,$28,$48,$58,$78,$18,$38,$58,$48,$28
	;dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	;dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
#endif
	samepage aux


contirq
#if SYSTEM & PAL
	ldx #5
0$	dex
	bpl 0$

	lda #$f0	; video matrix $1c00($1e00), ROM chars
	sta $9005

	lda #COLS2+128
	sta $9002
	lda #2*ROWS+2
	sta $9003
shift	lda #SCRCENTER-COLS2-1
	sta $9000
#else
	lda #$f0	; video matrix $1c00($1e00), ROM chars
	sta $9005

	lda #COLS2+128
	sta $9002
	lda #2*ROWS+2
	sta $9003
shift	lda #SCRCENTER-COLS2-1
	sta $9000
#endif

	jsr newcol
#if SYSTEM & NTSC
	inc 0$+1
0$:	lda #1
	lsr
	bcc noscr
#endif
shcnt	ldx #1
	dex
	bmi doscr
	stx shcnt+1
	txa
	clc
#if SYSTEM & PAL
	adc #SCRCENTER-1-COLS2
#else
	and #1
#endif
	sta shift+1

noscr
#if SYSTEM & NTSC
	lda #$e8
	STA $900e
#endif

	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$
	jmp leave

doscr	jsr doscroll

leave	lda $9111	; Stopped by button?
	and #$20
	beq 96$

	jsr plt
96$	jmp irq2





;	/* 00 - back, 01 - border, 10 - character, 11 - aux */
	.org $11a0

	dc.b $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	dc.b $50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

	dc.b $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	dc.b $50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50,$50
	dc.b $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;	.org $1200
;scrollchars:
;	ds.b 256


	align 256,0
sin70:
#if 1
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
        dc.b $7f,$7f,$7f,$7e,$7e,$7d,$7c,$7b,$7a,$79,$78,$76,$74,$73,$71,$6f
        dc.b $6c,$6a,$68,$65,$63,$60,$5d,$5b,$58,$55,$52,$4f,$4c,$49,$46,$43
        dc.b $40,$3d,$3a,$37,$34,$31,$2e,$2b,$28,$25,$23,$20,$1d,$1b,$18,$16
        dc.b $14,$11,$0f,$0d,$0c,$0a,$08,$07,$06,$05,$04,$03,$02,$02,$01,$01
        dc.b $01,$01,$01,$02,$02,$03,$04,$05,$06,$07,$08,$0a,$0c,$0d,$0f,$11
        dc.b $14,$16,$18,$1b,$1d,$20,$23,$25,$28,$2b,$2e,$31,$34,$37,$3a,$3d
#else
	dc.b $37,$34,$32,$2f,$2c,$2a,$27,$24
	dc.b $22,$1f,$1d,$1a,$18,$16,$14,$12
	dc.b $10,$0e,$0c,$0a,$09,$07,$06,$05
	dc.b $04,$03,$02,$01,$01,$00,$00,$00
	dc.b $00,$00,$00,$00,$01,$01,$02,$03
	dc.b $04,$05,$06,$07,$09,$0a,$0c,$0e
	dc.b $10,$12,$14,$16,$18,$1a,$1d,$1f
	dc.b $22,$24,$27,$2a,$2c,$2f,$32,$34
	dc.b $37,$3a,$3c,$3f,$42,$44,$47,$4a
	dc.b $4c,$4f,$51,$54,$56,$58,$5a,$5c
	dc.b $5e,$60,$62,$64,$65,$67,$68,$69
	dc.b $6a,$6b,$6c,$6d,$6d,$6e,$6e,$6e
	dc.b $6f,$6e,$6e,$6e,$6d,$6d,$6c,$6b
	dc.b $6a,$69,$68,$67,$65,$64,$62,$60
	dc.b $5e,$5c,$5a,$58,$56,$54,$51,$4f
	dc.b $4c,$4a,$47,$44,$42,$3f,$3c,$3a

	dc.b $37,$34,$32,$2f,$2c,$2a,$27,$24
	dc.b $22,$1f,$1d,$1a,$18,$16,$14,$12
	dc.b $10,$0e,$0c,$0a,$09,$07,$06,$05
	dc.b $04,$03,$02,$01,$01,$00,$00,$00
	dc.b $00,$00,$00,$00,$01,$01,$02,$03
	dc.b $04,$05,$06,$07,$09,$0a,$0c,$0e
	dc.b $10,$12,$14,$16,$18,$1a,$1d,$1f
	dc.b $22,$24,$27,$2a,$2c,$2f,$32,$34
	dc.b $37,$3a,$3c,$3f,$42,$44,$47,$4a
	dc.b $4c,$4f,$51,$54,$56,$58,$5a,$5c
	dc.b $5e,$60,$62,$64,$65,$67,$68,$69
	dc.b $6a,$6b,$6c,$6d,$6d,$6e,$6e,$6e
	dc.b $6f,$6e,$6e,$6e,$6d,$6d,$6c,$6b
	dc.b $6a,$69,$68,$67,$65,$64,$62,$60
	dc.b $5e,$5c,$5a,$58,$56,$54,$51,$4f
	dc.b $4c,$4a,$47,$44,$42,$3f,$3c,$3a
#endif

;	dc.b $08,$2a,$4c,$4c,$5d,$3b,$7f,$19,$19,$19,$7f,$3b,$5d,$4c,$4c,$2a

back0:
	dc.b $00, $02,$04,$05,$07,$01,$03,$05,$04,$02,$00,$06,$03,$01,$03,$06
	dc.b $00, $02,$04,$05,$07,$01,$03,$05,$04,$02,$00,$06,$03,$01,$03,$06
back1:
	dc.b $00, $02,$04,$07,$04,$02,$00,$02,$04,$05,$07,$01,$03,$05,$04,$02
	dc.b $00, $02,$04,$07,$04,$02,$00,$02,$04,$05,$07,$01,$03,$05,$04,$02
back2:
	;dc.b $00, $06,$06,$03,$03,$07,$07,$01,$01,$01,$07,$07,$03,$03,$06,$06
	dc.b $00, $06,$02,$04,$04,$03,$03,$01,$01,$01,$03,$03,$04,$04,$02,$06
	dc.b $00, $06,$02,$04,$04,$03,$03,$01,$01,$01,$03,$03,$04,$04,$02,$06
	samepage back0


border0:
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
	dc.b $08, $28,$28,$48,$88,$98,$78,$18,$18,$18,$78,$98,$88,$48,$28,$28
border1:
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
	dc.b $08, $28,$28,$48,$58,$38,$78,$18,$18,$18,$78,$38,$58,$48,$28,$28
border2:
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28
	dc.b $08, $28,$28,$88,$88,$98,$78,$18,$18,$18,$78,$98,$88,$88,$28,$28
	samepage border0



cnt:
	dc.b 0

newcol:
#if 1
	lda $9111	; Stopped by button?
	and #$20
	beq 111$
#endif
	inc cnt
111$	lda cnt
	and #127
	tax

	lda sin70+$00,x		; having the sin premasked would only
	and #15			; save 64 cycles / frame
	clc
	adc #<border0
	sta 11$+1

	lda sin70+$0c,x
	and #15
	adc #<back0
	sta 12$+1

	lda sin70+$18,x
	and #15
	adc #<border1
	sta 21$+1

	lda sin70+$24,x
	and #15
	adc #<back1
	sta 22$+1

	lda sin70+$30,x
	and #15
	adc #<border2
	sta 31$+1

	lda sin70+$3c,x
	and #15
	adc #<back2
	sta 32$+1

	lda sin70+$48,x
	and #15
	adc #<border0
	sta 41$+1

	lda sin70+$54,x
	and #15
	adc #<back0
	sta 42$+1

	lda sin70+$60,x
	and #15
	adc #<border1
	sta 51$+1

	lda sin70+$6c,x
	and #15
	adc #<back1
	sta 52$+1

	lda sin70+$78,x
	and #15
	adc #<border2
	sta 61$+1

	lda sin70+$04,x
	and #15
	adc #<back2
	sta 62$+1

	lda sin70+$10,x
	and #15
	adc #<border0
	sta 71$+1

	lda sin70+$1c,x
	and #15
	adc #<back0
	sta 72$+1

	lda sin70+$28,x
	and #15
	adc #<border1
	sta 81$+1

	lda sin70+$34,x
	and #15
	adc #<back1
	sta 82$+1

	lda sin70+$40,x
	and #15
	adc #<border2
	sta 91$+1

	lda sin70+$4c,x
	and #15
	adc #<back2
	sta 92$+1



	lda sin70+$20,x
	;asl
#if SYSTEM & PAL
	and #63
	clc
	adc #1
#else
	and #15
#endif
	sta xcpos+1

	ldy #16-1
10$

11$	lda border0,y
12$	ora back0,y
	sta COL0,y

21$	lda border1,y
22$	ora back1,y
	sta COL1,y

31$	lda border2,y
32$	ora back2,y
	sta COL2,y

41$	lda border0,y
42$	ora back0,y
	sta COL3,y

51$	lda border1,y
52$	ora back1,y
	sta COL4,y

61$	lda border2,y
62$	ora back2,y
	sta COL5,y

71$	lda border0,y
72$	ora back0,y
	sta COL6,y

81$	lda border1,y
82$	ora back1,y
	sta COL7,y

91$	lda border2,y
92$	ora back2,y
	sta COL8,y

	dey
	bpl 10$

	ldx #0
	ldy #0
vert$
	lda COL0+0,y	; y= 0, 16, 32, 48..
	sta C0+1+0*5,x	; x= 0, 5, 10, 15..
	lda COL0+1,y
	sta C1+1+0*5,x
	lda COL0+2,y
	sta C2+1+0*5,x
	lda COL0+3,y
	sta C3+1+0*5,x
	lda COL0+4,y
	sta C4+1+0*5,x
	lda COL0+5,y
	sta C5+1+0*5,x
	lda COL0+6,y
	sta C6+1+0*5,x
	lda COL0+7,y
	sta C7+1+0*5,x
	lda COL0+8,y
	sta C8+1+0*5,x
	lda COL0+9,y
	sta C9+1+0*5,x
	lda COL0+10,y
	sta CA+1+0*5,x
	lda COL0+11,y
	sta CB+1+0*5,x
	lda COL0+12,y
	sta CC+1+0*5,x
	lda COL0+13,y
	sta CD+1+0*5,x
	lda COL0+14,y
	sta CE+1+0*5,x
	lda COL0+15,y
	sta CF+1+0*5,x

	tya
	clc
	adc #16
	tay
	txa
	adc #5
	tax
	cpx #45
	bne vert$

	rts



matrixinit:
	ldy #2
	ldx #0
1$	iny
	cpy #6
	bne 10$
	ldy #0
10$	tya
	clc
	adc #26

	sta VMATRIX+0*COLUMNS,x
	sta VMATRIX+1*COLUMNS,x
	sta VMATRIX+2*COLUMNS,x
	sta VMATRIX+3*COLUMNS,x
	sta VMATRIX+4*COLUMNS,x
	sta VMATRIX+5*COLUMNS,x
	sta VMATRIX+6*COLUMNS,x
	sta VMATRIX+7*COLUMNS,x
	sta VMATRIX+8*COLUMNS,x
	sta VMATRIX+9*COLUMNS,x
#if ROWS > 10
	sta VMATRIX+10*COLUMNS,x
#endif
#if ROWS > 11
	sta VMATRIX+11*COLUMNS,x
#endif
#if ROWS > 12
	sta VMATRIX+12*COLUMNS,x
#endif
#if ROWS > 13
	sta VMATRIX+13*COLUMNS,x
#endif
#if ROWS > 14
	sta VMATRIX+14*COLUMNS,x
#endif
#if ROWS > 15
	sta VMATRIX+15*COLUMNS,x
#endif
	inx
	cpx #COLUMNS
	bne 1$

	ldx #0
	lda #8
3$	sta CMATRIX+0,x
	sta CMATRIX+$100,x
	dex
	bne 3$


	ldx #COLS2-1
2$	lda #1
	sta CMATRIX+ROWS*COLUMNS,x	; color memory for bottom line
#if SYSTEM & NTSC
	; Mask the bug..
	lda #0
	sta CMATRIX+ROWS*COLUMNS+COLS2,x	; color memory for bottom line
#endif
	lda #32
	sta VMATRIX+ROWS*COLUMNS,x
	dex
	bpl 2$

#if SYSTEM & PAL
	; TODO: account for scrolling (in PAL)
	lda #7
	sta CMATRIX+ROWS*COLUMNS+2
	sta CMATRIX+ROWS*COLUMNS+3
	sta CMATRIX+ROWS*COLUMNS+COLS2-3
	sta CMATRIX+ROWS*COLUMNS+COLS2-4
	lda #5
	sta CMATRIX+ROWS*COLUMNS+0
	sta CMATRIX+ROWS*COLUMNS+1
	sta CMATRIX+ROWS*COLUMNS+COLS2-1
	sta CMATRIX+ROWS*COLUMNS+COLS2-2
#else
	lda #7
	sta CMATRIX+ROWS*COLUMNS+2
	sta CMATRIX+ROWS*COLUMNS+3
	sta CMATRIX+ROWS*COLUMNS+COLS2-4
	sta CMATRIX+ROWS*COLUMNS+COLS2-5
	lda #5
	sta CMATRIX+ROWS*COLUMNS+0
	sta CMATRIX+ROWS*COLUMNS+1
	sta CMATRIX+ROWS*COLUMNS+COLS2-1
	sta CMATRIX+ROWS*COLUMNS+COLS2-2
	sta CMATRIX+ROWS*COLUMNS+COLS2-3
#endif
	jmp player_init

bmask:
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$80
	dc.b $e0,$f8,$fe,$ff, $bf,$2f,$0b,$02
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$80
	dc.b $e0,$f8,$fe,$ff, $bf,$2f,$0b,$02
amask:
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$c0
	dc.b $f0,$fc,$ff,$ff, $ff,$3f,$0f,$03
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$c0
	dc.b $f0,$fc,$ff,$ff, $ff,$3f,$0f,$03


plt:
#if 1
FADEOUT0
	lda #128	; fade out the scroll..
	bmi f0$
	lsr
	tay

	lda #0
	sta back0,y	; back0 + back1
	sta back2,y	; back2 + border0
	sta border1,y	; border1 + border2
#if SYSTEM & PAL
	lda aux,y
	and #15
	sta aux,y	; aux 4/5ths
#else
	tya
	and #31
	tax
	lda aux,x
	and #15
	sta aux,x	; aux 4/5ths
#endif
	dec FADEOUT0+1
	rts
f0$
#endif
plt$	ldy #0
	inc plt$+1
	lda sin70,y
	lsr
	clc
	adc #1

	sec
1$	sbc #24
	bcs 1$
	adc #24

	tay

	lda #$55
	ora amask+5*4,y
	eor amask+5*4,y
	ora bmask+5*4,y
	sta mask0$+1

	lda #$50
	ora amask+4*4,y
	eor amask+4*4,y
	ora bmask+4*4,y
	sta mask1$+1

	;lda #$00
	;ora amask+3*4,y
	;eor amask+3*4,y
	;ora bmask+3*4,y
	lda bmask+3*4,y
	sta mask2$+1

	lda #$55
	ora amask+2*4,y
	eor amask+2*4,y
	ora bmask+2*4,y
	sta mask3$+1

	lda #$50
	ora amask+1*4,y
	eor amask+1*4,y
	ora bmask+1*4,y
	sta mask4$+1

	;lda #$00
	;ora amask+0*4,y
	;eor amask+0*4,y
	;ora bmask+0*4,y
	lda bmask+0*4,y
	sta mask5$+1

	ldx #15
0$
mask0$	lda #0
	sta $11a0,x
mask1$	lda #0
	sta $11b0,x
mask2$	lda #0
	sta $11c0,x
mask3$	lda #0
	sta $11d0,x
mask4$	lda #0
	sta $11e0,x
mask5$	lda #0
	sta $11f0,x

	dex
	bpl 0$	;16* (6*(2+5)+5) = 16*47 = 752 = 10.6 lines

	rts




lopo1
C0	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop		;2
	lda aux+0,x	;4
	STA $900e	;4
	lda xmod-1,x	;5!
	tax		;2 also 4 bytes!
#else
	bit $ea
	lda aux+0,x	; 65 cycles
	sta $900e
#endif

C1	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+1,x	; 65 cycles
	sta $900e
#endif

C2	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+2,x	; 65 cycles
	sta $900e
#endif

C3	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+3,x	; 65 cycles
	sta $900e
#endif

C4	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+4,x	; 65 cycles
	sta $900e
#endif

C5	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+5,x	; 65 cycles
	sta $900e
#endif

C6	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+6,x	; 65 cycles
	sta $900e
#endif

C7	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+7,x	; 65 cycles
	sta $900e
#endif

C8	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+8,x	; 65 cycles
	sta $900e
#endif

C9	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+9,x	; 65 cycles
	sta $900e
#endif

CA	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+10,x	; 65 cycles
	sta $900e
#endif

CB	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+11,x	; 65 cycles
	sta $900e
#endif

CC	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+12,x	; 65 cycles
	sta $900e
#endif

CD	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop
	lda aux+0,x	;4
	STA $900e
	lda xmod-1,x
	tax	; also 4 bytes!
#else
	bit $ea
	lda aux+13,x	; 65 cycles
	sta $900e
#endif

CE	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	lda aux+0,x	;4
	STA $900e	;4
	lda #8		;2
	dey		;2
	beq lope1	;2
	bit $ea		;3=17
#else
	lda 0		;3
	dey		;2
	beq lope1	;2	; 65 cycles
	sta $900e	;4=11
#endif

CF	lda #0		;2
	sta $900f	;4
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
	lda #0
	sta $900f
#if SYSTEM & PAL
	nop		;2
	lda aux+1,x	;4
	STA $900e	;4
	inx		;2
	inx		;2
	jmp lopo1	;3=18
#else
	lda aux+15,x	;4	; 65 cycles
	sta $900e	;4
	jmp lopo1	;3=11
#endif
lope1
#if SYSTEM & NTSC
	ldy #8
	sty $900f
	sta $900e	;4=11
#else
	sta $900f
#endif
	jmp contirq


text:
	dc.b "                                             "
	dc.b " CODE BY ALBERT OF PU239..            "
	dc.b " MUSIC BY ANDERS CARLSSON             "
	dc.b " SEE THE GREAT ROM FONT IN ACTION        "
	dc.b " 19 UNIQUE SPLITS "
	dc.b "(18 VISIBLE IN NTSC) "
	dc.b "   (INCLUDES THE OVERLAY COLOR BAR)..         "
	dc.b " YOU CAN HAVE A COUPLE MORE IF YOU HAVE MEMORY..       "
	dc.b " JUST KEEP ASKING ME WHAT'S POSSIBLE...       "
	dc.b "                ", 255

contwait
	sei
	lda #<irq2
	sta $0314
	lda #>irq2
	sta $0315
	cli

	ldx #<nextFile
	ldy #>nextFile
	jsr LOADER+19
	bcs *

	; then fade sound
	dec FADEOUT+1
1$	lda FADEOUT+1
	bpl 1$

#if 1
	sei
	lda #$15
	sta $0314
	lda #$eb
	sta $0315
	lda $9124
#endif
	jmp $100d

irq2
	lda #2*ROWS+2
	sta $9003

	jsr player_update
	jsr player_update

FADEOUT	lda #128	; fade out music
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


	align 256,0
	org $1c00
MUS equ $1c00
#include "../durplayer/sng-morse.dur"
#include "../durplayer/player-dur.a65"

	.org $1e00
start
	lda #0
	sta $900f
	lda #COLUMNS+128	; video matrix at $1e00
	sta $9002
	lda #2*ROWS+2
	sta $9003
	lda $900e
	and #15
	sta $900e
	lda #$fc	; video matrix $1c00($1e00), chars $1000
	sta $9005

#if SYSTEM & PAL
	lda #RASTER
	sta $9001
	lda #SCRCENTER-COLUMNS
#else
#if WITHVICE
	lda #RASTER
#else
	lda #RASTER-1	;; For the real machine! Don't know why!?
#endif
	sta $9001
	lda #SCRCENTER-COLUMNS+1	; force it!
#endif
	sta $9000

	ldx #$50
0$	txa
	and #63
	clc
	adc #1
	sta xmod-1,x
	dex
	bne 0$

	; Fix loader code to return instead of running the code
	lda #3
	sta $034b	; ldx #15 -> ldx #3 to only clear $9000..$9003
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389

	jmp contstart

