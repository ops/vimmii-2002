
SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3


  processor 6502


DYCPCOLORS = 1

; $0100-013d	dycpchars, dycpoffset, dycpptr
; $0200-02ff	sinus
; $1000-121c	Video matrix
; $121c-1300	tables, code
; $1300-1500	DYCP data 1, columns  0-31, char codes $60-$9f (8-line, base $1000)
; $1500-1700	DYCP character font (32 9-line chars with 7 empty lines)
; $1700-1900	DYCP character font shifted >> 2
; $1900-2000	tables, code, music
; $9680-96df	dycpcolors



NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34
RASTER	= 46
#else
LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26
RASTER  = 32	; 40
#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2

#if SYSTEM & PAL
DYCPW	EQU	30	; DYCP width (height = 16 chars) -> 32+30*16=512
#else
DYCPW	EQU	25;26	; Maximum NTSC width 25 + 1 (-> 32+26*16=448)
#endif



#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm



; The BASIC line - Note: it is overwritten by the techtech data later

	.org $1001	; for the unexpanded Vic-20
basic	.word 0$	; link to next line
	.word 1997	; line number
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

	org $1010
start:
	sei	; so that keyboard scan does not destroy $28d/$28e
#if 1
	lda #SCRCENTER-DYCPW	; centered
	sta $9000
#endif
	ldy #DYCPW	; e.g. 30
	sty $9002
#if SYSTEM & PAL
	ldy #2*16+4	; 16	rows
#else
	ldy #2*12+4	; 12	rows
#endif
	sty $9003	; 8x8 chars
	lda #RASTER
	sta $9001
	ldx #$cc	; $cc = base $1000, charset $1000, $ee = base $1800, charset $1800
	stx $9005
	lda #8
	sta $900f
	lda #7+16*3
	sta $900e

	ldx #0
	ldy #255
2$	lda sinuss,x
	sta sinus,x
	sta sinus,y
	dey
	inx
	bpl 2$
	jmp contstart

	.org $1000+18*DYCPW

	;align 16,0
dycptablo:
	dc.b <(MATRIX+1*DYCPW),<(MATRIX+2*DYCPW),<(MATRIX+3*DYCPW),<(MATRIX+4*DYCPW)
	dc.b <(MATRIX+5*DYCPW),<(MATRIX+6*DYCPW),<(MATRIX+7*DYCPW),<(MATRIX+8*DYCPW)
	dc.b <(MATRIX+9*DYCPW),<(MATRIX+10*DYCPW),<(MATRIX+11*DYCPW),<(MATRIX+12*DYCPW)
	dc.b <(MATRIX+13*DYCPW),<(MATRIX+14*DYCPW),<(MATRIX+15*DYCPW),<(MATRIX+16*DYCPW)

	;align 16,0
dycptabhi:
	dc.b >(MATRIX+1*DYCPW),>(MATRIX+2*DYCPW),>(MATRIX+3*DYCPW),>(MATRIX+4*DYCPW)
	dc.b >(MATRIX+5*DYCPW),>(MATRIX+6*DYCPW),>(MATRIX+7*DYCPW),>(MATRIX+8*DYCPW)
	dc.b >(MATRIX+9*DYCPW),>(MATRIX+10*DYCPW),>(MATRIX+11*DYCPW),>(MATRIX+12*DYCPW)
	dc.b >(MATRIX+13*DYCPW),>(MATRIX+14*DYCPW),>(MATRIX+15*DYCPW),>(MATRIX+16*DYCPW)

	;align 32,0
dycpsrclo:
	dc.b 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
	dc.b 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
	;align 32,0
dycpsrchi:
	dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

patscroll
#if 0
	lda $14f8
	asl
	rol $14f0
	adc #0
	asl
	rol $14f0
	adc #0
	sta 1$+1
	lda $14f0
	sta 2$+1
#else
	lda $14f0
	lsr
	ror $14f8
	bcc no1$
	ora #$80
no1$
	lsr
	ror $14f8
	bcc no2$
	ora #$80
no2$
	sta 2$+1
	lda $14f8
	sta 1$+1
#endif

	ldy #8-1
2$	lda #0
	sta $14f0,y
1$	lda #0
	sta $14f8,y
	dey
	bpl 2$

	;jmp chtextcol
	;rts

	; color luminance order
	; 062485371

chtextcol:
0$	lda #0
	and #7
	tay
	lda cnt$
	asl
	;clc
	adc acc$
	sta acc$
	bcc 1$
	iny
1$
	lda $900e
	and #15
	ora COLO1$,y
	sta $900e
	lda COLO2$,y
	sta PATCOL+1	;$900f

	inc cnt$
	bpl 2$
	lda #0
	sta cnt$
	inc 0$+1
2$	rts
cnt$	dc.b 0
acc$	dc.b 0

COLO1$	dc.b $90,$c0,$a0,$30,$a0,$c0,$90,$d0, $90
COLO2$	dc.b $88,$48,$28,$68,$28,$48,$88,$58, $88


	.org $1500
	incbin dycp.data

	.org $1700

	align 256,0

#if SYSTEM & PAL
sinus = $200
sinuss
	dc.b $77,$76,$75,$73,$72,$70,$6f,$6d,$6c,$6a,$69,$67,$66,$65,$63,$62
	dc.b $60,$5f,$5d,$5c,$5a,$59,$58,$56,$55,$53,$52,$51,$4f,$4e,$4c,$4b
	dc.b $4a,$48,$47,$46,$44,$43,$42,$40,$3f,$3e,$3d,$3b,$3a,$39,$38,$36
	dc.b $35,$34,$33,$31,$30,$2f,$2e,$2d,$2c,$2b,$29,$28,$27,$26,$25,$24
	dc.b $23,$22,$21,$20,$1f,$1e,$1d,$1c,$1b,$1a,$19,$18,$18,$17,$16,$15
	dc.b $14,$13,$13,$12,$11,$10,$10,$0f,$0e,$0d,$0d,$0c,$0b,$0b,$0a,$0a
	dc.b $09,$09,$08,$07,$07,$06,$06,$06,$05,$05,$04,$04,$04,$04,$03,$03
	dc.b $03,$03,$03,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01
;	dc.b $01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$03,$03
;	dc.b $03,$03,$03,$04,$04,$04,$04,$05,$05,$06,$06,$06,$07,$07,$08,$09
;	dc.b $09,$0a,$0a,$0b,$0b,$0c,$0d,$0d,$0e,$0f,$10,$10,$11,$12,$13,$13
;	dc.b $14,$15,$16,$17,$18,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22
;	dc.b $23,$24,$25,$26,$27,$28,$29,$2b,$2c,$2d,$2e,$2f,$30,$31,$33,$34
;	dc.b $35,$36,$38,$39,$3a,$3b,$3d,$3e,$3f,$40,$42,$43,$44,$46,$47,$48
;	dc.b $4a,$4b,$4c,$4e,$4f,$51,$52,$53,$55,$56,$58,$59,$5a,$5c,$5d,$5f
;	dc.b $60,$62,$63,$65,$66,$67,$69,$6a,$6c,$6d,$6f,$70,$72,$73,$75,$76
#else
sinus = $200
sinuss
	dc.b $57,$56,$55,$54,$53,$52,$51,$50,$4f,$4e,$4d,$4c,$4b,$4a,$49,$48
	dc.b $47,$46,$45,$44,$42,$41,$40,$3f,$3e,$3d,$3c,$3b,$3a,$39,$38,$37
	dc.b $36,$35,$34,$33,$33,$32,$31,$30,$2f,$2e,$2d,$2c,$2b,$2a,$29,$28
	dc.b $27,$27,$26,$25,$24,$23,$22,$21,$21,$20,$1f,$1e,$1d,$1d,$1c,$1b
	dc.b $1a,$1a,$19,$18,$17,$17,$16,$15,$15,$14,$13,$13,$12,$11,$11,$10
	dc.b $10,$0f,$0e,$0e,$0d,$0d,$0c,$0c,$0b,$0b,$0a,$0a,$09,$09,$08,$08
	dc.b $08,$07,$07,$06,$06,$06,$05,$05,$05,$04,$04,$04,$04,$03,$03,$03
	dc.b $03,$02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01
;	dc.b $01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02
;	dc.b $03,$03,$03,$03,$04,$04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07
;	dc.b $08,$08,$08,$09,$09,$0a,$0a,$0b,$0b,$0c,$0c,$0d,$0d,$0e,$0e,$0f
;	dc.b $10,$10,$11,$11,$12,$13,$13,$14,$15,$15,$16,$17,$17,$18,$19,$1a
;	dc.b $1a,$1b,$1c,$1d,$1d,$1e,$1f,$20,$21,$21,$22,$23,$24,$25,$26,$27
;	dc.b $27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$33,$34,$35
;	dc.b $36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$44,$45,$46
;	dc.b $47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56

#endif
#if DYCPCOLORS
	align 32,0
dycpcolors = $9680
dycpcolorss:
	dc.b 0,6,0,6,6,3,6,3, 3,1,3,1,1,3,1,3, 3,6,3,6,6,0,6,0, 0,0,0,2,2,4,2,4
	dc.b 4,5,4,5,5,3,5,3, 3,1,3,1,1,7,1,7, 7,5,7,5,5,4,5,4, 4,2,4,2,2,0,0,0

	dc.b 0,6,0,6,6,3,6,3, 3,1,3,1,1,3,1,3, 3,6,3,6,6,0,6,0, 0,0,0,2,2,4,2,4

#endif


contstart:
	ldx #DYCPW-1
4$	txa
	and #1
	ora #$9e
	sta MATRIX+0*DYCPW,x
#if SYSTEM & PAL
	sta MATRIX+17*DYCPW,x
#else
	sta MATRIX+13*DYCPW,x
	lda #$a0		; DYCP font bytes 0..7 are clear
	sta MATRIX+14*DYCPW,x
#endif
	lda #8+1
	sta CMATRIX+0*DYCPW,x
#if SYSTEM & PAL
	sta CMATRIX+17*DYCPW,x
#else
	sta CMATRIX+13*DYCPW,x
	lda #0+0
	sta CMATRIX+14*DYCPW,x
#endif
	dex
	bpl 4$

	ldx #8-1
5$	lda #$4e		; 01 00 11 10 10 11 00 01
	sta $14f0,x
	lda #$b1
	sta $14f8,x
	dex
	bpl 5$


#if DYCPCOLORS
	ldx #8*12-1
1$	lda dycpcolorss,x
	sta dycpcolors,x
	dex
	bpl 1$
#endif
	jsr player_init
	jmp continit

	.org $1900
continit:
	ldx #0
0$	lda $1500,x
	lsr
	lsr
	sta $1700,x
	lda $1600,x
	lsr
	lsr
	sta $1800,x

	inx
	bne 0$

	jsr dycpinit
	jsr dycpplot

	sei
	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda #$00	; disable Timer A free run
	sta $912b

sync	ldx #RASTER+8/2-10	; wait for this raster line (times 2)
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

	lda $9124
	cli

#if 0
KEYWAIT	lda #0
	beq KEYWAIT
#if 1
nextpart:
	ldx #<nextFile
	ldy #>nextFile
#if 1
	lda LOADER
	cmp #$78
0$	bne 0$
#endif
	jmp LOADER


nextFile:
	dc.b "3K-PLASM"	; 8 significant chars
#endif
#else
	; Fix loader code to return instead of running the code
	lda #3
	sta $034b	; ldx #15 -> ldx #3 to only clear $9000..$9003
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389

	jmp KEYWAIT
#endif


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

#if SYSTEM & PAL
	ldx #10
#else
	ldx #12
#endif
w1$	dex
	bne w1$

dycppos	lda #SCRCENTER-DYCPW	; centered
	sta $9000


	lda #8
	sta $900f

#if SYSTEM & PAL
	jsr player_update
	jsr player_update
#else
	jsr player_update
#endif

	;inc $900f

	jsr dycpscroll

	;inc $900f

	; ~30 lines free here
	jsr patscroll

	;dec $900f

#if SYSTEM & PAL
	ldx #RASTER+17*8/2-1
#else
	ldx #RASTER+13*8/2-1
#endif
0$	cpx $9004
	bne 0$

	;dec $900f

	ldx #25
w2$	dex
	bne w2$

	lda #SCRCENTER-DYCPW	; centered
	sta $9000
#if SYSTEM & NTSC
	ldx #5
1$	dex
	bpl 1$
#endif
PATCOL	lda #0
	sta $900f

	jsr dycpplot

#if DYCPCOLORS
	lda dcoff+1
	clc
	adc #1
	and #63
	ora #<dycpcolors
	sta dcoff+1
#endif

	; Check keyboard
	lda $9121
	lsr
	bcs 95$
	; run/stop
	inc KEYWAIT+1
95$
#if 0
	inc $900f
	dec $900f
#endif

	jmp $eb18	; return from IRQ





DYCPCHARS	EQU	$100	; LSB must be 0, 2*DYCPW entries
DYCPOFFSET	EQU	(DYCPCHARS+2*DYCPW)
DYCPPTR		EQU	(DYCPOFFSET+1)
MATRIX		EQU	$1000
CMATRIX		EQU	$9400
DYCPZP		EQU	$02	;$9e

dycptable
	dc.b "              albert of pu\]^  presents       dycp unexpanded   "
	dc.b "      music by  anders carlsson        "
	dc.b "   use joy to control the dycp[       "
	dc.b " hellos to      andreas matthies     marko makela     adam bergstrom     jonas hulten     asger alstrup    "
	dc.b " aleksi eeben     britelite    viznut     mermaid    "
	dc.b " and  all other vic\o wackos out there [[         "
	dc.b " the glory of  vimm  for the unexpanded vic\o            "
	dc.b 0


count:
	dc.b 3	; go through the scrolltext 2 times


nextFile:
	dc.b "3K-PLASM"	; 8 significant chars



dycpinit:
	ldx #DYCPW-1
	lda #0
	sta DYCPOFFSET
0$	sta DYCPCHARS,x
	dex
	bpl 0$

	lda #SCRCENTER-DYCPW+1		; centered
	sta dycppos+1

#if DYCPCOLORS
#else
	ldx #4*DYCPW-1
1$
	lda #1
	sta CMATRIX+1*DYCPW,x
	sta CMATRIX+5*DYCPW,x
	sta CMATRIX+9*DYCPW,x
#if SYSTEM & NTSC
	;lda #0	; mask the screen-horizontally-at-0-fetch-another-line -bug
	;sta CMATRIX+13*DYCPW,x
	lda #8+1
	sta CMATRIX+13*DYCPW,x
	lda #0
	sta CMATRIX+14*DYCPW,x
#else
	sta CMATRIX+13*DYCPW,x
#endif
	dex
	bpl 1$
#endif


dycpinittext:
	dec count
	bne 0$
	lda AUTO
	beq 0$
	inc KEYWAIT+1
0$	lda #<dycptable
	sta dycptext+1
	lda #>dycptable
	sta dycptext+2
	rts

dycpscroll:
	lda #$7f
	sta $9122	; DDR for port B
	lda $9120	; VIA#2 port B
	bmi 0$
	dec DSPEED+1
0$	lda #$ff
	sta $9122	; Restore DDR

	lda $9111	; VIA#1 port A
	tay
	and #4
	bne 1$
	inc DDIFF+1
1$	tya
	and #8
	bne 2$
	dec DDIFF+1
2$	tya
	and #16
	bne 3$
	inc DSPEED+1
3$	tya
	and #$20	; fire ?
	bne 4$
	rts
	;
4$	lda #$16
	cmp dycpsel+1
	bne 6$
	lda #$14
	sta dycpsel+1
	rts

6$	sta dycpsel+1
	ldx dycppos+1
	dex
	stx dycppos+1
	cpx #SCRCENTER-DYCPW-1
	beq 5$
	rts
	;
5$	ldx #SCRCENTER-DYCPW+1
	stx dycppos+1
	;
	lda DYCPOFFSET
	clc
	adc DDIFF+1
	sta DYCPOFFSET
	;
dycptext:
	lda $aaaa
	inc dycptext+1
	bne 0$
	inc dycptext+2
0$	cmp #0
	bne 2$
	jsr dycpinittext

	lda #0
2$	and #31
	;
	ldy dycpptr0+1
	sta DYCPCHARS,y		; ring buffer with a tail..
	sta DYCPCHARS+DYCPW,y	; some call it delta buffer
	iny
	cpy #DYCPW
	bne 22$
	ldy #0
22$	sty dycpptr0+1
	sty dycpptr1+1
	rts

	; dycpplot+1:	$13
dycpplot:		; Plot position $1300
	lda #$13	; $13
	sta plttrg+2
	ldx #0
	stx plttrg+1
	ldx #2*DYCPW-1
	lda #$a0		; DYCP font bytes 0..7 are clear

1$	sta MATRIX+1*DYCPW,x
	sta MATRIX+3*DYCPW,x
	sta MATRIX+5*DYCPW,x
	sta MATRIX+7*DYCPW,x
	sta MATRIX+9*DYCPW,x
	sta MATRIX+11*DYCPW,x
#if SYSTEM & PAL
	sta MATRIX+13*DYCPW,x
	sta MATRIX+15*DYCPW,x
#endif
	dex
	bpl 1$	; 8x unrolled 2700 cycles = 38 lines	(16x 36 lines)

	ldx #0
	ldy DYCPOFFSET

dycploop
	sty dycpcnt+1

dycpptr0
	lda DYCPCHARS,x	; skip spaces.. one plot 224 cycles -> even if this
	beq pltnext	; check would take 7 cycles we will still gain cycles
			; if there is at least one space on the line

	lda sinus,y
	and #7
	sta dycpsub+1
	lda sinus,y
	lsr
	lsr
	lsr
	tay		; Y is the character line..
	txa
	clc
	adc dycptablo,y
	sta DYCPZP
	lda dycptabhi,y
	adc #0
	sta DYCPZP+1

	; Columns 0..31 -> char codes $40..$7f
	txa
	asl		; 0 -> C
	;ora #$40
	adc #$60
	ldy #0
	sta (DYCPZP),y
	ora #1
	ldy #DYCPW
	sta (DYCPZP),y

#if DYCPCOLORS
	lda DYCPZP+1	; $12/$13
	and #1
	ora #$94
	sta DYCPZP+1	; $94/$95
dcoff	lda dycpcolors,x
	;ldy #DYCPW
	sta (DYCPZP),y	; NOTE!! THE COLORS ARE NOT DOUBLE-BUFFERED !!
	ldy #0
	sta (DYCPZP),y	; 24 cycles/char = 10.1 lines
#endif


	; Plot source
dycpptr1
	ldy DYCPCHARS,x
	lda dycpsrclo,y
	sec
dycpsub	sbc #0		; 0..7
	; carry clear if borrow
	sta pltsrc+1	; low byte
	lda dycpsrchi,y
dycpsel	adc #$14	; $14+C = $15 (unless borrow)
	sta pltsrc+2	; hi byte

	ldy #15
pltsrc	lda $aaaa,y
plttrg	sta $aaaa,y
	dey
	bpl pltsrc	; 224 cycles = 3.2 lines / char

pltnext	lda plttrg+1	; next 16 bytes to plot
	clc
	adc #16
	sta plttrg+1
	bcs dycppage	; next page

dycpcnt	lda #0
DDIFF	adc #7
	tay

	inx
	cpx #DYCPW
	bne dycploop

	lda DYCPOFFSET
	clc
DSPEED	adc #5
	sta DYCPOFFSET
	rts

dycppage
	inc plttrg+2
	clc
	bcc dycpcnt

;	0	1	4	7
;0	a			
;1	a	a		
;2	a	a		
;3	a	a		
;4	a	a	a	
;5	a	a	a	
;6	a	a	a	
;7	a	a	a	a
;8	a	a	a	a
;9		a	a	a
;a			a	a
;b			a	a
;c			a	a
;d				a
;e				a
;f				a


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
	dec FADEOUT+1
0$	jmp $eb15	; return from IRQ, ack timer interrupt



	org $1d90
MUS
#if SYSTEM & PAL
SPD eqm 3					; song speed
#else
SPD eqm 2					; song speed
#endif
#include "../durplayer/dur-axelf.sng"
#include "../durplayer/player-dur.a65"

