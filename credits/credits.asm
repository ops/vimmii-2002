

; Big DYCP: 8-line steps with video matrix, fine steps by copying
;


  processor 6502

; music $00..$90

xhoriz = $b0
columncnt_h = $b1
h_code = $b2
char_offset = $b3
effect_cnt = $b4
effect_arg = $b5

h_mod = $90	; COLUMNS+3	$90..$b0
hs_idx = $b6	; COLUMNS	$b6..$d3
		;		$d3..$f0

hor_map  = $100	; $100..$130	; must be aligned!
hs_phase = $130	; $130..$150
fivex    = $150	; 3*COLUMNS	$150..$1a7

hs_coslo = $9500
hs_coshi = $9580
map_h    = $9600	; $9600..$9700
eff0_la  = $9700	; $9700..$9780
hs_hi    = $9780	; $9780..$97ba	2*COLUMNS

; copyloop = $10e8	; $10e0..$11f0
; copyhoriz2 = $200	; $200..$2f0	(max size $114)
; effect1 = $316	; $316..$3f5	(max size $ea)


; $0000-0090	music data
; $0090-00d3	tables
; $0100-01a7	tables
; $01a7-01ff	stack space
; $0200-02f0	code
; $0314-0315	IRQ vectors
; $0316-03f5	code
; $0400-1000	--open space--
; $1000-10e8	Video matrix (init code)
; $10e8-1400	code
; $1400-1888	chars (COLUMNS*MIDROWS 8-byte chars) (init code)
; $1888-1890	empty char ($55)
; $1890-1e90	font (packed)
; $1e90-2000	code, music player


NTSC	= 1
PAL	= 2

#if 1;SYSTEM & PAL
CHCOL = 1
#else
CHCOL = 0
#endif

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
SCRCENTER	EQU	34
RASTER	= 90
				; 8-byte chars:
COLUMNS = 29	; 29 max!	; (4+1)*29	= 145 chars
MIDROWS = 5	; must be 4 or 5!
#else

LINES = 261
CYCLES_PER_LINE = 65
SCRCENTER	EQU	26
RASTER	= 80
				; 8-byte chars:
COLUMNS = 22;25;22	; 29 max!	; (4+1)*26	= 130
MIDROWS = 5;4;5	; must be 4 or 5!
;
; Either 22-column 5-row scroll or 25-column 4-row scroll
;
#endif

ROWS = MIDROWS+3
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2	; P $5685 N $4245


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm
	.org $1001
	;dc.b 2
	sei
	lda #$98
	sta $900f
	ldx #0
	stx $9002	; 0 columns
	stx $9003	; 0 rows
	stx columncnt_h
	lda $900e
	and #15
	ora #$80
	sta $900e
	lda #$cd	; video matrix $1000, chars $1400
	sta $9005

	lda #RASTER-ROWS*4
	sta $9001

	;ldx #0	; already 0
15$	lda mussrc,x
	sta musorg,x
	inx
	cpx #musend-mussrc
	bne 15$

	; Generate "sinus"..
	ldx #64-1
	ldy #0
2$	txa
	lsr
#if 0 ;SYSTEM & NTSC
	lsr
#endif
	and #7
	sta hs_coslo+64,x
	sta hs_coslo+0,y
	txa
	lsr
	lsr
	lsr
	lsr
#if 0 ;SYSTEM & NTSC
	lsr
#endif
	sta hs_coshi+64,x
	sta hs_coshi+0,y

	lda eff0_la_src,x
	sta eff0_la+$00,x	; lo
	lsr
	lsr
	lsr
	lsr
	sta eff0_la+$40,x	; hi

	iny
	dex
	bpl 2$

	;lda #0
	;sta columncnt_h
	;sta h_code
	lda #10
	sta char_offset


;inithoriz:
	lda #SCRCENTER-COLUMNS+1
	sta xhoriz

	ldx #COLUMNS-1
	stx h_code
8$	lda #0
	sta hs_phase,x
	txa
	sta h_mod,x
	dex
	bpl 8$
	jmp start


	.org $1000 + ROWS*COLUMNS	; NTSC COLUMNS=25: $107d

; Do NOT CHANGE code! Addresses are modified by code!
copyloop:
	ldx #0
loop$
	ldy hor_map,x			; 4	copyloop+2,3,4
	lda font_h,y			; 4	copyloop+5,6,7
	sta $1400+0*8*MIDROWS,x		; 5
	ldy hor_map,x
	lda font_h,y
	sta $1400+1*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+2*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+3*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+4*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+5*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+6*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+7*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+8*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+9*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+10*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+11*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+12*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+13*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+14*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+15*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+16*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+17*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+18*8*MIDROWS,x
	ldy hor_map,x
	lda font_h,y
	sta $1400+19*8*MIDROWS,x
#if COLUMNS > 20
	ldy hor_map,x
	lda font_h,y
	sta $1400+20*8*MIDROWS,x
#endif
#if COLUMNS > 21
	ldy hor_map,x
	lda font_h,y
	sta $1400+21*8*MIDROWS,x
#endif
#if COLUMNS > 22
	ldy hor_map,x
	lda font_h,y
	sta $1400+22*8*MIDROWS,x
#endif
#if COLUMNS > 23
	ldy hor_map,x
	lda font_h,y
	sta $1400+23*8*MIDROWS,x
#endif
#if COLUMNS > 24
	ldy hor_map,x
	lda font_h,y
	sta $1400+24*8*MIDROWS,x
#endif
#if COLUMNS > 25
	ldy hor_map,x
	lda font_h,y
	sta $1400+25*8*MIDROWS,x
#endif
#if COLUMNS > 26
	ldy hor_map,x
	lda font_h,y
	sta $1400+26*8*MIDROWS,x
#endif
#if COLUMNS > 27
	ldy hor_map,x
	lda font_h,y
	sta $1400+27*8*MIDROWS,x
#endif
#if COLUMNS > 28
	ldy hor_map,x
	lda font_h,y
	sta $1400+28*8*MIDROWS,x
#endif
	inx		;2
	cpx #MIDROWS*8;40		;2
	beq 1$		;2
	jmp loop$	;3 = 29*13+9 -> 40*386 = 217 lines
1$			; 32*(25*13+9)=164 lines
	rts


scrollhoriz:
	lda $9111	; VIA#1 port A
	and #$20	; fire?
	beq lrts$
	; faster scroll (1 cycle / frame) -- too fast
	dec xhoriz
	lda xhoriz
	cmp #SCRCENTER-COLUMNS-1	; 4? -> 6
	beq copy$
	cmp #SCRCENTER-COLUMNS-0	; 5?
	bne lrts$

	lda #0
	sta firstcolflag$+1
	dec columncnt_h
	bmi ptr$
lrts$	rts

	; counter negative -- get next character
eff$	jsr seteffect
ptr$	lda text_h
	inc ptr$+1
	bne 4$
	inc ptr$+2
4$	tay
	bpl 5$
	cmp #255
	bne eff$
reset$	ldy #<text_h
	sty ptr$+1
	ldy #>text_h
	sty ptr$+2

	dec FADEOUT+1

	lda #" "
5$	ldy #1
	cmp #"I"
	beq 6$
	;cmp #" "
	;beq 6$
	cmp #"."
	bne nodot$
	lda #38		; last piece of L
	dey
	beq 7$

nodot$	iny	;ldy #2
6$	and #31
	sta add$+1
	asl
	clc
add$	adc #0
7$	sta ccode$+1
	sty columncnt_h

	lda char_offset
	sta firstcolflag$+1
	rts


copy$	ldy h_code
	ldx h_mod+1,y
	stx h_code

	lda hs_phase,y
	sec
firstcolflag$
	adc #0
	sta hs_phase,x

	ldy hs_idx,x	;4
ccode$	ldx #0
	lda map_h+$00,x	; lo
	lsr
	lsr
	lsr
	lsr		; x to C
	lda map_h+$80,x
	and #15		; XXXX to A
	rol		; XXXXx to A
	adc #>font_h
	pha
	lda map_h+$00,x	; 0000xxxx
	asl		; 000xxxx0
	asl		; 00xxxx00
	asl		; 0xxxx000
	asl		; xxxx0000
	asl		; xxx00000
	clc
	adc #<font_h
	sta copyloop+6,y	;5
	pla
	bcc no$
	adc #0
no$	sta copyloop+7,y
	inc ccode$+1

	lda #SCRCENTER-COLUMNS+1	; 6
	sta xhoriz
	rts



contstart:
	jsr player_init
	lda $9124	; ack interrupt..
	cli
loop$
#if 0	; debug color
	inc $900f
	dec $900f
#endif
	jmp loop$



copyhoriz:
	ldx #COLUMNS-1
again:	lda hs_phase,x	;4
	clc
hs_speed:
	adc #2		;2
	sta hs_phase,x	;5
	and #127
	tay		;2
	lda hs_coshi,y	;4
	;and #15
	sta hs_hi,x	;5
	sta hs_hi+COLUMNS,x ; 5	; delta buffer
	lda hs_coslo,y	;4
	and #15
#if <hor_map
	clc
	adc #<hor_map
#endif
	ldy hs_idx,x	;4
	sta copyloop+3,y	;5

	dex		;2
	bpl again	;3 = 48 -> 29*48 = 19.6 lines
	jmp copyhoriz2


E_ROTATE   = 128
E_MODULATE = 129
E_PIPE     = 130


; Do not use characters WXZ, [ is 2
text_h
	dc.b "  HERE ENDS ", E_ROTATE, "VICI ITERUM MMII     "
	dc.b "  I HOPE YOU ENJOYED THIS VIC[O DEMO      "
	dc.b "  USE JOYSTICK TO CONTROL THE SCROLL     "
#if 0
	dc.b "HELLOS TO   ALEKSI EEBEN", E_MODULATE
	dc.b "   BRITELITE  VI[NUT   MERMAID   "
	dc.b "MARKO MAKELA   ANDERS CARLSSON   AND ALL", E_PIPE, " OTHER VIC[O FREAKS... "
#else
	dc.b " IT TOOK QUITE AN EFFORT TO FIT ALL THESE EFFECTS", E_MODULATE, " INTO THIS PART...     "
	dc.b " BUT I LIKE A CHALLENGE AS MUCH AS ANYONE.", E_PIPE, "      "
#endif
	dc.b "REMEMBER.   FIVE KB OF TOTAL MEMORY AND STILL SOME FREE"
	dc.b "          ", 255


	.org $1400	; size $490

mussrc	;$1004
#rorg $00
musorg
#include "newsong.a65"
#rend
musend	;$1094, size $90

start
	;ldx #255	; already 255
	inx
	stx h_mod+COLUMNS+0
	inx
	stx h_mod+COLUMNS+1
	inx
	stx h_mod+COLUMNS+2

	ldx #0
	txa	;lda #0
9$	sta fivex+0*COLUMNS,x
	sta fivex+1*COLUMNS,x
	sta fivex+2*COLUMNS,x
	clc
	adc #MIDROWS
	inx
	cpx #COLUMNS
	bne 9$

	ldx #map_end-map_hs
10$	lda map_hs,x
	sta map_h,x	; lo part
	lsr
	lsr
	lsr
	lsr
	sta map_h+$80,x	; hi part
	dex
	bpl 10$

	ldx #(MIDROWS+1)*8-1
11$	lda hor_maps,x
	sta hor_map,x
	dex
	bpl 11$

	ldx #0
12$	lda horizsrc,x
	sta horizorg,x
	inx
#if horizend-horizsrc > $100
	bne 12$
13$	lda horizsrc+$100,x
	sta horizorg+$100,x
	inx
	cpx #horizend-horizsrc-$100
	bne 13$
#else
#if horizend-horizsrc == $100
	bne 12$
#else
	cpx #horizend-horizsrc
	bne 12$
#endif
#endif

	ldx #0
14$	lda effsrc,x
	sta efftrg,x
	inx
	cpx #effend-effsrc
	bne 14$

	lda #0
	tax
16$	sta hs_idx,x
	inx
	clc
	adc #9
	bcc 16$

	; 0,9,18,27,36,45,54,63,72,81,90,99,108,117,126,135,144,153,162,171,180,189,198,207
	; 216,225,234,243,252

	ldx #COLUMNS*ROWS
1$	lda #29*5	; Max COLUMNS*MIDROWS
	sta $1000-1,x
	lda #9
	sta $9400-1,x
	dex
	bne 1$

	;sei
	lda #$7f
	sta $913e	; disable and acknowledge interrupts / NMIs
	sta $912d
	sta $911e	; disable NMIs (Restore key)


;synchronize with the screen
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
	jmp contstart


horizsrc	;$153b
#rorg $200
horizorg

copyhoriz2:
	ldy h_code
	lda h_mod+1,y
	clc
	adc #<fivex
	sta f$+1
	lda #>fivex
	adc #0
	sta f$+2

	lda h_mod+1,y
	clc
	adc #<hs_hi
	sta h$+1
	lda #>hs_hi
	adc #0
	sta h$+2

	lda #29*5	; Max COLUMNS*MIDROWS
	ldx #COLUMNS-1
clr$
#if ROWS == 7
	sta $1000+0*COLUMNS,x
	sta $1000+1*COLUMNS,x
#if MIDROWS < 5
	sta $1000+2*COLUMNS,x
	sta $1000+4*COLUMNS,x
#endif
	sta $1000+5*COLUMNS,x
	sta $1000+6*COLUMNS,x
#endif

#if ROWS == 8
	sta $1000+0*COLUMNS,x
	sta $1000+1*COLUMNS,x
	sta $1000+2*COLUMNS,x
#if MIDROWS < 5
	sta $1000+3*COLUMNS,x
	sta $1000+4*COLUMNS,x
#endif
	sta $1000+5*COLUMNS,x
	sta $1000+6*COLUMNS,x
	sta $1000+7*COLUMNS,x
#endif
	dex
	bpl clr$

	ldx #0
9$
h$	lda hs_hi,x	;4
	and #3
	tay
	txa
	clc
	adc htab,y	; *COLUMNS
	tay

f$	lda fivex,x
	sta $1000 + 0*COLUMNS,y
	clc
	adc #1
	sta $1000 + 1*COLUMNS,y
	adc #1
	sta $1000 + 2*COLUMNS,y
	adc #1
	sta $1000 + 3*COLUMNS,y
#if MIDROWS > 4
	adc #1
	sta $1000 + 4*COLUMNS,y
#endif
	inx		;2
	cpx #COLUMNS	;2
	bne 9$		;2
	jmp copyloop

htab:
	dc.b 3*COLUMNS,2*COLUMNS,1*COLUMNS,0*COLUMNS


irq	lda #<(TIMER_VALUE-46+2)	; 2 for reload time
	sec
	sbc $9124	; 46 to 53 cycles delay at this stage
			; 90..83/23..16 in $9124 for PAL/NTSC
	; A = 0..7	0=wait 7 cycles .. 7=wait 0 cycles
#if SYSTEM & PAL
	sta *+4
	bne *+2
	nop
	lda #$a9
	lda #$a9
	lda #$a9
	bit $ea
#endif

#if CHCOL
	jsr chtextcol	; wait until border to hide the change bug...
#endif
#if 0
	inc $900f
#endif
	jsr scrollhoriz

#if 0
	inc $900f
	dec $900f
#endif
	lda xhoriz
	ldx #COLUMNS
	sta $9000
	stx $9002

	lda #2*ROWS
	sta $9003

	jsr copyhoriz		; must be finished before the highest bottom
				; line of scroll shown

#if 0	; debug color..
	inc $900f
	dec $900f
#endif
	jsr effects		; can be executed while scroll is shown
#if 0	; debug color..
	inc $900f
	dec $900f
#endif


FADEOUT	lda #128+1	; fade out music, continue the scroll..
	bmi 0$

	lsr
	lsr
	lsr
	lsr
	sta ora$+1
	lda $900e
	and #$f0
ora$	ora #$00
	sta $900e
1$	dec FADEOUT+1
0$

#if 0	; debug color..
	dec $900f
#endif
	jmp player_update
	;jmp $eb18	;$eabf


;	effect 0	rotate
;	effect 1	modulate
;	effect 2	pipe

;effectNone:
;	lda #$ff
;	sta effect_cnt
;	rts

effect0:		; rotate	$10d
	lda effect_cnt
	and #63
	tax
	lda #0
	ldy #32-1
0$	sta hor_map+8,y
	dey
	bpl 0$

	lda eff0_la+$00,x
	and #15
	sta la$+1
	lda eff0_la+$40,x
	asl
	asl
	asl
	asl
	ora la$+1
	sta la$+1
	ldy eff0_ys,x
#if 0
	lda eff0_ya,x
	sta ya$
#else
	lda #$88
#if MIDROWS > 4
	cpy #16
#else
	cpy #12
#endif
	bcs 11$
	lda #$c8
11$	sta ya$
#endif

#if MIDROWS > 4
	lda #8*31
#else
	lda #8*23
#endif
next$	tax			;2
	lsr			;2
	lsr			;2
	lsr			;2	/8-table would save ~1 line
	sta hor_map+8,y		;5
ya$	dey			;2
	txa			;2
	;sec
la$	sbc #8*32/32		;2
	bcs next$		;3= 29 -> max 32*29 = 928 cycles = 13 lines

	lda effect_cnt
	;clc
	adc effect_arg		; allowed speeds 1,2,3
	cmp #2*64	; do it twice
	bcc updateCnt
effectNone:
	lda #255
updateCnt:
	sta effect_cnt
	rts

#rend
horizend	;$1643, size $108


map_hs:
#if MIDROWS > 4
#incbin "map_h.bin"	;size $54
#else
#incbin "map_n.bin"	;size $54
#endif
map_end

	;align 256,0

hor_maps:
#if MIDROWS > 4
	dc.b 0,0,0,0,0,0,0,0
	dc.b 0,1,2,3,4,5,6,7
	dc.b 8,9,10,11,12,13,14,15
	dc.b 16,17,18,19,20,21,22,23
	dc.b 24,25,26,27,28,29,30,31
	dc.b 0,0,0,0,0,0,0,0
#else
	dc.b 0,0,0,0,0,0,0,0		;size $28
	dc.b 0,1,2,3,4,5,6,7
	dc.b 8,9,10,11,12,13,14,15
	dc.b 16,17,18,19,20,21,22,23
	dc.b 0,0,0,0,0,0,0,0
#endif

effsrc	;$16c7
#rorg $316
efftrg

effect1:		; modulate	$3b+eff3_tab($42)
	lda effect_cnt
#if MIDROWS > 4
	and #31
#else
	sec		; modulo 24
sub$	sbc #24
	bcs sub$
	adc #24
#endif
	tax
#if MIDROWS > 4
	ldy #31
#else
	ldy #23
#endif
	tya
	bne ok$	; jump always

0$	and #31
	clc
	sbc eff1_tab,x
	bpl ok$
	lda #0
ok$	sta hor_map+8,y
	inx
	dey
	bpl 0$

	bmi eff_leave	; jump always


eff1_tab:
#if MIDROWS > 4
	dc.b  0,-1,-1, 0,-1, 0,-1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1,-1, 0
	dc.b  0,-1,-1, 0,-1, 0,-1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0,-1, 0, 0,-1,-1,-1, 0
#else
	; 24 = 2*2*3  32 = 2*2*2*2
	dc.b -1, 0,-1,-1, 0,-1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0,-1, 0,-1
	dc.b -1, 0,-1,-1, 0,-1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0,-1, 0,-1
#endif


effect2:		; pipe		$36+eff3_tab($42)
	lda effect_cnt
#if MIDROWS > 4
	ldy #31
	ldx #16
#else
	ldy #23
	ldx #12
#endif
	bne ok$	; jump always

0$	clc
	sbc eff1_tab,x
ok$	and #31
	sta hor_map+8,y
	inx
	dey
	bpl 0$

eff_leave:
	inc effect_cnt
	bpl 1$

	dec effect_arg
	bmi 1$
	iny	;ldy #0
	sty effect_cnt
	sty effect_type+1	; ->0 rotate
	inc effect_arg		; ->1 once

1$	rts

;	effect 0	rotate
;	effect 1	modulate
;	effect 2	pipe

efptrhi:
	dc.b >(effect0-1),>(effect1-1),>(effect2-1),>(effectNone-1)
efptrlo:
	dc.b <(effect0-1),<(effect1-1),<(effect2-1),<(effectNone-1)

seteffect:
	and #3
	tay
	lda effect_cnt
	bpl lrts$

	ldx #0
	stx effect_cnt
	inx	;ldx #1
	stx effect_arg
	sty effect_type+1
lrts$	rts

#if MIDROWS > 4
eff0_ys:
	dc.b 31,31,31,31,31,31,30,29,28,26,25,24,22,20,19,18
	dc.b 15,14,13,12,10, 8, 7, 6, 4, 3, 2, 1, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 1, 1, 2, 3, 4, 6, 7, 8,10,12,13,14
	dc.b 17,18,19,20,22,24,25,26,28,29,30,31,31,31,31,31
#else
eff0_ys:
	dc.b 23,23,23,23,23,22,21,20,19,18,17,16,15,14,13,12
	dc.b 11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1
	dc.b  1, 1, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11
	dc.b 12,13,14,15,16,17,18,19,20,21,22,23,23,23,23,23
#endif
;eff0_ya:
;	dc.b $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
;	dc.b $c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8
;	dc.b $c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8,$c8
;	dc.b $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88


#rend
effend	;$17a6, size $df


eff0_la_src:	; size $40
	dc.b 8*32/32,8*32/32,8*32/31,8*32/31
	dc.b 8*32/30,8*32/28,8*32/27,8*32/25
	dc.b 8*32/23,8*32/20,8*32/18,8*32/15
	dc.b 8*32/12,8*32/9,8*32/6,8*32/3

	dc.b 255,8*32/3,8*32/6,8*32/9
	dc.b 8*32/12,8*32/15,8*32/18,8*32/20
	dc.b 8*32/23,8*32/25,8*32/27,8*32/28
	dc.b 8*32/30,8*32/31,8*32/31,8*32/32

	dc.b 8*32/32,8*32/32,8*32/31,8*32/31
	dc.b 8*32/30,8*32/28,8*32/27,8*32/25
	dc.b 8*32/23,8*32/20,8*32/18,8*32/15
	dc.b 8*32/12,8*32/9,8*32/6,8*32/3

	dc.b 255,8*32/3,8*32/6,8*32/9
	dc.b 8*32/12,8*32/15,8*32/18,8*32/20
	dc.b 8*32/23,8*32/25,8*32/27,8*32/28
	dc.b 8*32/30,8*32/31,8*32/31,8*32/32


	.org $1400+29*5*8	; Max COLUMNS*MIDROWS*8
	dc.b $55,$55,$55,$55,$55,$55,$55,$55


	; 00 - background, 01 - border, 11 - aux, 10 - character




	;.align 256,0
#if MIDROWS > 4
font_h:
#incbin "font_h.bin"	; size $600

#else
font_h:
#incbin "font_n.bin"	; size $600
#endif



#if CHCOL
	; color luminance order
	; 062485371

chtextcol:
	nop
	nop
	nop
	nop
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
#if 1
	lda $900e
	and #15
	ora COLO1$,y
#else
	lda #7		; volume
	ora COLO1$,y
#endif
	sta $900e
	lda COLO2$,y
	sta $900f

	inc cnt$
	bpl 2$
	lda #0
	sta cnt$
	inc 0$+1
2$	rts
cnt$	dc.b 0
acc$	dc.b 0

COLO1$	dc.b $80,$40,$20,$60,$20,$40,$80,$50, $80
COLO2$	dc.b $98,$c8,$a8,$38,$a8,$c8,$98,$d8, $98
#endif


effects:
	lda #$7f
	sta $9122	; DDR for port B
	lda $9120	; VIA#2 port B
	bmi 0$		; right
	dec char_offset

0$	lda #$ff
	sta $9122	; Restore DDR

	lda $9111	; VIA#1 port A
	lsr
	lsr
	lsr
	bcs 1$		; up
	inc hs_speed+1

1$	lsr
	bcs 2$		; down
	dec hs_speed+1

2$	lsr
	bcs 3$		;left
	inc char_offset

3$
	; Check keyboard	run/stop lshift x v n , / up/down
	lda $9121
	bmi 92$

	lda #E_ROTATE
	bne set$

92$	asl
	bmi 95$
	; x
	lda #E_MODULATE
	bne set$

95$	asl
	bmi 97$
	; lshift
	lda #E_PIPE
set$	jsr seteffect
97$

nocheck$
	lda effect_cnt
	bmi lrts1
effect_type:
	ldy #3
	lda efptrhi,y
	pha
	lda efptrlo,y
	pha
lrts1	rts		; jump to one effect or another


	; $1f27
	#include "player.a65"
	; $1ff7, size $d0
