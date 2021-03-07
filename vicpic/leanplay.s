; VICPIC player (durplayer 3, fitted with Photographic)
; (c) 2002 Anders Carlsson <anders.carlsson@mds.mdh.se>

;#processor 6502

;SYSTEM eqm PAL

;STANDALONE equ 1	; set to 0 if included as module
;MY_ORG	equ 5120	; routine needs to know where it begins (ZP or elsewhere)

; bit patterns in data
tend		equ $ff

;#seg main
;#include "../basic.i"
;#include "../timings.i"

;#org MY_ORG
MY_ORG
tracks:	; 20 bytes
	.byte %10010010, %00100000, %00100000, %00010000
	.byte %00010010, %00001011, %00001101, %00001001
	.byte %01010010, %00010100, %00100000, %11111111
	.byte %10010010, %11111111, %00001110, %00000000
	.byte %11111111, %00000000, %11111111, %00000000

	;; First byte of block:	duration*16 + note index
	;;
	;; Following bytes of block as follows:	
	;; 0000 - 0111 :	relative index 0-7
	;; 1000		:	unused (nibble shift marker)
	;; 1001 - 1101 :	duration index 1-5
	;; 1110		:	quiet
	;; 1111		:	block end

	;; tracks + blocks = 80 bytes (!)
	;; frequency table = 16 bytes (including channel byte)
	;; variable data   = 36 bytes (9x4)
	;; misc music data = 21 bytes (porta, jump-to, speed, block list)
		
b00:	; 2 bytes
	.byte $50,$fe

b01:	; 3 bytes
	.byte $1e,$10,$0f

b02:	; 3 bytes
	.byte $12,$70,$0f

b03:	; 13 bytes
	.byte $21,$9e,$56,$76,$3b,$a0,$e6,$5b,$97,$56,$e6,$4c,$0f

b04:	; 7 bytes
	.byte $42,$a2,$c5,$a0,$d3,$36,$0f

b05:	; 18 bytes
	.byte $26,$55,$95,$67,$a7,$95,$a4,$95,$6e
	.byte $4a,$49,$5a,$54,$e9,$01,$21,$01,$0f

b06:	; 14 bytes
	.byte $23,$22,$92,$56,$a2,$14,$19,$a4,$33,$01,$51,$94,$43,$f5

;	freqtable = d#2 f1 g1 a1 e2 h1 c2 d2 f2 g2 a2 h2 c3

channel					; freqtable[0] = channel
freqtable	.byte 0
	.byte 199,156,167,176,202,185,189,196,205,211,216,220,222,255,254

data		.byte 0,0,0,0	; > 16
sdur		.byte 0,0,0,0	; > 16
dur		.byte 0,0,0,0	; > 16
blockloc	.byte 0,0,0,0	; > 16
startloc	.byte 0,0,0,0	; > 16
sindex		.byte 0,0,0,0	; < 16
transpose	.byte 0,0,0,0	; < 16
repti		.byte 0,0,0,0	; < 16
trackloc	.byte 0,0,0,0	; < 16
porta		.byte 1,1,2,3	; < 16
jump_to		.byte 0,5,6,7	; < 16	(0,5,6,7)

#if SYSTEM & PAL	
speed		.byte 1*3-1,2*3-1,3*3-1,6*3-1,8*3-1
#else
speed		.byte 1*4-1,2*4-1,3*4-1,6*4-1,8*4-1
#endif
blist		.byte 20,22,25,28,41,48,66,81

player_init
	ldx	#3
	stx	channel
init2$:	txa
	sta	trackloc,x
	lda	#0
	sta	$900a,x		; (A) equals 0 == silence the voices
	sta	dur,x
	jsr	track_read
	dex
	bpl	init2$

	lda	$900e
	ora	#7		; medium volume to avoid distortion
	sta	$900e
	rts

; parameter in: X = track/channel (0-3)
; loop until we've set block pointers
track_read
	ldy	trackloc,x
tr2$:	lda	tracks,y
	cmp	#tend
	bcc	nolop$
	lda	jump_to,x
	sta	trackloc,x
	tay
	bcs	tr2$
nolop$:	
;	%XXYYYZZZ; X = transpose, Y = repeat, Z = block
	pha
	and	#$c0
	asl
	rol
	rol			; (C) is cleared by now
	sta	transpose,x
	pla
	pha
	and	#56
	lsr
	lsr
	sta	repti,x
	tya
	adc	#4
	sta	trackloc,x
	pla
	and 	#$07
	tay	
	lda	blist,y
	sta	startloc,x	;  for repeat
comm2:	pha			; store future (Y)
	tay
	lda	MY_ORG,y		;  initial read
	pha
	and	#$0f
	sta	sindex,x
	pla
	lsr
	lsr
	lsr
	lsr
	tay		
	lda	speed-1,y	; durations are stored as 1-5 in music data
	sta	sdur,x
	pla
	tay
comm:	iny
#if MY_ORG >= 256
	tya			; for non-zp use
	sta	blockloc,x	; for non-zp use
#else
	sty	blockloc,x
#endif
	lda	MY_ORG,y
	sta	data,x
	rts

; parameter in: X (channel 0-3)
; outputs: A (note value)
block_read
	lda	data,x
blr2:	pha
	lsr
	lsr
	lsr
	sec	; make sure to insert the %1000 pattern into data
	ror
	sta	data,x
	pla
	and	#$0f
	cmp	#14
	beq	quiet_note
	bcs	mblock_end
	cmp	#8
	beq	nread
	bcc	note$
	and	#7		; set new duration
	tay
	lda	speed-1,y	; durations are stored as 1-5 in music data
	sta	sdur,x
	bne	block_read	; (A) is non-zero
note$:	adc	transpose,x
	adc	sindex,x
	tay
	lda	freqtable,y
	bne	play_note
quiet_note
	lda	#0
play_note
	sta	$900a,x
	lda	sdur,x
	sta	dur,x
	rts
	
nread:	ldy	blockloc,x
	jsr	comm		; blockloc will never == 0
	bne	blr2		; no music data is 00
	
mblock_end
	dec	repti,x
	beq	br2$
	lda	startloc,x	; never zero
	jsr	comm2
	bne	blr2
	
br2$:	jsr	track_read
	bne	blr2
		
player_update
;#if STANDALONE
;	irqsync		
;	if SYSTEM & PAL
;		ldy #6
;		dey
;		bne *-1
;		bit $24
;	endif
;#endif
	ldx	channel
	lda	dur,x
 	bne porta_test

	jsr	block_read	; dur=0
	bne	irqut		; (A) will never be zero (speed read)
porta_test
	cmp	porta,x
	dec	dur,x
	bcs	irqut
	lda	#0
	sta	$900a,x
irqut
	dex
	bpl	ch_ok$
	ldx	#3
ch_ok$:	stx	channel
;#if STANDALONE
;	jmp	$eabf
;
;	jsr player_init			; entry point for standalone
;	setirq 1,0,player_update,TRUE
;#endif
;	rts
	jmp $eb18

	




