
SYSTEMSEL = $31a
AUTO      = $31b	; non-zero for automatic control, zero for manual
LOADER    = $31c	; ..$3e3



	; Code by Albert of Pu-239	http://www.cs.tut.fi/~albert/
	; Except part of the loader code originally by Marko Mäkelä

	; Bitmap 4k
	; Color memories 512 nybbles
	; Video memories 512 bytes
	; Back/Border/Aux colors 3*160 nybbles
	; Total = 5104 bytes
	; Total memory in an unexpanded VIC inluding color memory: 5632 bytes


	processor 6502
	seg code


ZP = 0	; one zero-page variable used
;   0000-0172	space for music	; ZP = $00 used for display code!
;   0180-01e1	main loop
;   01e2-01ff	stack space (30 bytes)
;   0200-02f9	Video matrix

;   02fa-0313	file names
;   0314-0319	IRQ vectors -- Reserved
;   031a-03e3	[loader, do not overwrite]
;   03e8-03fe	code

;   0400-0fff	--open area--
;   1000-1fa0	graphics
;   1fa0-2000	display code

;   9401-94a0	Background colors	(nybbles)
;   9501-95a0	Border colors		(nybbles)
;   9600-96ff	Color memory		(nybbles)
;   9701-97a0	Auxiliary colors	(nybbles)


NTSC	= 1
PAL	= 2

;SYSTEM	= NTSC	; 6560-101: 65 cycles per raster line, 261 lines
;SYSTEM	= PAL	; 6561-101: 71 cycles per raster line, 312 lines

#if SYSTEM & PAL
LINES = 312
CYCLES_PER_LINE = 71
#endif
#if SYSTEM & NTSC
LINES = 261
CYCLES_PER_LINE = 65
#endif
TIMER_VALUE = LINES * CYCLES_PER_LINE - 2


COLUMNS = 25	; Note: only 25 supported
ROWS	= 10	; COLUMNS*ROWS = 250 < 256

VAL1	= COLUMNS
VAL2	= 128+COLUMNS


; ===========================================================================


#mac samepage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if >. - >({1})
    echo "fatal: page boundary crossed at",{1},"- compilation aborted"
    err
  endif
#endm

#mac nextpage		; !!!CAN'T USE LOCAL SYMBOLS with this macro!!!
  if (>. - >({1})) - 1
    echo "fatal: page boundary not crossed at",{1},"- compilation aborted"
    err
  endif
#endm


#if SYSTEM & PAL
RASTER	= 32	; effect=screen at RASTER + 10 (+ 1)
#else
RASTER	= 14	;18
#endif




	.org $1001	; for the *un*expanded Vic-20
basic	.word 0$	; link to next line
	.word 1998	; line number
	.byte $9E	; SYS token

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
0$:	.byte 0,0,0	; end of BASIC program

start	sei
	; initialize the stack pointer
	ldx #$ff
	txs

	ldy #0
	;sty $9002	; columns
	sty $9003	; rows
	sty $900e	; aux color & volume
	;sty $900f	; back & front colors

	lda #$7f
	sta $912e	; disable and acknowledge interrupts
	sta $912d
	sta $911e	; disable NMIs (Restore key)

	ldx #block_end - block_src
7$	lda block_src-1,x
	dex
	sta block_org,x
	bne 7$

	ldx #code_end - code_src
8$	lda code_src-1,x
	dex
	sta code_org,x
	bne 8$

	; Copy the colors for the first pic, load directly for other pics
	ldx #0
2$	lda colors,x		; color mem 1
	sta $9600,x
	dex
	bne 2$

	ldx #160
5$	lda colors+$100,x	; aux colors
	lsr
	lsr
	lsr
	lsr
	sta $9700,x
	lda colors+$200,x	; front/back colors
	sta $9500,x		; lo nybble
	lsr
	lsr
	lsr
	lsr
	sta $9400,x		; hi nybble
	dex
	bne 5$

	ldx #0
16$	lda playersrc,x
	;sta playerorg,x
	dc.b $9d
	dc.b <playerorg
	dc.b >playerorg
	inx
#if playerend-playersrc > 256
	bne 16$
161$	lda playersrc+256,x
	sta playerorg+256,x
	inx
	cpx #playerend-playersrc-256
	bne 161$
#else
	cpx #playerend-playersrc
	bne 16$
#endif

	jsr player_init

#if 0
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #$00	; disable Timer A free run
	sta $912b
	ldx #RASTER+1	; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$
#endif

sync	ldx #RASTER	; wait for this raster line (times 2)
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
	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

	lda #<TIMER_VALUE
	ldx #>TIMER_VALUE
	sta $9126

	jsr wait15	; wait routine already copied to stack!
	jsr wait14
#if SYSTEM & PAL
	jsr wait15
	jsr wait15
	jsr wait12
	bit $ea
#else
	jsr wait12
	ldy #14
2$	dey
	bne 2$
#endif

	stx $9125	; start the IRQ timer A
			; 6560-101: xx cycles from $9004 change
			; 6561-101: yy cycles from $9004 change

	ldx #0
4$	txa
	sta $200,x
	inx
	bne 4$

	ldx #b2_end - b2_src
17$	lda b2_src-1,x
	dex
	sta b2_org,x
	bne 17$



	lda #ROWS*2+1	; 8 rows & 16x8 chars
	sta $9003
	lda #$8c	; video matrix at $0000, character mem at $1000
	sta $9005
	lda #$c0
	sta $912e	; enable Timer A underflow interrupts

	lda #<irq2	; set the raster IRQ routine pointer
	sta $314
	lda #>irq2
	sta $315
	cli

	; Fix loader code to return instead of running the code
	lda #2
	sta $034b	; ldx #15 -> ldx #2 to only clear $9000..$9002
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389
	jmp continue

wait15	nop
wait14	nop
wait13	nop
wait12	nop
	nop
wait10	nop
wait9	nop
	nop
	nop
	rts	; 6+6


	;.org $117e

block_src
#rorg $180
block_org
continue		; create the video matrices
	ldx #<gfxFile
	ldy #>gfxFile
	jsr LOADER+19
	bcs nextpart	; failed to load!

	sei
	lda #<irq	; set the raster IRQ routine pointer
	sta $314
	lda #>irq
	sta $315

#if SYSTEM & PAL
	lda #34-COLUMNS	; centered
#else
	lda #26-COLUMNS	;27-COLUMNS
#endif
	sta $9000	; horizontal centering
	lda #RASTER+11
	sta $9001	; vertical centering
	lda #VAL2
	sta $9002	; columns + 9th bit of video matrix/color memory

	ldx #3
	stx count+1
	stx count+0
	stx KEYWAIT+1

	lda $9124	; ack timer intr

	cli
KEYWAIT	cpx #0
	beq KEYWAIT

	sei
	inc colFile+7
	inc gfxFile+7

	lda #<irq2	; set the raster IRQ routine pointer
	sta $314
	lda #>irq2
	sta $315

	cli
	ldx #<colFile
	ldy #>colFile
	jsr LOADER+19
	bcc continue

	; failed to load! try to load the next part
nextpart:
	; Fix loader code
	lda #15
	sta $034b	; ldx #15
	lda #$b0	; bcs
	sta $0389	; $0389	bcs $0389
	ldx #<nextFile
	ldy #>nextFile
	jmp LOADER

#rend
block_end

b2_src
#rorg $2fa
b2_org
colFile:
	dc.b "3K-VCOLA"	; 8 significant chars
gfxFile:
	dc.b "3K-VGFXA"	; 8 significant chars
nextFile:
	dc.b "3K-DYCPI"	; 8 significant chars
#rend
b2_end


code_src
#rorg $3e8
code_org

irqexit
	dec count+0
	bne 0$
	dec count+1
	bne 0$
	lda AUTO
	beq 0$
	inc KEYWAIT+1
0$
	jmp player_update
	;jmp $eb18	; return from IRQ

count	dc.b 0,2
#rend
code_end



playersrc
#rorg $001
playerorg
#include "leanplay.s"
#rend
playerend


	.org $1300
colors	incbin "colors"

	.org $1600

	incbin "graphend"


	.org $1000+4000


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
	; A = 0..8	0=wait 10 cycles .. 8=wait 2 cycles
	sta *+4
irqjmp	bne *+2		; +1 cycles for all but A = 0
	nop		; 0:11-1 = 10
	lda #$a9	; 1:9 2:8
	lda #$a9	; 3:7 4:6
	lda #$a9	; 5:5 6:4
	bit $ea		; 7:3 8:2
	; now we are synchronized 18 cycles from the IRQ
	samepage irqjmp

	ldx #ROWS*16
effect			; 71 cycles / loop (PAL)
	stx ZP		;3

	lda $9400,x	; MSn
	asl
	asl
	asl
	asl
	sta ora$+1
	lda $9500,x	; LSn
	and #15
ora$	ora #0
	tay
	nop
	nop		;2
	lda $9700,x	;4
	asl		;2
#if SYSTEM & PAL		; 65 cycles / loop (NTSC)
	ldx #0		;2
	stx $900f	;4	; screen right edge
#endif
	ldx ZP		;3	; linecount
	beq leave	;2
	asl		;2
	asl		;2
	asl		;2
volume	ora #7
	sta $900e	;4
	sty $900f	;4	; screen left edge
	dex		;2
	jmp effect	;3/2
	samepage effect

leave
#if SYSTEM & NTSC
	ldx #0		;2
	stx $900f	;4
#endif
	lda $9121	; Check keyboard
	lsr
	bcs 1$
	; run/stop
	inc KEYWAIT+1
1$
irq2:
	lda $9124
	jmp irqexit


	.end

