
SYSTEMSEL = $31a
AUTO      = $31b
LOADER    = $31c	; ..$3e3


  processor 6502

NTSC	= 1
PAL	= 2


; =========================================================================
; Initializations and the disk drive's part of the fastloader.
; =========================================================================

; KERNAL definitions

secnd	= $ff93	; send secondary address for LISTEN
ciout	= $ffa8	; write serial data
unlsn	= $ffae	; send UNLISTEN command
listn	= $ffb1	; send LISTEN command
fa	= $ba	; Current Device Number

AMOUNT = $20	; amount of data bytes to transfer with one M-W command
ESCBYTE = $ef	; the escape char used in the transfers
RETRIES = 20	; amount of retries in reading a block


DEFAULT_DEVICE = 8	; Default device number

; the initialization code

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
	lda #$f8
	sta $900f

matrix = $1a00

	ldx #0
cc$	lda $1e00,x
	sta matrix+$000,x
	lda $1f00,x
	sta matrix+$100,x
	inx
	bne cc$

	; relocate video matrix, do the switch before overwritten by music
	ldx #$e0	; matrix at $1800 + $200
	stx $9005

	ldx #0
0$	lda mussrc,x
	sta MUS,x
	inx
#if musend-mussrc > 256
	bne 0$
1$	lda mussrc+256,x
	sta MUS+256,x
	inx
	cpx #musend-mussrc-256
	bne 1$
#else
	cpx #musend-mussrc
	bne 0$
#endif
	jsr player_init

	sei

PAL_LINES = 312
PAL_CYCLES_PER_LINE = 71
PAL_RASTER = 10

NTSC_LINES = 261
NTSC_CYCLES_PER_LINE = 65
NTSC_SCRCENTER = 26

PAL_TIMER_VALUE = PAL_LINES * PAL_CYCLES_PER_LINE - 2
NTSC_TIMER_VALUE = NTSC_LINES * NTSC_CYCLES_PER_LINE - 2

;synchronize with the screen
	; If the timer is running, wait for it to finish..
	lda $912b
	and #$40
	beq sync
	lda #0		; disable Timer A free run
	sta $912b


sync	ldx #PAL_RASTER-10	; wait for this raster line (times 2)
0$	cpx $9004
	bne 0$		; at this stage, the inaccuracy is 7 clock cycles
			; the processor is in this place 2 to 9 cycles
			; after $9004 has changed
	ldy #9
	bit $24
1$	ldx $9004
	txa

	bit $24
	ldx #24
	dex
	bne *-1		; first spend some time (so that the whole
	cmp $9004	; loop will be 2 raster lines)
	bcs *+2		; save one cycle if $9004 changed too late
	dey
	bne 1$


	lda #$40	; enable Timer A free run on irq-VIA
	sta $912b

#if SYSTEM & NTSC
	lda #<NTSC_TIMER_VALUE
	ldx #>NTSC_TIMER_VALUE
#else
	lda #<PAL_TIMER_VALUE
	ldx #>PAL_TIMER_VALUE
#endif
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

	cli

	jsr initloader

	; Do some fade effect

	ldx #<nextFile
	jmp loadnext


initloader:
	ldx #<loadersize
#if loadersize > 256
#echo "irq loader too long!"
#err
#endif
xferloop$:
	lda irqload - 1,x
	dex
	sta irqorig,x
	bne xferloop$

; send the m-w command to write the data
mwloop$:
	jsr inidev$
	ldx #lmwcmd$ - 1
smwcmd$:
	lda mwcmd$,x
	jsr ciwait
	dex
	bpl smwcmd$

; send the actual data bytes  
	ldx #0
mwbyte$:
	lda drvcode,x
	jsr ciwait
	inx
	cpx #AMOUNT
	bne mwbyte$

; complete the command
	jsr unlsn
	jsr serialwait

; update the addresses
	clc
	lda #AMOUNT
	adc mwbyte$ + 1
	sta mwbyte$ + 1
	bcc noupdhi1$
	clc
	inc mwbyte$ + 2
noupdhi1$:

	lda #AMOUNT
	adc mwcmd$ + 2
	sta mwcmd$ + 2
	tax
	lda #0
	adc mwcmd$ + 1
	sta mwcmd$ + 1
	cpx #<edrvcode
	sbc #>edrvcode
	bcc mwloop$

	jsr inidev$
	lda #"U"
	jsr ciwait

	lda #"3"
	jsr ciwait

	jsr unlsn
	jsr serialwait
	rts
	;jmp $100d

; subroutine: make the current drive listen

inidev$:
	lda fa	; get the device number
	bne nodef$	; if not set, then use the default device number
	lda #DEFAULT_DEVICE
nodef$:
	sta fa		; save the device number
	jsr listn
	jsr serialwait
	lda #$6f
	jsr secnd
	jmp serialwait


; the m-w command backwards

mwcmd$:
	dc.b AMOUNT,>drive,<drive,"W-M"
lmwcmd$ = . - mwcmd$


ciwait:	jsr ciout
serialwait:
	ldy #55
0$	dey
	bne 0$
	rts



LEDFLASH = 3	; LED flashing level:
		; 0 = normal (LED constantly on while loading a file)
		; 1 = LED glows on and off while waiting for a command
		; 2 = LED on only while reading sectors
		; 3 = 1 + 2


; =========================================================================
; the drive code
; =========================================================================

drvcode:

#rorg $500

;---------------------------------------
; The fastload routine for the drive
;---------------------------------------
; The 1581-code and some optimization by Pasi Ojala, albert@cs.tut.fi

PARANOID = 0	;1
		; The 1581 docs say that track 40, 0 contains
		; a pointer to the first directory block.
		; 1=use it, 0=make a guess (track 40, sector 3)
;---------------------------------------
; FOR 1581
;---------------------------------------
;acsbf8	= $03	; job for buffer 1 (not used directly)
trkbf8	= $0d	; track for job 1
sctbf8	= $0e	; sector for job 1

;ciapa	= $4000	; (not used directly)
ciapb	= $4001
ledon	= $cbcc	; activity led on: $cbcc, off: $cbc3
ledoff	= $cbc3	; or using the job queue:  on $94 / off $96

;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------
acsbf	= $01	; access to buffer 1
trkbf	= $08	; track of buffer 1
sctbf	= $09	; sector of buffer 1
iddrv0	= $12	; id of drive 0
id	= $16	; id

via1pb	= $1800
via2pb	= $1c00

;---------------------------------------
; For both 154x/7x and 1581
;---------------------------------------
buf	= $0400	; sector buffer (for job 1)
datbf	= $14	; databuffer - temporary (on 1581 sector for job 4)


drive	cld
	lda $ff54	; Execute Job -routine entry in the jump table
	cmp #$4c	; jmp abs	- probably exists
	beq drive1581
	cmp #$6c	; jmp (abs)	- probably exists
	beq drive1581
	jmp drive1571

name	ds.b 8


drive1581		; interrupts always enabled
#if !(LEDFLASH & 1)
	jsr ledoff
#endif
	ldy #0
nl$	jsr recv$	; get the file name, first char
	sta name,y
	iny
	cpy #8		; 8 chars
	bne nl$
#if !(LEDFLASH & 2)
	jsr ledon
#endif

#if PARANOID
	ldx #40		; get the root block
	ldy #0
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldx buf		; read the disk directory (track 40, sector 3 (usually))
	ldy buf+1
#else
	ldx #40
	ldy #3
#endif
dirloop$
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldy #2
nextfile$
	lda buf,y	; check file type
	and #$87
	cmp #$82	; must be PRG
	bne notfound$

	sty notfound1$+1
	ldx #0
cl$	lda buf+3,y	; check the first characters
	cmp name,x
	bne notfound1$
	iny
	inx
	cpx #8
	bne cl$

;found$
	; Error led flash off
	lda #$9a
	ldx #1
	jsr $ff54	; execute command for queue 1, return value in A also

	ldy notfound1$+1
	ldx buf+1,y	; get the track and sector numbers
	lda buf+2,y
	tay
nextsect$
	jsr readsect$
	bcs errquit$	; quit if the sector could not be read
	ldy #255
	lda buf
	bne notlast$	; if the track is nonzero, this wasn't the last sector

	ldy buf+1	; last sector: get the index of last valid byte
notlast$
	sty numlast$+1

	ldy #1		; skip the track and sector when sending the buffer
sendbuf$		; send the buffer contents to the computer
	ldx buf+1,y
	cpx #ESCBYTE
	bne noesc$

	jsr send$	; escape the escape character
	ldx #ESCBYTE
noesc$	jsr send$
	iny
numlast$
	cpy #255	; last valid byte in the sector?
	bne sendbuf$

	ldy buf+1	; the track and sector of next block
	ldx buf		; note the load order - buf[0] is zero for last block
	bne nextsect$	; loop until all sectors are loaded

finish$
	ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
	jsr send$	; returns with X=0 and Z set
	;ldx #0
	jsr send$	; returns with X=0 and Z set
	jmp drive1581


notfound1$
	ldy #0
notfound$
	tya
	clc
	adc #$20
	tay
	bcc nextfile$

	ldy buf+1	; get next sector
	ldx buf		; and track
	bne dirloop$	; keep trying until the last directory block has been searched
	; file not found: fall through
errquit$
	ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
	jsr send$	; returns with X=0 and Z set
	inx		;ldx #1
	jsr send$	; returns with X=0 and Z set

	; Error led flash on
	lda #$98
	inx		;ldx #1
	jsr $ff54	; execute command for queue 1, return value in A also
	jmp drive1581


;---------------------------------------
; readsect$: read a sector

readsect$
	stx trkbf8
	sty sctbf8
#if LEDFLASH & 2
	jsr ledon
#endif
	lda #$80	; $80 - read sector command
	ldx #1		; job queue #1
	jsr $ff54	; execute command A for queue X, return value in A too
	cmp #2		; 0 and 1 (0-2 and 1-2) clear, errors set carry
#if LEDFLASH & 2
	jmp ledoff	; led off (does not affect carry)
#else
	rts
#endif

; send$ sends the X register contents. datbf is used as temporary storage.
; returns with X=0 and Z set
send$	stx datbf
	ldx #8		; send 8 bits
sendb$	lsr datbf	; read next bit
	lda #2		; prepare for CLK=high, DATA=low
	bcs sskip$
	lda #8		; prepare for CLK=low, DATA=high
sskip$	sta ciapb	; send the data
sack$	lda ciapb	; wait for CLK==DATA==low
	and #5
	eor #5
	bne sack$
	sta ciapb	; set CLK=DATA=high (A=0)
	lda #5
swait$	bit ciapb
	bne swait$	; wait for CLK==DATA==high
	dex
	bne sendb$	; loop until all bits have been sent
	rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$	sty y$+1
#if LEDFLASH & 1
	ldy #0		; LED brightness (0=dim, 255=lit)
	tsx
fincr$	jsr doflash$
	ldy datbf
	iny
	bne fincr$
fdecr$	dey
	jsr doflash$
	ldy datbf
	bne fdecr$
	beq fincr$
doflash$
	sty datbf	; store the counter for LED flashing
	jsr ledoff
	jsr fdelay$	; perform the delay
	jsr ledon
	lda datbf
	eor #$ff
	tay		; fall through
fdelay$
	lda #$85
	and ciapb	; wait for any signal from the bus
	bne flashdone$
	iny
	bne fdelay$
	rts
flashdone$
	jsr ledoff
	txs		; discard the return address
#endif

	ldx #8		; counter: receive 8 bits
recvb$	lda #$85
	and ciapb	; wait for CLK==low || DATA==low
	beq recvb$
	bmi gotatn$	; quit if ATN was asserted
	lsr		; read the data bit
	lda #2		; prepare for CLK=high, DATA=low
	bcc rskip$
	lda #8		; prepare for CLK=low, DATA=high
rskip$	sta ciapb	; acknowledge the bit received
	ror datbf	; and store it
rwait$	lda ciapb	; wait for CLK==high || DATA==high
	and #5
	eor #5
	beq rwait$
	lda #0
	sta ciapb	; set CLK=DATA=high
	dex
	bne recvb$	; loop until all bits have been received
	lda datbf	; read the data to A
y$	ldy #0
	rts

gotatn$	pla		; If ATN gets asserted, exit to the operating system.
	pla		; Discard the return address.
	cli		; Enable the interrupts.
	rts


;edrvcode


;---------------------------------------
; FOR 1540/41/70/71
;---------------------------------------


drive1571
	;cld
	cli		; interrupts enabled until first sector read
#if !(LEDFLASH & 1)
	lda #$f7	; led off
	and via2pb
	sta via2pb
#endif
	ldy #0
nl$	jsr recv$	; get the file name, first char
	sta name,y
	iny
	cpy #8		; 8 chars
	bne nl$

#if !(LEDFLASH & 2)
	lda #8
	ora via2pb
	sta via2pb	; led on
#endif

	ldx #18
	ldy #1		; read the disk directory (track 18, sector 1)
dirloop$
	jsr readsect$	; read the sector
	bcs errquit$	; quit if it could not be read

	ldy #$02
nextfile$
	lda buf,y	; check file type
	and #$83
	cmp #$82	; must be PRG
	bne notfound$

	sty notfound1$+1
	ldx #0
cl$	lda buf+3,y	; check the first characters
	cmp name,x
	bne notfound1$
	iny
	inx
	cpx #8
	bne cl$

found$	ldy notfound1$+1
	ldx buf+1,y	; get the track and sector numbers
	lda buf+2,y
	tay

nextsect$
	jsr readsect$
	bcs errquit$	; quit if the sector could not be read
	ldy #255
	lda buf
	bne notlast$	; if the track is nonzero, this wasn't the last sector

	ldy buf+1	; last sector: get sector length
notlast$
	sty numlast$+1

	ldy #1		; skip the track and sector when sending the buffer
sendbuf$		; send the buffer contents to the computer
	ldx buf+1,y
	cpx #ESCBYTE
	bne noesc$

	jsr send$	; escape the escape character
	ldx #ESCBYTE

noesc$	jsr send$
	iny
numlast$
	cpy #255	; were all bytes of the block sent?
	bne sendbuf$

	ldy buf+1	; store the track and sector of next block
	ldx buf
	bne nextsect$	; loop until all sectors are loaded

finish$	ldx #ESCBYTE	; send the escape byte followed by 0 to notify the computer
	jsr send$	; returns with X=0 and Z set
	;ldx #0
	jsr send$	; returns with X=0 and Z set
	jmp drive1571

notfound1$
	ldy #0
notfound$
	tya
	clc
	adc #$20
	tay
	bcc nextfile$

	ldy buf+1	; get next sector
	ldx buf		; and track
	bne dirloop$	; keep trying until the last directory block has been searched
	; file not found: fall through
errquit$
	ldx #ESCBYTE	; send the escape byte followed by 1 to notify the computer
	jsr send$	; returns with X=0 and Z set
	inx		;ldx #1
	jsr send$	; returns with X=0 and Z set
	jmp drive1571


;---------------------------------------
; readsect$: read a sector

readsect$
	stx trkbf
	sty sctbf
#if LEDFLASH & 2
	lda #8
	ora via2pb
	sta via2pb	; turn the LED on
#endif
	ldy #RETRIES	; load the retry count
	cli		; enable interrupts, so that the command will be executed
retry$	lda #$80
	sta acsbf	; code for reading the sector
poll1$	lda acsbf	; wait for the command to complete
	bmi poll1$
	cmp #1
	bne noexit$
#if LEDFLASH & 2
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
#endif
	clc
	sei		; disable interrupts again to make the program faster
	rts		; success: exit the loop

noexit$	dey		; decrement the retry count
	bmi error$	; quit if there were too many retries

	cpy #RETRIES / 2
	bne skipcode$

	lda #$c0
	sta acsbf	; half the retries left: knock the head (seek track 1)
skipcode$
	lda id		; tolerate disk id changes
	sta iddrv0
	lda id+1
	sta iddrv0+1

poll2$	lda acsbf	; wait for the command to complete
	bmi poll2$
	bpl retry$	; branch always

error$
#if LEDFLASH & 2
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
#endif
	sec
	sei
	rts

; send$ sends the X register contents. datbf is used as temporary storage.
; returns with X=0 and Z set

send$	stx datbf
	ldx #8	; send 8 bits
; sendbit$ sends a bit
sendb$	lsr datbf	; read next bit
	lda #2		; prepare for CLK=high, DATA=low
	bcs sskip$
	lda #8		; prepare for CLK=low, DATA=high
sskip$	sta via1pb	; send the data
sack$	lda via1pb	; wait for CLK==DATA==low
	and #5
	eor #5
	bne sack$
	sta via1pb	; set CLK=DATA=high
	lda #5
swait$	bit via1pb
	bne swait$	; wait for CLK==DATA==high
	dex
	bne sendb$	; loop until all bits have been sent
	rts

;---------------------------------------
; recv$ receives a byte to A. datbf is used as temporary storage.

recv$	sty y$+1
#if LEDFLASH & 1
	ldy #0		; LED brightness (0=dim, 255=lit)
	tsx
fincr$	jsr doflash$
	ldy datbf
	iny
	bne fincr$
fdecr$	dey
	jsr doflash$
	ldy datbf
	bne fdecr$
	beq fincr$

doflash$
	sty datbf	; store the counter for LED flashing
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
	jsr fdelay$	; perform the delay
	lda #8
	ora via2pb
	sta via2pb	; turn the LED on
	lda datbf
	eor #$ff
	tay		; fall through

fdelay$	lda #$85
	and via1pb	; wait for any signal from the bus
	bne flashdone$
	iny
	bne fdelay$
	rts

flashdone$
	lda #$f7
	and via2pb
	sta via2pb	; turn the LED off
	txs		; discard the return address
#endif

	ldx #8		; counter: receive 8 bits
recvbit$
	lda #$85
	and via1pb	; wait for CLK==low || DATA==low
	bmi gotatn$	; quit if ATN was asserted
	beq recvbit$
	lsr		; read the data bit
	lda #2		; prepare for CLK=high, DATA=low
	bcc rskip$
	lda #8		; prepare for CLK=low, DATA=high
rskip$	sta via1pb	; acknowledge the bit received
	ror datbf	; and store it
rwait$	lda via1pb	; wait for CLK==high || DATA==high
	and #5
	eor #5
	beq rwait$
	lda #0
	sta via1pb	; set CLK=DATA=high

	dex
	bne recvbit$	; loop until all bits have been received
	lda datbf	; read the data to A
y$	ldy #0
	rts

gotatn$	pla		; If ATN gets asserted, exit to the operating system.
	pla		; Discard the return address.
	cli		; Enable the interrupts.
	rts

edrvcode
;#endif

#rend
; =========================================================================


	; Code by Albert of Pu-239	http://www.cs.tut.fi/~albert/
	; Except part of the loader code originally by Marko Mäkelä


; =========================================================================
; The asynchronous 1540/1541/1570/1571 fast loader, computer's part
; =========================================================================

irqload
#rorg $31a
irqorig:

SYSTEMSEL	; $31a
	dc.b 0		; 0 for NTSC
AUTO		; $31b
	dc.b 1		; non-zero for automatic control

; I/O constants and the variables

iecport1 = $912c	;$dd00	;$912c
dato = 32		;32	;32
clko = 2		;16	;2
iecport2 = $911f	;$dd00	;$911f
atno = 128		;8	;128
clki = 1		;64	;1
dati = 2		;128	;2

LOADER			; $31c
	sei
	lda #$7f
	sta $912e	; disable and acknowledge (timer) interrupts
	sta $912d
	lda #$bf	; normal IRQ just in case
	sta $314
	lda #$ea
	sta $315
	; $32f
	stx name$+1
	sty name$+2

	lda iecport1
	and #255 - dato - clko
	sta iec1d1a$	; CLK=1, DATA=1
	sta iec1d1b$
	eor #clko
	sta iec0d1a$	; CLK=0, DATA=1
	sta iec0d1b$

	lda #0
	ldx #15	; $34b
clr$	sta $9000,x	; color, volume, rows, columns, etc.
	dex
	bpl clr$

	tay	; ldy #0
name$	lda $aaaa,y
	jsr putbyt$	; send the file name (8 chars)
	iny
	cpy #8
	bne name$

	jsr getbyt$	; get the start address
	tay
	jsr getbyt$
	sta adrhi$	; use the load address..

loadloop$:
	jsr getbyt$	; get next file byte, exit on completion
adrhi$ = . + 2
	sta $100,y	; store it
	iny
	bne loadloop$
	inc adrhi$
	jmp loadloop$

;---------------------------------------
; getbyt$: get a byte, interpret the escape codes

getbyt$:
	jsr getbits$
	cmp #ESCBYTE
	bne getdone$
	jsr getbits$	; escape char fetched, get another byte
	cmp #ESCBYTE	; another escape char: it is a literal
	beq getdone$

	cmp #1		; Transfer finished. 0=ok, nonzero=error.
	pla		; Set the C flag accordingly.
	pla		; discard the return address

fail$	bcs fail$
	jmp 4109	;$100d


; getbits$: get a byte

getbits$:
	ldx #8	; counter: get 8 bits
getbit$:
	lda iecport2
	and #dati | clki
	eor #dati | clki
	beq getbit$	; wait for CLK==low || DATA==low

#if dati == 128
	asl		; Carry = DATA==low
#else
#if dati < clki
	and #dati
#endif
	cmp #dati
#endif

iec0d1a$ = . + 1
	lda #255 - dato
	bcs gskip$
	eor #dato | clko
gskip$:	sta iecport1	; acknowledge the bit
	ror store$	; store the data

	lda #dati | clki
wgack$:	bit iecport2
	beq wgack$	; wait for CLK==high || DATA==high

iec1d1a$ = . + 1
	lda #255 - clko - dato
	sta iecport1	; raise CLK and DATA
	dex
	bne getbit$	; loop until all bits are received
store$ = . + 1
	lda #0
getdone$:
	rts

; putbyt$ puts a byte

putbyt$:
	sta store$
	ldx #8	; counter: send all 8 bits
putbit$:
	lsr store$	; read a bit
iec0d1b$ = . + 1
	lda #255 - dato
	bcc pskip$
	eor #dato | clko
pskip$:	sta iecport1	; send the data

	lda #dati | clki
wputack1$:
	bit iecport2
	bne wputack1$	; wait for CLK==DATA==low

iec1d1b$ = . + 1
	lda #255 - clko - dato
	sta iecport1	; set DATA=CLK=high
wputack2$:
	lda iecport2
	and #dati | clki
	eor #dati | clki
	bne wputack2$	; wait for CLK==DATA==high
	dex
	bne putbit$	; loop until all bits are sent
	rts

loaderend:
#rend
loadersize = . - irqload


#if SYSTEM & PAL
SPD eqm 3					; song speed 3/2
#else
SPD eqm 5					; song speed 4/2
#endif

mussrc
#rorg $1e00
MUS
#include "../durplayer/sng-jack.dur"
#include "../durplayer/player-dur.a65"
#rend
musend

	org $1a00
matrix

	org $1d54
	; color luminance order
	; 062485371

nextFile:
	dc.b "3K-VIMMII"	; 8 significant chars

chtextcol:
0$	lda #0
	and #7
	tay
	lda cnt$
	asl
	adc acc$
	sta acc$
	bcc 1$
	iny
1$	lda COLO2$,y
	sta $900f

	inc cnt$
	inc cnt$
	inc cnt$
	inc cnt$
	bpl 2$
	lda #0
	sta cnt$
	inc 0$+1
2$	clc
	rts
cnt$	dc.b 0
acc$	dc.b 0

COLO2$	dc.b $13+8,$16+8,$10+8,$10+8


loadnext
	lda #0
	beq loadnext

	; Fix loader code to return instead of running the code
	lda #3
	sta $034b	; ldx #15 -> ldx #3 to only clear $9000..$9003
	lda #$60	; rts
	sta $0389	; $0389	bcs $0389

	;ldx #<nextFile
	ldy #>nextFile
	jsr LOADER+19
	bcs *

	; Fix loader code
	lda #15
	sta $034b	; ldx #15
	lda #$b0	; bcs
	sta $0389	; $0389	bcs $0389
#if 0
	lda #$bf	; normal IRQ just in case
	sta $314
	lda #$ea
	sta $315
#endif
	bcs *		; failed to load!
	lda $100d
	cmp #$78
	beq 0$
	jmp $100d	; no sei in short decompressor!
0$	jmp $100e


irq	jsr chtextcol
	lda $9001
	;clc
	adc #1
	sta $9001

	and #1
	bne done$

	lda $9003
	and #127
	;clc
	sbc #1
	bcs doing$

	inc loadnext+1
	lda #<done$
	sta $314
	lda #>done$
	sta $315
	lda #0

doing$	sta $9003
done$	jsr player_update	; must be high enough!
	jsr player_update
#if SYSTEM & NTSC
	jsr player_update
#endif
	jmp $eb15	; leave IRQ, ack timer interrupt
	; should not reach screen memory!

