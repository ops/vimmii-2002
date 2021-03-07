#if POS == 1
        mac c		
        .byte (1+({1}-1)*12)/16,(1+({1}-1)*12)%16
        endm
        mac c#
        .byte (2+({1}-1)*12)/16,(2+({1}-1)*12)%16
        endm
        mac d
        .byte (3+({1}-1)*12)/16,(3+({1}-1)*12)%16 
        endm
        mac d#
        .byte (4+({1}-1)*12)/16,(4+({1}-1)*12)%16
        endm
        mac e
        .byte (5+({1}-1)*12)/16,(5+({1}-1)*12)%16
        endm
        mac f
        .byte (6+({1}-1)*12)/16,(6+({1}-1)*12)%16
        endm
        mac f#
        .byte (7+({1}-1)*12)/16,(7+({1}-1)*12)%16
        endm
        mac g
        .byte (8+({1}-1)*12)/16,(8+({1}-1)*12)%16
        endm
        mac g#
        .byte (9+({1}-1)*12)/16,(9+({1}-1)*12)%16
        endm
        mac a
        .byte (10+({1}-1)*12)/16,(10+({1}-1)*12)%16
        endm
        mac a#
        .byte (11+({1}-1)*12)/16,(11+({1}-1)*12)%16
        endm
        mac h
        .byte (12+({1}-1)*12)/16,(12+({1}-1)*12)%16
        endm

        mac quiet     ;play a silent tone for the preset duration
        .byte 0,0
        endm

	mac bd
	.byte 2,9	; 9+2*16 = 41
	endm
	mac sd
	.byte 2,10
	endm
	mac hh
	.byte 2,11
	endm

	mac t
	.byte {1},{2},{3},{4},{5},{6},{7},{8}
	endm
#else
        mac c		
        .byte 1+({1}-1)*12
        endm
        mac c#
        .byte 2+({1}-1)*12
        endm
        mac d
        .byte 3+({1}-1)*12
        endm
        mac d#
        .byte 4+({1}-1)*12
        endm
        mac e
        .byte 5+({1}-1)*12
        endm
        mac f
        .byte 6+({1}-1)*12
        endm
        mac f#
        .byte 7+({1}-1)*12
        endm
        mac g
        .byte 8+({1}-1)*12
        endm
        mac g#
        .byte 9+({1}-1)*12
        endm
        mac a
        .byte 10+({1}-1)*12
        endm
        mac a#
        .byte 11+({1}-1)*12
        endm
        mac h
        .byte 12+({1}-1)*12
        endm

        mac quiet     ;play a silent tone for the preset duration
        .byte 0
        endm

	mac bd
	.byte 41
	endm
	mac sd
	.byte 42
	endm
	mac hh
	.byte 43
	endm

	mac t
	.byte {2}+{1}*16,{4}+{3}*16,{6}+{5}*16,{8}+{7}*16
	endm
#endif
