#CFLAGS = code=near data=auto nodebug stringmerge stringsect=code verbose


all: 3k-vicpic ppmtovic
# victoilbm

install: 3k-vicpic graphics 00.c 01.c 07.c 02.c 0466.c parrot.c pata.c rose.c shuksan.c
# 03.c spider.c
	cp 3k-vicpic ../bin/3k-vicpic
	cp graphics ../bin/3k-vgfxa
	cp 00.c ../bin/3k-vcolb
	cp 00.g ../bin/3k-vgfxb
	cp pata.c ../bin/3k-vcolc
	cp pata.g ../bin/3k-vgfxc
	cp 01.c ../bin/3k-vcold
	cp 01.g ../bin/3k-vgfxd
	cp shuksan.c ../bin/3k-vcole
	cp shuksan.g ../bin/3k-vgfxe
	cp 02.c ../bin/3k-vcolf
	cp 02.g ../bin/3k-vgfxf
	cp rose.c ../bin/3k-vcolg
	cp rose.g ../bin/3k-vgfxg
	cp 0466.c ../bin/3k-vcolh
	cp 0466.g ../bin/3k-vgfxh
	cp parrot.c ../bin/3k-vcoli
	cp parrot.g ../bin/3k-vgfxi
	cp 07.c ../bin/3k-vcolj
	cp 07.g ../bin/3k-vgfxj

3k-vicpic: vicpic
	pucrunch -c20 vicpic 3k-vicpic

vicpic: vicpic.asm colors graphend
	dasm vicpic.asm -ovicpic -v3 -lvicpic.lst -MSYSTEM=NTSC

splitstuff: splitstuff.c
	gcc -Wall -O -o $@ $<

colors: splitstuff pic.vic
	./splitstuff pic.vic colors graphics graphend

graphics: splitstuff pic.vic
	./splitstuff pic.vic colors graphics graphend

graphend: splitstuff pic.vic
	./splitstuff pic.vic colors graphics graphend


ppmtovic: ppmtovic.c
	gcc -Wall -O -o $@ $<

#victoilbm: victoilbm.c
#	gcc -Wall -O -o $@ $<

pic.vic: 14.v
	cp 14.v pic.vic

14.v: 14.ppm ppmtovic
	./ppmtovic -u11010101110000 -c75 -b-10 14.ppm 14.v
#	./victoilbm 14.v 14.iff

14.ppm: 14.jpeg
	djpeg 14.jpeg >14.ppm


00.ppm: 00.jpeg
	djpeg 00.jpeg >00.ppm

01.ppm: 01.jpeg
	djpeg 01.jpeg >01.ppm

02.ppm: 02.jpeg
	djpeg 02.jpeg >02.ppm

0466.ppm: 0466.jpg
	djpeg 0466.jpg >0466.ppm

07.ppm: 07.jpeg
	djpeg 07.jpeg >07.ppm


00.c: 00.v splitstuff
	./splitstuff 00.v 00.c 00.g

01.c: 01.v splitstuff
	./splitstuff 01.v 01.c 01.g

02.c: 02.v splitstuff
	./splitstuff 02.v 02.c 02.g

0466.c: 0466.v splitstuff
	./splitstuff 0466.v 0466.c 0466.g

07.c: 07.v splitstuff
	./splitstuff 07.v 07.c 07.g

00.g: 00.v splitstuff
	./splitstuff 00.v 00.c 00.g

01.g: 01.v splitstuff
	./splitstuff 01.v 01.c 01.g

02.g: 02.v splitstuff
	./splitstuff 02.v 02.c 02.g

0466.g: 0466.v splitstuff
	./splitstuff 0466.v 0466.c 0466.g

07.g: 07.v splitstuff
	./splitstuff 07.v 07.c 07.g

00.v: 00.ppm ppmtovic
	./ppmtovic -u1101011111000 -d1 -c23 -b28 00.ppm 00.v
#	./victoilbm 00.v 00.iff

01.v: 01.ppm ppmtovic
	./ppmtovic -u1101011111000 -d1 -c30 -b0 01.ppm 01.v
#	./victoilbm 01.v 01.iff

07.v: 07.ppm ppmtovic
	./ppmtovic -u110101111100 -d1 -c20 -b-10 07.ppm 07.v
#	./victoilbm 07.v 07.iff

02.v: 02.ppm ppmtovic
	./ppmtovic -l4 -u1101101111000 -d1 -c90 -b60 02.ppm 02.v
#	./victoilbm 02.v 02.iff

0466.v: 0466.ppm ppmtovic
	./ppmtovic -u1101011111000 -d1 -c0 -b-10 0466.ppm 0466.v
#	./victoilbm 0466.v 0466.iff

03.v: 03.ppm ppmtovic
	./ppmtovic -l6 -u11010111110000 -d1 -c100 -b15 03.ppm 03.v
#	./victoilbm 03.v 03.iff


parrot.c: parrot.v
	./splitstuff parrot.v parrot.c parrot.g

pata.c: pata.v
	./splitstuff pata.v pata.c pata.g

rose.c: rose.v
	./splitstuff rose.v rose.c rose.g

spider.c: spider.v
	./splitstuff spider.v spider.c spider.g

shuksan.c: shuksan.v
	./splitstuff shuksan.v shuksan.c shuksan.g


parrot.v: parrot.ppm
	./ppmtovic -l3 -u11110101110000 -d1 -c35 -b25 parrot.ppm parrot.v
#	./victoilbm parrot.v parrot.iff

pata.v: pata.ppm
	./ppmtovic -l4 -u11110001110000 -d2 -c0 -b0 pata.ppm pata.v
#	./victoilbm pata.v pata.iff

rose.v: rose.ppm
	./ppmtovic -u11111111110000 -d1 -c30 -b25 rose.ppm rose.v
#	./victoilbm rose.v rose.iff

spider.v: spider.ppm
	./ppmtovic -u11110011110000 -d2 -c100 -b32 spider.ppm spider.v
#	./victoilbm spider.v spider.iff

shuksan.v: shuksan.ppm
	./ppmtovic -l6 -u11010111100000 -d2 -c20 -b0 shuksan.ppm shuksan.v
#	./victoilbm shuksan.v shuksan.iff


parrot.ppm: parrot.jpg
	djpeg parrot.jpg >parrot.ppm

pata.ppm: pata.jpg
	djpeg pata.jpg >pata.ppm

rose.ppm: rose.jpg
	djpeg rose.jpg >rose.ppm

spider.ppm: spider.jpg
	djpeg spider.jpg >spider.ppm

shuksan.ppm: shuksan.jpg
	djpeg shuksan.jpg >shuksan.ppm

