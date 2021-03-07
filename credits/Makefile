
all: 3k-crediz

install: 3k-crediz
	cp 3k-crediz ../bin/


3k-crediz: credits
	pucrunch -x0x1001 -c20 credits 3k-crediz

credits: credits.asm font_h.bin map_h.bin font_n.bin map_h.bin
	dasm credits.asm -ocredits -v3 -lcredits.lst -MSYSTEM=NTSC
	#dasm credits.asm -ocredits -v3 -lcredits.lst -MSYSTEM=PAL




# 00 - back, 01 - border, 10 - character, 11 - aux
bigfont_h.bin: bigfont_h.ppm Makefile
	../c/ppmtomcol -b1230301230222222 -0 bigfont_h.ppm bigfont_h.bin
#                        0123456789abcdef
bigfont_n.bin: bigfont_n.ppm Makefile
	../c/ppmtomcol -b1230301230222222 -0 bigfont_n.ppm bigfont_n.bin
#                        0123456789abcdef

bigfont_h.ppm: bigfont_h.iff
	ilbmtoppm bigfont_h.iff >bigfont_h.ppm

font_h.bin: bigfont_h.bin charpack
	./charpack bigfont_h.bin font_h.bin map_h.bin

map_h.bin: bigfont_h.bin charpack
	./charpack bigfont_h.bin font_h.bin map_h.bin

font_n.bin: bigfont_n.bin charpack
	./charpack bigfont_n.bin font_n.bin map_n.bin

map_n.bin: bigfont_n.bin charpack
	./charpack bigfont_n.bin font_n.bin map_n.bin

charpack: charpack.c
	gcc -O -o $@ $<
