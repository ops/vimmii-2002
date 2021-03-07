
all: 3k-vimmii

install: 3k-vimmii
	cp 3k-vimmii ../bin/

3k-vimmii: vimmii
	pucrunch -c20 vimmii 3k-vimmii -fshort -m5

vimmii: vimmii.asm packed.bin map.bin
	dasm vimmii.asm -ovimmii -v3 -lvimmii.lst -MSYSTEM=NTSC

# 00 - back, 01 - border, 10 - character, 11 - aux
bigfont.bin: bigfont.ppm
	../c/ppmtomcol -b1230301230222222 -0 bigfont.ppm bigfont.bin
#                        0123456789abcdef

bigfont.ppm: bigfont.iff
	ilbmtoppm bigfont.iff >bigfont.ppm

charpack: charpack.c
	gcc -O -o $@ $<

packed.bin: bigfont.bin charpack
	./charpack bigfont.bin packed.bin map.bin

map.bin: bigfont.bin charpack
	./charpack bigfont.bin packed.bin map.bin
