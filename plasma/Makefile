
all: 3k-plasma

install: 3k-plasma
	cp 3k-plasma ../bin/


3k-plasma: plasma
	pucrunch -c20 plasma 3k-plasma -fshort -m6

plasma: plasma.asm
	dasm plasma.asm -oplasma -v3 -lplasma.lst -MSYSTEM=NTSC
