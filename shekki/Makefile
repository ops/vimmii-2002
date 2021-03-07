
all: 3k-shekki

install: 3k-shekki
	cp 3k-shekki ../bin/


3k-shekki: shekki
	pucrunch -c20 shekki 3k-shekki


shekki: shekki.asm
	dasm shekki.asm -oshekki -v3 -lshekki.lst -MSYSTEM=NTSC
