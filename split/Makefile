
all: 3k-split

install: 3k-split
	cp 3k-split ../bin/


3k-split: split
	pucrunch -c20 split 3k-split -fshort

split: split.asm
	dasm split.asm -osplit -v3 -lsplit.lst -MSYSTEM=NTSC
