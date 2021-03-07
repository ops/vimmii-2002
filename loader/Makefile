all: vimmii.prg

install: vimmii.prg
	cp vimmii.prg ../bin/

vimmii.prg: loader
	pucrunch -c20 loader vimmii.prg -fshort -m5

loader: loader.asm
	dasm loader.asm -oloader -v3 -lloader.lst -MSYSTEM=NTSC

