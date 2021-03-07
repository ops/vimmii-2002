
all: 3k-dycpi

install: 3k-dycpi
	cp 3k-dycpi ../bin/

3k-dycpi: dycpi
	pucrunch -c20 dycpi 3k-dycpi -fshort

dycpi: dycp.asm dycp.data
	dasm dycp.asm -odycpi -v3 -ldycpi.lst -MSYSTEM=NTSC

dycp.data: dycp.ppm
	../c/ppmtologo -0 dycp.ppm dycp.data

dycp.ppm: dycp.iff
	ilbmtoppm dycp.iff >dycp.ppm

