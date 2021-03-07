all:
	#mkdir bin
	cd c ; make
	cd loader ; make install
	cd vimmii ; make install
	cd vicpic ; make install
	cd split ; make install
	cd shekki ; make install
	cd plasma ; make install
	cd dycp ; make install
	cd credits ; make install
	make disk

disk:
	c1541 -format vimmii,06 d64 vimmii.d64
	c1541 -attach vimmii.d64 \
	-delete vimmii.prg 3k-vimmii 3k-vicpic 3k-vgfxa \
	3k-vgfxb 3k-vcolb 3k-vgfxc 3k-vcolc 3k-vgfxd 3k-vcold \
	3k-vgfxe 3k-vcole 3k-vgfxf 3k-vcolf 3k-vgfxg 3k-vcolg \
	3k-vgfxh 3k-vcolh 3k-vgfxi 3k-vcoli 3k-vgfxj 3k-vcolj \
	3k-split 3k-shekki 3k-plasma 3k-dycpi 3k-crediz \
	-write bin/vimmii.prg \
	-write bin/3k-vimmii \
	-write bin/3k-vicpic \
	-write bin/3k-vgfxa \
	-write bin/3k-vgfxb \
	-write bin/3k-vcolb \
	-write bin/3k-vgfxc \
	-write bin/3k-vcolc \
	-write bin/3k-vgfxd \
	-write bin/3k-vcold \
	-write bin/3k-vgfxe \
	-write bin/3k-vcole \
	-write bin/3k-vgfxf \
	-write bin/3k-vcolf \
	-write bin/3k-vgfxg \
	-write bin/3k-vcolg \
	-write bin/3k-vgfxh \
	-write bin/3k-vcolh \
	-write bin/3k-vgfxi \
	-write bin/3k-vcoli \
	-write bin/3k-vgfxj \
	-write bin/3k-vcolj \
	-write bin/3k-split \
	-write bin/3k-shekki \
	-write bin/3k-plasma \
	-write bin/3k-dycpi \
	-write bin/3k-crediz 

