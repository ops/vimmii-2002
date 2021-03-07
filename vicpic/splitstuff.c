#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char UBYTE;

UBYTE gfxmem[5120];

int main(int argc, char *argv[]) {
    FILE *handle;

    if(argc != 5 && argc != 4) {
	fprintf(stderr, "Usage: %s vicfile colors gfx$1000 gfx$1600\n", argv[0]);
	fprintf(stderr, "Usage: %s vicfile colors$9400 gfx$1000\n", argv[0]);
	exit(10);
    }

    if((handle = fopen(argv[1], "rb"))) {
	char tmp[2];

	fprintf(stderr, "Converting %s\n", argv[1]);

	fread(tmp, 2, 1, handle); /* Load address */
	fread(gfxmem+768, 5120-768, 1, handle);
	fread(gfxmem, 768, 1, handle);

	fclose(handle);

	if (argc == 4) {
	    if((handle = fopen(argv[2], "wb"))) {
		unsigned char cols[2+1024];
		int i;
		cols[0] = 0x00;
		cols[1] = 0x94;

		for (i=0;i<256;i++) {
		    cols[2+0*256+i] = (gfxmem[4096+256+2*256+i]>>4) & 15;
		    cols[2+1*256+i] = (gfxmem[4096+256+2*256+i]>>0) & 15;
		    cols[2+2*256+i] = (gfxmem[4096+256+0*256+i]>>0) & 15;
		    cols[2+3*256+i] = (gfxmem[4096+256+1*256+i]>>4) & 15;
		}
		fwrite(cols, 2+3*256+161, 1, handle);
		fclose(handle);
	    }
	    if((handle = fopen(argv[3], "wb"))) {
		tmp[0] = 0;
		tmp[1] = 0x10; /* 0x1000 */
		fwrite(tmp, 2, 1, handle);
		fwrite(gfxmem, 4000 /*4096*/, 1, handle);
		fclose(handle);
	    }
	    return 0;
	}

	if((handle = fopen(argv[2], "wb"))) {
	    fwrite(gfxmem+4096+256, 5120-4096-256, 1, handle);
	    fclose(handle);
	}
	if((handle = fopen(argv[3], "wb"))) {
	    tmp[0] = 0;
	    tmp[1] = 16; /* 0x1000 */
	    fwrite(tmp, 2, 1, handle);
	    fwrite(gfxmem, 0x600 /*4000*/ /*4096*/, 1, handle);
	    fclose(handle);
	}
	if((handle = fopen(argv[4], "wb"))) {
#if 0
	    tmp[0] = 0;
	    tmp[1] = 16; /* 0x1600 */
	    fwrite(tmp, 2, 1, handle);
#endif
	    fwrite(gfxmem+0x600, 4000-0x600 /*4096*/, 1, handle);
	    fclose(handle);
	}
    } else {
	fprintf(stderr, "Could not access %s\n", argv[1]);
	exit(10);
    }
    return 0;
}


