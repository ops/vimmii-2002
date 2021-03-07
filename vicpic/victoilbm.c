#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <exec/memory.h>
#include <proto/exec.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <proto/intuition.h>
#include <dos/dos.h>
#include <libraries/iff.h>

void __regargs __chkabort(void);
void __regargs __chkabort(void){}
void chkabort(void) {}
int CXBRK(void){return 0;}


#define W25		/* 28/25 columns, i.e. 112x144/100x160 pixels */

#ifdef W25
#define WIDTH 100
#define HEIGHT 160
#else
#define WIDTH 112
#define HEIGHT 144
#endif /* W25 */


const char *vers="\0$VER: victoppm 1.0 (12.1.98)\n";

UBYTE gfxmem[5120];



void main(int argc, char *argv[]) {
    FILE *handle;

    struct Library *IFFBase=NULL;
    struct GfxBase *GfxBase=NULL;
    struct RastPort myrastport;
    struct BitMap *bm=NULL;
    static const UWORD colortable[16]={ 0x000,0xfff,0xd00,0x0fd,
					0xf0d,0x0f0,0x00d,0xee0,
					0xe60,0xeca,0xfcc,0xcff,
					0xfcf,0xcfc,0xccf,0xffc};
    int x, y, ind, pos, bits;
    int g[4], colbits;

    if(argc != 3) {
	fprintf(stderr, "%s\n",vers+7);
	fprintf(stderr, "Usage: %s vicfile ilbmfile\n",argv[0]);
	exit(10);
    }

    if((handle = fopen(argv[1], "rb"))) {
	char tmp[2];

	fprintf(stderr, "Converting %s\n", argv[1]);

	fread(tmp, 2, 1, handle); /* Load address */
	fread(gfxmem+768, 5120-768, 1, handle);
	fread(gfxmem, 768, 1, handle);

	if((GfxBase = (struct GfxBase *)OpenLibrary("graphics.library", 39L))) {
	    if((bm = AllocBitMap(3*WIDTH,HEIGHT,4,BMF_CLEAR | BMF_MINPLANES, NULL))) {
		InitRastPort(&myrastport);
		myrastport.BitMap = bm;

		for(y=0;y<HEIGHT;y++) {
		    for(x=0;x<WIDTH;x++) {
#ifndef W25
			ind = x/4 + (y/16)*28 + ((y&8)<<5);	/* color memory index */
			pos = y%16 + (x/4)*16 + (y/16)*16*28;	/* grafix memory byte */
#else
			ind = x/4 + (y/16)*25 /*+ ((y&8)<<5)*/;
			pos = y%16 + (x/4)*16 + (y/16)*16*25;
#endif /* W25 */
			bits = (x%4);				/* bit numbers */

			g[0] = (gfxmem[0x1300+HEIGHT-y]>>4) & 15;	/* == background */
			g[1] = (gfxmem[0x1300+HEIGHT-y] & 7);		/* Border */
			g[2] = (gfxmem[0x1000+ind] & 7);		/* color memory */
			g[3] = (gfxmem[0x1200+HEIGHT-y]>>4) & 15;	/* Aux */

			colbits = (gfxmem[pos] >> (6-2*bits)) & 3;
			SetAPen(&myrastport, g[colbits]);
			WritePixel(&myrastport, 3*x+0, y);
			WritePixel(&myrastport, 3*x+1, y);
			WritePixel(&myrastport, 3*x+2, y);
		    }
		}

		if((IFFBase = OpenLibrary(IFFNAME, IFFVERSION))) {
		    if(!IFFL_SaveBitMap(argv[2], bm, (WORD *)colortable, 1)) {
			fprintf(stderr,"IFF save failed, returncode %d\n",IFFL_IFFError());
		    }
		    CloseLibrary(IFFBase);
		} else {
		    fprintf(stderr,"Could not open %s v%d\n",IFFNAME,IFFVERSION);
		}
		FreeBitMap(bm);
	    } else {
		fprintf(stderr,"Could not allocate bitmap\n");
	    }
	    CloseLibrary((struct Library *)GfxBase);
	}
	fclose(handle);
    } else {
	fprintf(stderr, "Could not access %s\n", argv[1]);
	exit(10);
    }
}


