#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#define FAST_COL


typedef unsigned char UBYTE;
#define MAXINT 0x7FFFFFFF

char *vers="\0$VER: ppmtovic 1.0 (21.1.2000)\n";

static const UBYTE Overlay[]= {
    0x00,0x38,0x7C,0x6C,0x6C,0x6C,0x6C,0xFE,0xFE,0xE6,0xE6,0xE6,0xE6,0xE6,0x66,0x00,
    0x00,0x88,0x88,0x88,0x88,0xCF,0xCF,0xE9,0xE9,0xED,0xED,0xED,0xED,0xEF,0x67,0x00,
    0x00,0x00,0x00,0x00,0x00,0x1C,0xBE,0xA6,0xA6,0xBE,0xBC,0xB0,0xB6,0xBE,0x1C,0x00,
    0x00,0x03,0x03,0x03,0x03,0x77,0xF7,0x83,0x83,0xC3,0xE3,0xE3,0xE3,0xE3,0x61,0x00,
    0x00,0x0F,0x1F,0x19,0x19,0x99,0x99,0x19,0x19,0x19,0x99,0x99,0x99,0x9F,0x8F,0x00,
    0x00,0x1E,0xBF,0xB3,0xB3,0xB3,0xB3,0xB3,0xB3,0xB3,0xB3,0xB3,0xB3,0xBF,0x1E,0x00, /*00*/
/*    0x00,0x0F,0x1F,0x18,0x18,0x98,0x9F,0x0F,0x01,0x01,0x81,0x81,0x81,0x81,0x81,0x00,
      0x00,0x1E,0xBF,0xB1,0xB1,0xB1,0xBF,0x9F,0x83,0x83,0x83,0x83,0x83,0x83,0x83,0x81, =99*/
/*    0x00,0x1E,0xBF,0xB1,0xB1,0xB1,0xBF,0xBF,0xB9,0xB9,0xB9,0xB9,0xB9,0xB9,0xBF,0x9E, =98*/
};


UBYTE pic[256][100];	/* [y][x] */
/*
	25x10(+6)	100x160	AR=1.875
	24x10(+16)
	23x11(+3)	92x176	AR=1.568
	22x11(+14)
	21x12(+4)	84x192	AR=1.3125	~4:3
	20x12(+16)
	19x13(+9)	76x208	AR=1.096
	18x14(+4)	72x224	AR=0.964
	17x15(+1)	68x240	AR=0.85
	16x16(+-0)	64x256	AR=0.75		3:4
 */


#define Y(r,g,b) ((30*r+59*g+11*b)/100)
#define U(y,b)   (3*493*(b-y)/4096+128)
#define V(y,r)   (3*877*(r-y)/4096+128)

#define ERROR(y,y1,u,u1,v,v1) (abs(y-y1)+3*abs(u-u1)+3*abs(v-v1))

static char colUsed[16] = {1,1,1,1,1,1,1,1, 1,1,0,0,0,0,0,0};
UBYTE col[16][6]= {
/*    R   G   B        Y   U   V */
    0x00,0x00,0x00,    0,128,128,
    0xf0,0xf0,0xf0,  240,128,128,
    0xd0,0x00,0x20,   72,103,235,	/* red is not so pure */
    0x00,0xf0,0xd0,  168,153, 21,
    0xf0,0x00,0xd0,   98,179,219,
    0x00,0xf0,0x00,  141, 78, 38,
    0x00,0x00,0xd0,   26,205,112,
    0xe0,0xe0,0x00,  213, 52,145,

    0xe0,0x60,0x00,  128, 82,199,
    0xe0,0xc0,0xa0,  189,106,160,
    0xf0,0xc0,0xc0,  206,123,149,
    0xc0,0xf0,0xf0,  225,133,107,
    0xf0,0xc0,0xf0,  211,138,146,
    0xc0,0xf0,0xc0,  220,118,111,
    0xc0,0xc0,0xf0,  197,143,125,
    0xf0,0xf0,0xc0,  234,113,131,
};

const char * const colornames[]= {
    "black",
    "white",
    "red",
    "cyan",
    "purple",
    "green",
    "blue",
    "yellow",
    "orange",
    "light orange",
    "pink",
    "light cyan",
    "light purple",
    "light green",
    "light blue",
    "light yellow",
};

int Convert(int status, char *nimi, int height, int width,
	    int init0, int init1, int init2, int back, int lumratio);

#define MODE_VERBOSE    2
#define MODE_QUIET	4
#define MODE_OPTIMIZE   8

#define STAT_ERROR	32
#define STAT_USAGE	64

#define NOT_USED	0xff

static int errs[16][16];

void main(int argc, char *argv[]) {
    UBYTE *a, *buffer, color, apu[100], *b, ty, tv, tu;
    int i, x, y, error, temp, width, height, w, h, maxval;
    int brightness=0, contrast=0, dither=1, lumratio=7, inf=0, ouf=0, stat=0;
    int c0 = NOT_USED, c1 = NOT_USED, c2 = NOT_USED, c3 = NOT_USED;
    FILE *handle;
#ifdef FAST_COL
    static UBYTE closest[16][16][16];
#endif

#if 0
for(i=0;i<16;i++) {
    int y = Y(col[i][0], col[i][1], col[i][2]);

    printf("    %3d,%3d,%3d,  %3d,%3d,%3d,\n",
	   col[i][0], col[i][1], col[i][2],
	   y, U(y,col[i][2]), V(y,col[i][0]));
}
#endif
    for(i=0;i<16;i++) {
	int y = Y(col[i][0], col[i][1], col[i][2]);

	col[i][3] = y;
	col[i][4] = U(y, col[i][2]);
	col[i][5] = V(y, col[i][0]);
    }

    for(i=1;i<argc;i++) {
	if(argv[i][0]=='-') {
	    x = 1;
	    while(argv[i][x]) {
		switch(argv[i][x]) {
		case 'b':
		    brightness = atoi(argv[i]+x+1);
		    if(brightness<-255 || brightness>255) {
			fprintf(stderr, "Brightness should be -255..255!\n");
			brightness = 0;
			stat |= STAT_ERROR;
		    }
		    x = strlen(argv[i])-1;
		    break;
		case 'c':
		    contrast = atoi(argv[i]+x+1);
		    if(contrast<-100 || contrast>200) {
			fprintf(stderr, "Contrast should be -100..200!\n");
			contrast = 0;
			stat |= STAT_ERROR;
		    }
		    x = strlen(argv[i])-1;
		    break;
		/* dither -d<value> , default value is 4 */
		case 'd':
		    if(!(dither = atoi(argv[i]+x+1)))
			dither = 4;
		    x = strlen(argv[i])-1;
		    break;
		case 'l':
		    lumratio = atoi(argv[i]+x+1);
		    if(lumratio<0 || lumratio>20) {
			fprintf(stderr, "Luminance ration should be 0..20!\n");
			stat |= STAT_ERROR;
		    }
		    x = strlen(argv[i])-1;
		    break;
		case 'v':
		    stat |= MODE_VERBOSE;
		    break;
		case 'q':
		    stat |= MODE_QUIET;
		    break;
		case 'o':
		    stat |= MODE_OPTIMIZE;
		    break;
		case 'i':
		    x++;
		    c0 = (atoi(argv[i]+x) & 15);
		    while(argv[i][x]>='0' && argv[i][x]<='9')
			x++;
		    if(argv[i][x]=='/') {
			x++;
			c1 = (atoi(argv[i]+x) & 15);
			while(argv[i][x]>='0' && argv[i][x]<='9')
			    x++;
			if(argv[i][x]=='/') {
			    x++;
			    c2 = (atoi(argv[i]+x) & 15);
			    while(argv[i][x]>='0' && argv[i][x]<='9')
				x++;
			    if(argv[i][x]=='/') {
				x++;
				c3 = (atoi(argv[i]+x) & 15);
				while(argv[i][x]>='0' && argv[i][x]<='9')
				    x++;
			    }
			}
		    }
		    x--;
		    break;
		case 'u':
		    y = 0;
		    x++;
		    while(y<16 && (argv[i][x]=='0' || argv[i][x]=='1')) {
			colUsed[y++] = argv[i][x++]-'0';
		    }
		    x--;
		    break;
		default:
		    stat |= STAT_ERROR;
		    break;
		}
		x++;
	    }
	    if(stat & STAT_ERROR) {
		fprintf(stderr, "Error in the options!\n");
		break;
	    }
	} else if(!inf) {
	    inf = i;
	} else if(!ouf) {
	    ouf = i;
	} else {
	    stat |= STAT_ERROR;
	    fprintf(stderr, "Too many filenames!\n");
	    break;
	}
    }

    if((stat & STAT_ERROR)) {
	fprintf(stderr, "%s\n",vers+7);
	fprintf(stderr, "Usage: %s [-<options>] [ppmfile [vicfile]]\n",argv[0]);
	fprintf(stderr, "       d = do a simple dither in the color conversion (1-n)\n");
	fprintf(stderr, "       l = the importance of luminance in color conversion (0..20), default 7\n");
	fprintf(stderr, "       b = brighten or darken the picture (-255..255), default 0\n");
	fprintf(stderr, "       c = increase the contrast (-100..100 [0%%..200%%), default 0\n");
	fprintf(stderr, "       v = verbose, output extra information about the process\n");
	fprintf(stderr, "       q = quiet, no progress output\n");
	fprintf(stderr, "       o = optimize color selection (mean average error)\n");
	fprintf(stderr, "       i<colmem/hi/lo/back> = initialize colors (e.g. -i0/8/9/7)\n");
	fprintf(stderr, "       u1111111110000000 = use colors (default shown)\n");
	exit(10);
    }

    if(!(stat & MODE_QUIET)) {
	if(!ouf)
	    fprintf(stderr, "Outputting to stdout\n");
	if(!inf)
	    fprintf(stderr, "Reading from stdin\n");
    }

    if(!inf || (handle=fopen(argv[inf], "rb"))) {
	if(!inf) {
	    handle = stdin;
	} else {
	    if(!(stat & MODE_QUIET))
		fprintf(stderr, "%s:\n", argv[inf]);
	}

	fgets((char *)apu, 100, handle);
	if(!strncmp((char *)apu, "P6", 2)) {
	    do {
		fgets((char *)apu, 100, handle);
	    } while (!strncmp((char *)apu, "#", 1));
	    width = atoi((char *)apu);
	    i=0;
	    while(apu[i]!=' ' && apu[i]!='\t' && apu[i])
		i++;
	    height = atoi((char *)apu+i);
	    fgets((char *)apu, 100, handle);    /* just get the maxval..*/
	    maxval = atoi((char *)apu);

	    if(!(stat & MODE_QUIET))
		fprintf(stderr, "Picture is %d x %d (maxval=%d)\n", width, height, maxval);

	    if((buffer = (UBYTE *)calloc(3*width*height, 1))) {
		if(fread(buffer, 3*width*height, 1, handle) != 1)
		    fprintf(stderr,"Short file! Result may be quite original..\n");
		if(inf)
		    fclose(handle);

		if(maxval != 255) {
		    for(i=3*width*height-1;i>=0;i--) {
			buffer[i] = buffer[i]*255/maxval;
		    }
		}
		if(brightness || contrast) {
		    if(!(stat & MODE_QUIET)) {
			if(brightness>0)
			    fprintf(stderr, "Brightening the picture by %d\n", brightness);
			if(brightness<0)
			    fprintf(stderr, "Darkening the picture by %d\n", -brightness);
			if(contrast)
			    fprintf(stderr, "Adjusting the contrast to %3d%%\n", 100+contrast);
		    }
		    for(i=3*width*height-1;i>=0;i--) {
			temp = (buffer[i]+brightness-128)*(100+contrast)/100 + 128;
			if(temp<0)
			    temp = 0;
			else if(temp>255)
			    temp = 255;
			buffer[i] = temp;
		    }
		}

		if((width>100 || height>160) && !(stat & MODE_QUIET))
		    fprintf(stderr, "Output picture will be sized 100 x 160\n");

		w = width;
		h = height;
		if(w>100)
		    w = 100;
		if(h>160)
		    h = 160;

		if(!(stat & MODE_QUIET)) {
		    fprintf(stderr, "Converting colors");
#ifndef FAST_COL
		    fprintf(stderr, "\n");
#endif
		    fflush(stderr);
		}
#ifdef FAST_COL
		for(x=0;x<255;x+=16) {
		    for(y=0;y<255;y+=16) {
			int z;

			for(z=0;z<255;z+=16) {
			    /* Target YUV */
			    ty = Y(x,y,z);
			    tu = U(ty,z);
			    tv = V(ty,x);

			    error = MAXINT;
			    for(i=0;i<16;i++) {
				if(colUsed[i]) {
				    temp = ERROR(lumratio*ty, lumratio*col[i][3],
						 tu, col[i][4],
						 tv, col[i][5]);
				    if(temp<error) {
					color = i;
					error = temp;
				    }
				}
			    }
			    closest[x/16][y/16][z/16] = color;
			}
		    }
		}
#endif
		for(y=0;y<h;y++) {
		    a = buffer+3*width*y;
		    for(x=0;x<w;x++) {
#ifdef FAST_COL
			color = closest[*a/16][*(a+1)/16][*(a+2)/16];
#else
			/* Target YUV */
			ty = Y(*a,*(a+1),*(a+2));
			tu = U(ty,*(a+2));
			tv = V(ty,*a);

			error=MAXINT;
			for(i=0;i<16;i++) {
			    temp = ERROR(lumratio*ty,lumratio*col[i][3],
					 tu,col[i][4],
					 tv,col[i][5]);
			    if(temp<error) {
				color = i;
				error = temp;
				if(error<8)    /* we are very satisfied if the error is less than .. */
				    break;
			    }
			}
#endif
			pic[y][x] = color;

			if(dither && x<w-1) {
			    b = a+3;
			    temp = (*(a+0)-col[color][0])/dither/2+*b;
			    if(temp>=0 && temp<256)
				*b = temp;
			    b++;
			    temp = (*(a+1)-col[color][1])/dither/2+*b;
			    if(temp>=0 && temp<256)
				*b = temp;
			    b++;
			    temp = (*(a+2)-col[color][2])/dither/2+*b;
			    if(temp>=0 && temp<256)
				*b = temp;

			    if(y+1<h) {
				b = a+3*width;
				temp = (*a-col[color][0])/dither/2+*b;
				if(temp>=0 && temp<256)
				    *b = temp;
				b++;
				temp = (*(a+1)-col[color][1])/dither/2+*b;
				if(temp>=0 && temp<256)
				    *b = temp;
				b++;
				temp = (*(a+2)-col[color][2])/dither/2+*b;
				if(temp>=0 && temp<256)
				    *b = temp;
			    }
			}
			a += 3;
		    }
#ifdef FAST_COL
		    if(!(stat & MODE_QUIET) && (y&7)==7) {
		    	fprintf(stderr, ".");
		    	fflush(stderr);
		    }
#else
		    if(!(stat & MODE_QUIET)) {
			fprintf(stderr, "\rLine: %ld", (long)y);
			fflush(stderr);
		    }
#endif
		}
		if(!(stat & MODE_QUIET))
		    fprintf(stderr, "\n");
		free(buffer);

		if(ouf) {
		    error = Convert(stat, argv[ouf], h, w, c0, c1, c2, c3, lumratio);
		} else {
		    error = Convert(stat, NULL, h, w, c0, c1, c2, c3, lumratio);
		}
		exit(error);
	    } else {
		fprintf(stderr, "Memory allocation error (%d bytes)\n", 3*width*height);
		if(inf)
		    fclose(handle);
		exit(42);
	    }
	} else {
	    fprintf(stderr, "Don't know how to handle this type of ppm: %s\n", apu);
	    if(inf)
		fclose(handle);
	    exit(24);
	}
    } else {
	fprintf(stderr, "Could not access %s\n", argv[inf]);
	exit(10);
    }
}



int Convert(int stat, char *nimi, int height, int width,
	    int init0, int init1, int init2, int back, int lumratio) {
    int x, y, error, ind, pos, bits;
    int d[4], e, g[4], approx[256], appr=0;
    UBYTE c, colbits;
    UBYTE *BASE;
    UBYTE *colhist[16]; /* [16][512] */
    UBYTE *gfxmem;	/* [4096+1024] */
    FILE *handle;

#define ALLOC_SIZE    (8192+5120)
    const UBYTE bitmask[4]  = {0x3f, 0xcf, 0xf3, 0xfc};
    const UBYTE bitshift[4] = {0x40, 0x10, 0x04, 0x01};
    static int hist[16][160];


    if((BASE = (UBYTE *)calloc(ALLOC_SIZE, 1))) {
	/* initialize the pointers */
	for(x=0;x<16;x++)
	    colhist[x] = BASE+x*512;
	gfxmem = BASE+16*512;

	for(y=0;y<256;approx[y++]=0)
	    ;

	/*if(back==NOT_USED)
	    back = 0;*/

	for(x=0;x<16;x++) {
	    for(y=0;y<16;y++) {
		errs[x][y] = ERROR(lumratio*col[x][3],lumratio*col[y][3],
				   col[x][4],col[y][4],
				   col[x][5],col[y][5]);
	    }
	}
	for(y=0;y<height;y++) {
	    for(x=0;x<width;x++) {
		hist[pic[y][x]][y]++;
	    }
	}
	if(init0 == NOT_USED) {
	    int p;
/*
    1)	Fill the color memory with the color that is
	used in all 8 lines of the cell (or the maximum number)
    2)	Mark those locations that don't have the color with 16
 */
	    if(!(stat & MODE_QUIET))
		fprintf(stderr,"Selecting colors\n");

	    for(p=0;p<256/*512*/;p++) {
		int w, cf[16] = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0};

		y = /*((p&256)?8:0) +*/ ((p & 255)/25)*16;
		x = 4*((p & 255) % 25);

		if(x+3 >= width || y+15 >= height)
		    continue;

		for(w=0;w<16/*8*/;w++) {
		    int a0 = pic[y+w][x+0] & 15;
		    int a1 = pic[y+w][x+1] & 15;
		    int a2 = pic[y+w][x+2] & 15;
		    int a3 = pic[y+w][x+3] & 15;

		    cf[a0]++;
		    if(a1 != a0)
			cf[a1]++;
		    if(a2 != a0 && a2 != a1)
			cf[a2]++;
		    if(a3 != a0 && a3 != a1 && a3 != a2)
			cf[a3]++;
		}
		if(init1 != NOT_USED)
		    cf[init1] = 0;
		if(init2 != NOT_USED)
		    cf[init2] = 0;
		if(back != NOT_USED)
		    cf[back] = 0;

		w = 0;
		for(x=0;x<8;x++)	/* only 8 colors possible */
		    if(cf[x] > cf[w])
			w = x;
#if 0
fprintf(stderr, "%3d: ", p);
for(x=0;x<16;x++)	/* only 8 colors possible */
    fprintf(stderr, "%3d ", cf[x]);
fprintf(stderr,"%3d -> %x %s\n", p, w, colornames[w]);
#endif
		if(cf[w]) {
		    gfxmem[0x1000+p] = w;
		    gfxmem[0x1100+p] = w;/* */
		} else {
		    gfxmem[0x1000+p] = 16;
		    gfxmem[0x1100+p] = 16;
		}
	    }
	} else {
	    for(y=0;y<512;y++) {
		gfxmem[0x1000+y] = init0;
	    }
	}

	/* NOT!: 00 - back, 01 - character, 10 - border, 11 - aux */
	/* 00 - back, 01 - border, 10 - character, 11 - aux */
	/* First select border, background and auxiliary colors */
	for(y=0;y<height;y++) {
	    int c0, c1, c2;
	    int c[3];
	    long mintot = MAXINT;

#if 1
	    /* Remove color memory colors */
	    for(x=0;x<width;x++) {
		ind = x/4 + (y/16)*25 /*+ ((y&8)<<5)*/;

		if(pic[y][x] == gfxmem[0x1000+ind])
		    hist[pic[y][x]][y]--;
	    }
#endif
#if 0
printf("%3d: ", y);
for(x=0;x<16;x++)
   printf("%03d ", hist[x][y]);
#endif
	    if(init0 != NOT_USED)
		hist[init0][y] = -1;
	    if(init1 != NOT_USED)
		hist[init1][y] = -1;
	    if(init2 != NOT_USED)
		hist[init2][y] = -1;
	    if(back != NOT_USED)
		hist[back][y] = -1;

	    /*
		In fact we should do something like:
		1) Take the most used color (takes care of big
		   areas, where the color should not change)
		2) Select remaining 2 colors so that the
		   weighted error is the smallest possible
	     */

	    if((stat & MODE_OPTIMIZE)) {
		c0 = 1;
		c1 = 2;
		c2 = 3;

		for(c[0]=0;c[0]<8;c[0]++) {
		    for(c[1]=c[0]+1;c[1]<16;c[1]++) {
			for(c[2]=c[1]+1;c[2]<16;c[2]++) {
			    long total = 0;

			    for(x=0;x<16;x++) {
				long e = errs[x][c[0]];

				if(errs[x][c[1]] < e)
				    e = errs[x][c[1]];
				if(errs[x][c[2]] < e)
				    e = errs[x][c[2]];

				total += e * hist[x][y];
			    }
			    if(total < mintot) {
				mintot = total;
				c0 = c[0];
				c1 = c[1];
				c2 = c[2];
			    }
			}
		    }
		}
	    } else {
		c0 = back;
		if(c0==NOT_USED) {
		    c0 = 0;
		    for(x=1;x<16;x++) {
			if(hist[x][y] > hist[c0][y])
			    c0 = x;
		    }
		    hist[c0][y] = -1;
		}
		c1 = init1;
		if(c1==NOT_USED) {
		    c1 = 0;
		    for(x=1;x<16;x++) {
			if(hist[x][y] > hist[c1][y])
			    c1 = x;
		    }
		    hist[c1][y] = -1;
		}
		c2 = init2;
		if(c2==NOT_USED) {
		    c2 = 0;
		    for(x=1;x<16;x++) {
			if(hist[x][y] > hist[c2][y])
			    c2 = x;
		    }
		    hist[c2][y] = -1;
		}
	    }
	    /* Sort the colors - c0 smallest */
	    if(c2<c1) {
		int ctmp = c2;
		c2 = c1;
		c1 = ctmp;
	    }
	    if(c1<c0) {
		int ctmp = c1;
		c1 = c0;
		c0 = ctmp;
	    }
	    if(c2<c1) {
		int ctmp = c2;
		c2 = c1;
		c1 = ctmp;
	    }

#if 0
printf(" --> %d %d %d\n", c0, c1, c2);
#endif
	    /* c0 should be 3-bit */
	    if(c0 > 7) {
		printf("Can not represent c0: %d (line %d) (c1: %d, c2: %d)\n",
			c0, y, c1, c2);
	    }
	    gfxmem[0x1300+160-y] = (c1<<4) | 8 | c0;
	    gfxmem[0x1200+160-y] = (c2<<4);	/* Volume 0 */
	}

	if(init0 == NOT_USED) {
	    for(y=0;y<height;y++) {
		for(x=0;x<width;x++) {
		    int c = pic[y][x];
		    ind = x/4 + (y/16)*25 /*+ ((y&8)<<5)*/;
		    colhist[c][ind]++;
		}
	    }
	    for(y=0;y<256 /*512*/;y++) {
		if(gfxmem[0x1000+y] < 16 &&
		   colhist[gfxmem[0x1000+y]][y]==0) {
		    gfxmem[0x1000+y] = 16;
		    gfxmem[0x1100+y] = 16;
		}
	    }
	}

	if(!(stat & MODE_QUIET))
	    fprintf(stderr, "Converting picture\n");

	for(y=0;y<height;y++) {
	    for(x=0;x<width;x++) {
		ind = x/4 + (y/16)*25 /*+ ((y&8)<<5)*/;
		pos = y%16 + (x/4)*16 + (y/16)*16*25;
		bits = (x%4);				/* bit numbers */

		g[0] = (gfxmem[0x1300+160-y]>>4) & 15;	/* == background */
		g[1] = (gfxmem[0x1300+160-y] & 7);		/* Border */
		g[2] = gfxmem[0x1000+ind];		/* color memory */
		g[3] = (gfxmem[0x1200+160-y]>>4) & 15;	/* Aux */

		c = pic[y][x];
		if(c==g[0])    /* == background */
		    colbits = 0;
		else if(/*x && x != 100-1 &&*/ c==g[1])	/* Border */
		    colbits = 1;
		else if(c==g[2] && g[2]<8)	/* color memory */
		    colbits = 2;
		else if(c==g[3])	/* Aux */
		    colbits = 3;
		else if(/*x &&*/ g[2]>=8 && c<8) {
		    colbits = 2;
		    gfxmem[0x1000+ind] = c;
		    gfxmem[0x1100+ind] = c;
		    fprintf(stderr, "%3d <-- %d %d\n", ind, c, g[2]);
		    g[colbits] = c;
		} else {
		    /*
			We can't change the colors
			1) They may have been selected by the user
			2) They are already 'optimally' selected.
			So, we just have to make it with the colors
			we've got and minimize the error.
		    */

		    d[0] = errs[g[0]][c];
		    d[1] = errs[g[1]][c];
		    d[2] = (g[2]==15)?d[0]:errs[g[2]][c];
		    d[3] = errs[g[3]][c];

		    error = MAXINT;
		    colbits = 0;
		    for(e=0;e<4;e++) {
			if(/*(x || e!=1) &&*/ d[e]<error) {
			    colbits = e;
			    error = d[e];
			}
		    }
		    if(e==2 && g[2]>=8)
			fprintf(stderr, "!!!%3d <-- %d %d\n", ind, c, g[2]);
		    ++appr;
		    ++approx[c*16+g[colbits]];
		}
if(pos >= 4096)
{
fprintf(stderr, "7: %d\n", pos);
exit(20);
}
#if 0
		if(!x && colbits==1) {
		    fprintf(stderr, "****%d\n", colbits);
		    colbits = 2;
		}
#endif
		gfxmem[pos] = (gfxmem[pos] & bitmask[bits]) | (bitshift[bits]*colbits);
	    }
	}

	if(appr && (stat & MODE_VERBOSE) && !(stat & MODE_QUIET)) {
	    fprintf(stderr, "Approximations:\n");
	    for(e=0;e<256;e++) {
		if(approx[e])
		    fprintf(stderr, "%4d %s -> %s\n", approx[e],
			    colornames[e/16], colornames[e%16]);
	    }
	    fprintf(stderr, "%4d/16000\n", appr);
	}

	if (!nimi || (handle = fopen(nimi, "wb"))) {
	    if (!nimi) {
		handle = stdout;
	    } else {
		if(!(stat & MODE_QUIET))
		    fprintf(stderr, "Saving picture to %s\n", nimi);
	    }

	    for (x=0;x<512;x++) {
		gfxmem[0x1000+x] |= 8;
	    }

	    fwrite("\000\023", 2, 1, handle);
	    memcpy(gfxmem+0x0fa0, Overlay, sizeof(Overlay)); /* Albert99 */
	    memset(gfxmem+0x10fa, 0, 6); /* Colmem 1 */
	    memset(gfxmem+0x11fa, 0, 6); /* Colmem 2 */

	    fwrite(gfxmem+768, 5120-768, 1, handle);
	    fwrite(gfxmem, 768, 1, handle);

	    if (nimi) {
		fclose(handle);
	    }
	    free(BASE);
	} else {
	    fprintf(stderr, "Write failed!\n");
	    free(BASE);
	}
    } else {
	fprintf(stderr, "AllocMem failed! (%d bytes)\n", ALLOC_SIZE);
    }
    return 0;
}


