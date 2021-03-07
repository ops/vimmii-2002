/************************************************************************\
*                                                                        *
\************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Y(r,g,b) ((30*r+59*g+11*b)/100)
#define U(y,b)   (493*(b-y)/4096+128)
#define V(y,r)   (877*(r-y)/4096+128)
#define ERROR(y,y1,u,u1,v,v1) (abs(y-y1)+abs(u-u1)+abs(v-v1))


char *vers="\0$VER: ppmtomcol(vic-20) 1.0\n";

unsigned char font[4096];

unsigned char col[16][3]= {
/*    R   G   B    */
    0x00,0x00,0x00,
    0xf0,0xf0,0xf0,
    0xd0,0x00,0x20,	/* red is not so pure */
    0x00,0xf0,0xd0,
    0xf0,0x00,0xd0,
    0x00,0xf0,0x00,
    0x00,0x00,0xd0,
    0xe0,0xe0,0x00,

    0xf0,0x60,0x00,
    0xf0,0xb0,0x80,
    0xf0,0xc0,0xc0,
    0xc0,0xf0,0xf0,
    0xf0,0xc0,0xf0,
    0xc0,0xf0,0xc0,
    0xc0,0xc0,0xf0,
    0xf0,0xf0,0xc0
};


int main(int argc,char *argv[]) {
    static char map[] = {0,1,3,2,3,2,0,1,2,2,1,1,1,1,1,1};
		/* 00 - back, 01 - border, 10 - character, 11 - aux */

    unsigned char *a, *buffer=NULL, apu[100];
    int i=0, x, y, error, width, height, w, lastbyte = 0, n;
    FILE *handle;
    static unsigned char closest[16][16][16];
    char *inFile = NULL;
    char *outFile = NULL;
    int shift = 0, ty, tu, tv;

    for (n=1; n<argc; n++) {
	if (argv[n][0] == '-') {
	    x = 1;
	    while (argv[n][x]) {
		switch (argv[n][x]) {
		case 'b':
		    x++;
		    ty = 0;
		    while (argv[n][x] && ty < 16) {
			map[ty++] = (argv[n][x++] - '0') & 3;
		    }
		    x--;
		    break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		    shift = argv[n][x] - '0';
		    break;
		default:
		    break;
		}
		x++;
	    }
	} else if (!inFile) {
	    inFile = argv[n];
	} else if (!outFile) {
	    outFile = argv[n];
	} else {
	    fprintf(stderr, "Too many filenames: %s discarded\n",
		    argv[n]);
	}
    }

    if (!inFile || !outFile) {
	fprintf(stderr,"%s", vers+7);
	fprintf(stderr,"Usage: %s -<shift> ppmfile fontfile\n", argv[0]);
	return 5;
    }

    for (x=0; x<255; x+=16) {
	for (y=0; y<255; y+=16) {
	    int z;

	    for (z=0; z<255; z+=16) {
		ty = Y(x,y,z);
		tu = U(ty,z);
		tv = V(ty,x);

		error = 0x12345678;
		for (i=0; i<16; i++) {
		    int temp =	(x-col[i][0])*(x-col[i][0]) +
				(y-col[i][1])*(y-col[i][1]) +
				(z-col[i][2])*(z-col[i][2]);
		    if (temp < error) {
			closest[x/16][y/16][z/16] = i;
			error = temp;
		    }
		}
	    }
	}
    }

    fprintf(stderr, "Shift: %d\n", shift);
    if (!(handle = fopen(inFile, "rb"))) {
	fprintf(stderr,"Could not access %s\n", inFile);
	return 10;
    }

    fgets(apu, 100, handle);
    if (strncmp(apu, "P6", 2)) {
	fprintf(stderr,"Don't know how to handle this type of ppm ..(%s)\n",apu);
	fclose(handle);
	return 24;
    }
    while (fgets(apu, 100, handle) && apu[0] == '#')
	;
    w = width = atoi(apu);
    i = 0;
    while (apu[i]!=' ' && apu[i]!='\t' && apu[i])
	i++;
    height = atoi(apu+i);
    fprintf(stderr,"Input file seems to be %d x %d\n", width, height);
    fgets(apu, 20, handle);	/* just get the maxval..*/

    if (height==8) {
	fprintf(stderr,"Assuming 8-line characters.\n");
    } else if (height==16) {
	fprintf(stderr,"Assuming 16-line characters.\n");
    } else {
	fprintf(stderr,"Assuming 16-line characters, %d chars.\n", (height-1)/16+1);
	height = ((height-1)/16+1)*16;
    }

    if ((buffer = (char *)calloc(3*width*height, 1))) {
	if (fread(buffer, 1, 3*width*height, handle) != (3*width*height))
	    fprintf(stderr,"Short file! Result may be quite original.. \n");
	fclose(handle);

	if (width * height > 4*4096) {
	    w = 4*4096/height;
	    fprintf(stderr,"Only 4096 bytes will be converted (width=%d).\n", w);
	}
	for (y=0; y<height; y++) {
	    a = buffer+3*width*y;
	    for (x=0; x+shift<w && x+shift<width; x++) {
		int byte = y + height*((x + shift)/4);
		int bit = 6 - 2*((x+shift)&3);

		if (byte < 4096) {
		    int tmp = closest[*a/16][*(a+1)/16][*(a+2)/16];
/*
	00 - back, 01 - border, 10 - character, 11 - aux

	00	black
	01	white
	 11	red
	10	cyan
	 11	purple
	10	green
	00	blue
	01	yellow
	10	orange
	10	lt orange
	01	pink
	01	lt cyan
	01	lt purple
	01	lt green
	01	lt blue
	01	lt yellow
 */
/*printf("%02x %02x %02x %d %d\n", *a, *(a+1), *(a+2), tmp, map[tmp]);*/

		    font[byte] |= (map[tmp]<<bit);
		}
		if(byte > lastbyte)
		    lastbyte = byte;
		a += 3;
	    }
	}
	free(buffer);

	if ((handle = fopen(outFile, "wb"))) {
	    fwrite(font, lastbyte+1, 1, handle);
	    fclose(handle);
	    error = 0;
	} else {
	    fprintf(stderr,"Could not open %s\n", outFile);
	    error=10;
	}
	exit(error);
    } else {
	fprintf(stderr,"Memory allocation error (%d bytes)\n",3*width*height);
	fclose(handle);
	exit(42);
    }
    return 0;
}


