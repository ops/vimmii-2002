#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static unsigned char mem[256*16], out[256*16], map[256];


int main(int argc, char *argv[]) {
    FILE *fp;
    int len = 0, c, outptr = 0;

    if (argc<2) {
	return 20;
    }
    if ((fp = fopen(argv[1], "rb"))) {
/*	len = fread(mem, 2, 1, fp);*/
	len = fread(mem, 1, 256*16, fp);
	fclose(fp);
    }
    for (c=0; c<len/32; c++) {
	int a = 0, j;

	for (a=0; a<outptr; ) {
	    for (j=0; j<32; j++) {
		if(out[a + j] != mem[c*32+j])
		    break;
	    }
	    if (j>=32) {
		fprintf(stderr, "%d matches %d\n", c, a/32);
		map[c] = a/32;
		break;	/* found a match */
	    }
	    a += 32;
	}
	if (a >= outptr) {
	    fprintf(stderr, "%d becomes new char %d\n", c, outptr/32);
	    map[c] = outptr/32;
	    for (j=0; j<32; j++) {
		out[outptr++] = mem[c*32+j];
	    }
	}
    }
    if (argc > 2) {
	if ((fp = fopen(argv[2], "wb"))) {
	    fwrite(out, 1, outptr, fp);
	    fclose(fp);
	}
    }
    if (argc > 3) {
	if ((fp = fopen(argv[3], "wb"))) {
	    fwrite(map, 1, len/32, fp);
	    fclose(fp);
	}
    }
    return 0;
}


