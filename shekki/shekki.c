#include <stdio.h>
#include <stdlib.h>


int main(int argc, char *argv[])
{
    int cx1, cy1 = 0;
    int cx2, cy2 = 0;
    int d1 = 8, d2 = 3;
    int x, y;
    int mask = 0;
    char str[20];
    char map[4][4] = 
		{{' ', '-', '|', '+'},
		 {'-', ' ', '+', '|'},
		 {'|', '+', ' ', '-'},
		 {'+', '|', '-', ' '}};

    int COLUMNS = 27;

    if (argc > 1) {
	COLUMNS = strtol(argv[1], NULL, 0);
    }
    printf("COLUMNS %d\n", COLUMNS);

   for(d1=1;d1<128;d1++) {
	printf("$%02x,", ((COLUMNS*4)%d1)*2 | ((COLUMNS*4/d1)&1));
   }
   printf("\n");

   d1 = 8;

    cx1 = 2;
    cx2 = 3;
    for(x = 0; x<20; x++) {
	cx1--;
	if(cx1 < 0) {
	    mask ^= 1;
	    cx1 += d1;
	}
	cx2--;
	if(cx2 < 0) {
	    mask ^= 2;
	    cx2 += d2;
	}
	str[x] = mask;
    }
/*

*/

    mask = 0;
    for(y = 0; y<20; y++) {
	cy1++;
	cy2++;
	if(cy1 > d1) {
	    mask ^= 1;
	    cy1 -= d1;
	}
	if(cy2 > d2) {
	    mask ^= 2;
	    cy2 -= d2;
	}
	for(x = 0; x<20; x++) {
	   printf("%c%c",
		map[str[x]][mask],
		map[str[x]][mask]
		);
	}
	printf("\n");
    }

    return 0;
}
