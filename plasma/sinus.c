#include <stdio.h>
#include <math.h>


int main()
{
    int i;

    for(i=0;i<256;i++)
    {
	if(!(i&15))
	    fprintf(stdout, "\n\tdc.b ");
	fprintf(stdout, "$%02x,", (int)(4.0+4.0*sin((double)i*PI/128.0)) & 0xff);
    }
    fprintf(stdout, "\n");
    return 0;
}
