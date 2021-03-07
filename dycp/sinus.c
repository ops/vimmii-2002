#include <stdio.h>
#include <math.h>


int main()
{
    int i;

    for(i=0;i<256;i++)
    {
	if(!(i&15))
	    fprintf(stdout, "\n\tdc.b ");
	fprintf(stdout, "$%02x,",
		87-(int)(fabs(86.5*sin((double)i*PI/256.0))));
    }
    fprintf(stdout, "\n");
    return 0;
}
