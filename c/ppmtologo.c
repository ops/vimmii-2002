/************************************************************************\
*                                                                        *
\************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char *vers="\0$VER: ppmtofont(vic-20) 1.0\n";

unsigned char font[4096];


int main(int argc,char *argv[])
{
    char *a, *buffer=NULL, apu[100];
    int i=0, x, y, error, temp, width, height, w, lastbyte = 0;
    FILE *handle;

    if(argc==4 && argv[1][0]=='-')
    {
	char *inFile = argv[2];
	char *outFile = argv[3];
	int shift = atoi(argv[1]+1);

	fprintf(stderr, "Shift: %d\n", shift);
	if((handle = fopen(inFile, "rb")))
	{
	    fgets(apu, 20, handle);
	    if(!strncmp(apu, "P6", 2))
	    {
		fgets(apu, 20, handle);
		w = width = atoi(apu);
		i=0;
		while(apu[i]!=' ' && apu[i]!='\t' && apu[i])
		    i++;
		height = atoi(apu+i);
		fprintf(stderr,"Input file seems to be %d x %d\n", width, height);
		fgets(apu, 20, handle);	/* just get the maxval..*/

		if(height>32)
		{
		    fprintf(stderr,"Only 32 lines will be converted.\n");
		    height = 32;
		}
		if(height==8)
		{
		    fprintf(stderr,"Assuming 8-line characters.\n");
		}
		else if(height==16)
		{
		    fprintf(stderr,"Assuming 16-line characters.\n");
		}
		else
		{
		    fprintf(stderr,"Assuming 16-line characters, %d chars.\n", (height-1)/16+1);
		    height = ((height-1)/16+1)*16;
		}

		if((buffer = (char *)calloc(3*width*height, 1)))
		{
		    if(fread(buffer, 1, 3*width*height, handle) != (3*width*height))
			fprintf(stderr,"Short file! Result may be quite original.. \n");
		    fclose(handle);

		    if(width * height > 8*4096)
		    {
			w = 8*4096/height;
			fprintf(stderr,"Only 4096 bytes will be converted (width=%d).\n", w);
		    }
		    for(y=0;y<height;y++)
		    {
			a = buffer+3*width*y;
			for(x=0;x+shift<w && x+shift<width;x++)
			{
			    int byte = y + height*((x + shift)/8);
			    int bit = 7 - ((x+shift)&7);

			    temp = *a + *(a+1) + *(a+2);
			    if(temp && byte < 4096)
			    {
				font[byte] |= (1<<bit);
			    }
			    if(byte > lastbyte)
				lastbyte = byte;
			    a += 3;
			}
		    }
		    free(buffer);

		    if((handle = fopen(outFile, "wb")))
		    {
			fwrite(font, lastbyte+1, 1, handle);
			fclose(handle);
			error=0;
		    }
		    else
		    {
			fprintf(stderr,"Could not open %s\n", outFile);
			error=10;
		    }
		    exit(error);
		}
		else
		{
		    fprintf(stderr,"Memory allocation error (%d bytes)\n",3*width*height);
		    fclose(handle);
		    exit(42);
		}
	    }
	    else
	    {
		fprintf(stderr,"Don't know how to handle this type of ppm ..(%s)\n",apu);
		fclose(handle);
		return 24;
	    }
	}
	else
	{
	    fprintf(stderr,"Could not access %s\n", inFile);
	    return 10;
	}
    }
    fprintf(stderr,"%s\n", vers+7);
    fprintf(stderr,"Usage: %s -<shift> ppmfile fontfile\n", argv[0]);
    return 5;
}


