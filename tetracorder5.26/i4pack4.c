/*__________________________________________________________________
 |  TITLE:                   PIXEL PACKER (Version 4)              |
 |  PROGRAMMER:              Barry J. Middlebrook                  |
 |-----------------------------------------------------------------|
 |  DESCRIPTION:                                                   |
 |              This subroutine is for packing pixels into 32 bit  |
 |              numbers.  It handles 8, 16 and 32 bit numbers.     |
 |                                                                 |
 |-----------------------------------------------------------------|
 |  VARIABLES:                                                     |
 |            ibuff	- incoming array containing pixels to be   |
 |                      packed (32 bit -> 8 or 16 bit)             |
 |            npix	- number of pixels to pack                 |
 |            numsiz	- number of bits to pack to (32->numsiz)   |
 |            pak	- register variable the pixels are packed  |
 |                      in                                         |
 |            maxoff	- maximum offset from array starting       |
 |                      address                                    |
 |            inc	- increment to move through input array    |
 |            ercode	- return error code (1 - error, 0 - ok)    |
 |                                                                 |
 |_________________________________________________________________|
*/

union iobuff
{
	unsigned char		chbuff[8192];
	unsigned short int	i2buff[4096];
	unsigned long int	i4buff[2048];
};

/*HPUX
long int i4pack4 (ibuff,npix,numsiz)
HPUX*/

/*NONHPUX
long int i4pack4_ (ibuff,npix,numsiz)
NONHPUX*/

/*IA64HPUX
long int i4pack4_ (ibuff,npix,numsiz)
IA64HPUX*/

/*LINUX
long int i4pack4_ (ibuff,npix,numsiz)
LINUX*/

union iobuff	*ibuff;
long int	*npix,*numsiz;

{
	register long int	i,pak,j;
	long int		maxoff,inc,ercode;

/* Initialize variables */
	maxoff = (*npix << 2);  /* Equivalent to multiplying by 4 */
	inc = 128/(*numsiz);

/* Select between 8 (case 16) and 16 (case 8) bit numbers */
	switch (inc)
	{
	   case 16:	/* Pack 32-bit pixels to 8-bit numbers */	
			for (i = j = 0; i < maxoff; i += inc, j++) 
			{
				pak = ibuff -> chbuff[i+3];
				pak = (pak << 8) | ibuff -> chbuff[i+7];
				pak = (pak << 8) | ibuff -> chbuff[i+11];
				pak = (pak << 8) | ibuff -> chbuff[i+15];

				ibuff -> i4buff[j] = pak;
			}
			return(0);

	   case 8:	/* Pack 32-bit pixels to 16-bit numbers */	
			for (i = j = 0; i < maxoff; i += inc, j++) 
			{
				pak = ibuff -> chbuff[i+2];
				pak = (pak << 8) | ibuff -> chbuff[i+3];
				pak = (pak << 8) | ibuff -> chbuff[i+6];
				pak = (pak << 8) | ibuff -> chbuff[i+7];

				ibuff -> i4buff[j] = pak;
			}
			return(0);

	   default:	return (1);

	}
}
