/*   convert VAX floating point to TI, in place, by arrays. d[] is
     the array and n is the number of conversions to be performed.
     
     TI format is: sfffffffffffffffffffffffeeeeeeee, where sff...
     is the 2's complement fraction, and ee... is the excess 128
     power of 2 exponent.

     VAX (or PDP11) format is: seeeeeeeefffffffffffffffffffffff,
     where s is the sign of the number, ee... is the excess 128
     power of 2 exponent, and ff... is the positive fraction where
     there is an implied 1 before the first f (a hidden bit which
     would always be 1 in a normaized number).
*/

fpv2t (d,n)

int	d[];
short	*n;
{
	register int	i,e,f,s;
	short j;

	wswap(d,n);
	for (i=0;i < *n; i++) {
		f = d[i];
		if (f == 0) continue;
		e = (f & 0x7f800000) >> 23;				/* save exponent */
		if (f < 0)  s=0x80000000;
		else s=0;
		f = ((f | 0x800000) >> 1)  & 0x7fffff;	/* restore hidden bit */
		if (s != 0) {
			f = -f & 0x7fffff;					/* mask to 23 bits  and sign */
			while (f & 0x400000 == 0) {
				f << 1;							/* normalize */ 
				e--;
			} 
		}	
		d[i] = s | e | (f <<8); 
	}
	wswap(d,n);
	j = 2 * *n; 
	bswap(d,&j);
}


fpv2t_ (d,n)

int	d[];
short	*n;
{
	/* was:
            fpv2t (d,n);
             return;
	*/
	return(fpv2t (d,n));
}
