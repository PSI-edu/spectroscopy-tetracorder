/*   convert TI floating point to VAX, in place, by arrays. d[] is
     the array and n is the number of conversions to be performed.
     
     TI format is: sfffffffffffffffffffffffeeeeeeee, where sff...
     is the 2's complement fraction, and ee... is the excess 128
     power of 2 exponent.

     VAX (or PDP11) format is: seeeeeeeefffffffffffffffffffffff,
     where s is the sign of the number, ee... is the excess 128
     power of 2 exponent, and ff... is the positive fraction where
     there is an implied 1 before the first f (a hidden bit which
     would always be 1 in a normalized number).
*/

/* machine dependence added 3/26/85. Roger Clark */

fpt2v(d,n)

long	d[];
short	*n;
{
	register int	i,e,f,s;
	short	j;

	
	j = *n * 2;
	bswap(d,&j);

	wswap(d,n);
	for (i=0;i<*n;i++) {
		f = d[i];
		if (f == 0) continue;
		e = f & 0xff;				/* save exponent */
		f = f >> 8;
		if (f < 0) {
			f= -f; 
			s=0x80000000;
		}
		else s=0;
		while (f & 0x400000 == 0) {
			e--; 
			f<<1;     				/* normalize */
		}
		f = (f & 0x3fffff) <<1;		/* mask hidden bit & shift left */
		d[i] = s | (e << 23) | f; 
	}
	wswap(d,n);
}


fpt2v_(d,n)

long	d[];
short	*n;
{
	fpt2v(d,n);
}
