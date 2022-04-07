wswap(d,n)
short	d[],*n;
{					/* swap the halves of a long */
	short s,i;

	for (i=0; i<2**n; i+=2) {
		s = d[i];
		d[i] = d[i+1];
		d[i+1] = s;
	}
}


bswap(d,n)

char d[];
short *n;
{					/* swap the bytes of a short */
	int i;
	char s;

	for (i=0; i<2**n; i+=2) {
		s = d[i];
		d[i] = d[i+1];
		d[i+1] = s;
	}
}


bswap_(d,n)

char d[];
short *n;
{
	/* was:
	bswap(d,n);
	return;
	*/
	return(bswap(d,n));
}
