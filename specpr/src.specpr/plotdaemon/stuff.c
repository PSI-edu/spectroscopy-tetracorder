/*	%W% %G% %U% */

static char	Sccsid[] = "%W% %G% %U%";


stuff(src, count, dest, offset)
char	*src;
int		count;
char	*dest;
int		offset;
{
	long	bits = 0;

	if (count<0) count = -count;

	while (count--) {
		bits = *src << 8;
		bits >>= count;
		*dest++ |= (bits & 0177);
		*dest |= (bits >> 8) & 0177;
	}
	return(0);
}
