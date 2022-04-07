/* Roger N. Clark 2/18/2001 */

swab4order(x)
unsigned long *x;
{
	unsigned char *c = (unsigned char *)x;
	int y;

	y = c[3];            /* swap high and low bytes */
		c[3] = c[0];
		c[0] = y;

	y = c[2];           /* swap middle two bytes */
		c[2] = c[1];
		c[1] = y;
}

/* the following is the same as above, only with _  (for machine dependency) */

swab4order_(x)
unsigned long *x;
{
	unsigned char *c = (unsigned char *)x;
	int y;

	y = c[3];            /* swap high and low bytes */
		c[3] = c[0];
		c[0] = y;

	y = c[2];           /* swap middle two bytes */
		c[2] = c[1];
		c[1] = y;
}

