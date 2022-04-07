/* compare two strings starting at 0 up to n-1 */

strcmpn (s, t, n)

char s[], t[];
int n;

{
	int i, j;

	j = n;
	i = 0;
	while (i < n) {
		if (s[i] == t[i]) {
			j--;          /* decrement j; if strings are
					equal, then j will be 0    */
		}
		i++;
	}
	return (j);
}
