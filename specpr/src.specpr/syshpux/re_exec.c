re_exec(re,stringa)
char *re, stringa[];
{
	int re_exec();
	int i;
	char *rx;
	char *regex();

	rx = regex(re, stringa);

	if (rx) {
		return(1);
	} else {
		return(0);
	}
}
