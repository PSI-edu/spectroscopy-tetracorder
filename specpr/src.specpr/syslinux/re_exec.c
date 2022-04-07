re_exec(re,stringa)
char *re, stringa[];
{
	int re_exec();
	int i;
	char *rx;
	int regexec();

	rx = regexec(re, stringa);

	if (rx > 0) {
		return(1);
	} else {
		return(0);
	}
}
