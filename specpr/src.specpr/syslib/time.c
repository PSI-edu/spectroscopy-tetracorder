long time();

/*  machine dependency added 3/27/85. Roger Clark */
/*  change name so no conflict on HP-UX */

long ftime()

{
	return(time(0));
}


long ftime_()

{
	return(time(0));
}
