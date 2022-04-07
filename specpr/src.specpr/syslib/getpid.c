/*	fortran interface to getpid system call */

/*  machine dependency added 3/27/85. Roger Clark */

igtpid()

{
	return(getpid());
}

/* below is identical to above except _ added */

igtpid_()

{
	return(getpid());
}
