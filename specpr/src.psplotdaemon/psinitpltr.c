#include	"psdaemon.h"

/*
 *	psinitpltr.c
 *	03/13/2006 Copied framework from initpltr.c under hpgldaemon source dir
 */


static	char line[256];


initplotter()
{
	int		error;

/* Output initializing PostScript code */
	fprintf (stdout, "%%!PS-Adobe-2.0\n");
	fprintf (stdout, "gsave\n");
	fprintf (stdout, ".30 .37 scale\n");
	fprintf (stdout, "100 60 translate\n");
	fprintf (stdout, "[] 0 setdash\n");
	fprintf (stdout, "/fsize 24 def\n");
	fprintf (stdout, "3 setlinewidth\n");
	fprintf (stdout, "[] 0 setdash\n");

}
