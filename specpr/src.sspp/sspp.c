/* sspp: selective source pre-processor

 This program looks for a string given at the beginning of a line,
	and if found deletes that string.

 The program is implimented by:

    sspp -string1 -string2 -string3 .... -stringn filename

or (added 2/5/90, R. Clark):
    sspp -r -string1 ... filename   # for ratfor files
    sspp -f -string1 ... filename   # for fortran files
    sspp -c -string1 ... filename   # for C files

The -r, -f, -c options means add the appropriate comment characters
for that language, that way you don't have to include them.  This
was a particilar problem with ratfor and makefiles: the # could not
be in the makefile at all or everything after it would be a comment
and it couldn't be escaped!  - R. Clark

 as an option, the string -- means use standard input
 the output is the standard output.

*/

#include <stdio.h>

#define LSIZE 2048 /* line size */

/* file pointers */

FILE *fopen(), *fpinput;
char input[LSIZE];

void clearinput()
{
int i;

for (i=0;i<LSIZE;i++) {
	input[i]='\0';
	}
}

main(argc, argv)
int argc;
char *argv[];
{
	char chstr[LSIZE];
	char chstr2[LSIZE];
	char *ptr;
	
	int i, ier, itst, istr, j;
	int mode, istart;

	if (argc < 2) {
		fprintf (stderr,"insufficient arguments\n");
		exit (1);
	}


	itst = 0;
	istr = 0;
	i = 1;    /* start looking at argument 1 */
	mode = 0;
	if (strncmp(argv[1],"-r",2) == 0) {          /* Ratfor mode */
		mode = 1;
		istr++;
		i++;
	}
	else if (strncmp(argv[1],"-c",2) == 0) {     /* C mode */
		mode = 2;
		istr++;
		i++;
	}
	else if (strncmp(argv[1],"-f",2) == 0) {     /* Fortran mode */
		mode = 3;
		istr++;
		i++;
	}
	while (i <= argc-1 ) {
		if ( strncmp(argv[i],"--",2) == 0) {
			/* use standard input */
			fpinput = stdin;
			i++;
			goto next_step;
		}
		else if (strncmp(argv[i],"-\0",2) == 0) {
			/* string is null: ERROR */
			fprintf (stderr,"sspp ERROR: argument has no string\n");
			exit (1);
		}
		else if (strncmp(argv[i],"-",1) == 0) {
			/* comparison string, look at next arg */
			istr++;
			i++;
		}
		else if (strncmp(argv[i],"-",1) != 0) {
			/* file name, open file */
			fpinput = fopen (*(argv+i), "r");

			if (fpinput == NULL) {
				fprintf (stderr,"cannot open %s\n",*(argv+i));
				exit (1);
			}
			fseek (fpinput, 0L, 0);
			i++;
		}
	}

next_step:

/* debug:
	printf ("istr = %d\n", istr);
 */

	if ( istr != argc-2 ) {     /* argument count is wrong */
		fprintf (stderr,"argument count is wrong\ndid a ");
		fprintf (stderr,"comparison string come after the filename?\n");
		exit (1);
	}

/* now read data */

	istart = 1;
	if (mode > 0) istart = 2;
	
	clearinput();
	while (fgets (input, LSIZE, fpinput) != NULL) {

		for (i=istart ; i <= istr; i++) {
			ptr = (*(argv+i))+1;

			itst = strlen(argv[i]);
			if (mode == 0) {
				sprintf(chstr, "%s", ptr);
			} else if (mode == 1) {
				sprintf(chstr, "#%s", ptr);
			} else if (mode == 2) {
				sprintf(chstr, "/*%s", ptr);
				sprintf(chstr2, "%s*/", ptr);
			} else if (mode == 3) {
				sprintf(chstr, "c%s", ptr);
				sprintf(chstr2, "C%s", ptr);
			}

/* debug:
 *			printf ("debug: argv[%d] = %s\n", i, chstr);
 */

			itst = strlen(chstr);    /* first test */

			if ((strncmp(input,chstr,itst) == 0) && (itst > 0)) {
				/* string found, so remove string */
				/* from input line */
				ptr = input + strlen(chstr);
				goto do_output;
			}

			if (mode > 1) {     /* C and Fortran modes */
				itst = strlen(chstr2);    /* second test */

				if ((strncmp(input,chstr2,itst) == 0) && (itst > 0)) {
					/* string found, so remove string from input line */
					ptr = input + strlen(chstr2);
					goto do_output;
				}
			}
		}

		/* nothing matched, so copy input to output */

		ptr = input;

		/* now output the input line (that may be modified) */

do_output:
		printf ("%s", ptr);
		clearinput();
	}
	/* end of file reached, so quit */
	exit (0);
}

