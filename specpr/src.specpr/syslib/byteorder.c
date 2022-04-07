/*******************************************************************/
/*  byteorder.c                                                    */
/*                                                                 */
/*  02/27/2004 Randall Dailey                                      */
/*      Created from src.specpr/io/bytorder.r                      */
/*      Knowledge of specpr record structures is explicit          */
/*        refer src.specpr/commong/label1.h (fortran)              */
/*              src.fstospecpr/io_specpr.h                         */
/*                                                                 */
/*  Old Comments from src.specpr/io/bytorder.r                     */
/*  ------------------------------------------                     */
/*  02/18/2001 Roger N. Clark                                      */
/*                                                                 */
/*     subroutine bytorder (a,iflag)                               */
/*     reverse byteorder in a 4-byte word: 1234 -> 4321            */
/*                                                                 */
/*  Note: while variable "a" is an integer, the ordering will      */
/*  work on floats too.                                            */
/*                                                                 */
/*  iflag: when to swap bytes on a(1): the bit flags               */
/*         = 0 do not flip bytes on a(1)                           */
/*         = 1 flip bytes on a(1) before others (for read)         */
/*         = 2 flip bytes on a(1) after  others (for write)        */
/*                                                                 */
/*******************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#define check_bit(i,n) ((i & (1 << n)) != 0)
static char bytetemp;
typedef char *byteptr;
#define swap1byte(c1, c2) (bytetemp = (c1) , (c1) = (c2) , (c2) = bytetemp)
#define swap4byte(s)     (swap1byte(((byteptr)(s))[0], ((byteptr)(s))[3]), \
                         swap1byte(((byteptr)(s))[1], ((byteptr)(s))[2]),(s))

void byteorder(a,iflag)
int *a,iflag;
{
	int itmp,i;

	itmp=0;
	i=(int)a[0];

	if (iflag == 1) swap4byte(&((int *)a)[0]);  /* swap byte order on bitflags */

	if (check_bit(i,0) == 0 && check_bit(i,1) == 1) {
		/* 1st text data record */
		swap4byte(&((int *)a)[13]);  /* swap byte order on text pointer */
		swap4byte(&((int *)a)[14]);  /* swap byte order on text size    */

	} else if (check_bit(i,0) == 1 && check_bit(i,1) == 1) {
		/* Continuation text data record */
			/* do something in the block but really nothing to do. */
		itmp =0;
	} else if (check_bit(i,0) == 0 && check_bit(i,1) == 0) {
		/* 1st data record */
		for (i = 13; i <= 28; i++) {
			swap4byte(&((int *)a)[i]);
		}
		for (i = 118; i <= 383; i++) {
			swap4byte(&((int *)a)[i]);
		}
	} else if (check_bit(i,0) == 1 && check_bit(i,1) == 0) {
		/* Continuation data record */
		for (i = 1; i <= 383; i++) {
			swap4byte(&((int *)a)[i]);
		}
        } 

	if (iflag == 2) swap4byte(&((int *)a)[13]);  /* swap byte order on bitflags */

	return(0);
}
