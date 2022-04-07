#include "psdaemon.h"
#include <signal.h>
#include <sgtty.h>

/*
 *	initport.c
 *	03/13/2006 Copy from initport.c in hpgdeamon
 *		   Expect to output to stdout so may not be needed.
 *      03/21/2006 psplotdaemon expects to always output to stdout.
 *                 Thus, any file open below is not needed.
 */


initport()
{
/*	plotter = open(".postscript.plot", 2); */
/*       
	if (plotter<0) {
		logerr("initport: Can't open plot file .hpglplot\n");
		exit(1);
	}
*/
}
