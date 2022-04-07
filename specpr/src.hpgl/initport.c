#include "hpdaemon.h"
#include <signal.h>
#include <sgtty.h>

/*
 *	@(#)initport.c	2.3 5/14/85 09:46:43
 */


static char Sccsid[]="@(#)initport.c	2.3 5/14/85 09:46:43";

initport()
{
	plotter = open(".hpglplot", 2);
	if (plotter<0) {
		logerr("initport: Can't open plot file .hpglplot\n");
		exit(1);
	}
}
