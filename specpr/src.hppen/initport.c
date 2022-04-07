#include "hpdaemon.h"
#include <signal.h>
#include "hpgtty.h"

/*
 *	@(#)initport.c	2.3 5/14/85 09:46:43
 */


static char Sccsid[]="@(#)initport.c	2.3 5/14/85 09:46:43";

void alrm()
{
	logerr("plotter didn't respond within 120 sec.\n");
	exit(OFFLINE);
}

initport()
{
	static 	struct sgttyb	buf;
	int		error;
	char	line[128];


#ifdef PLOTTER
	plotter = open("/dev/plotter", 2);
#endif
#ifdef PLOTTER2
	plotter = open("/dev/plotter2", 2);
#endif
	if (plotter<0) {
		logerr("initport: Can't open plotter device\n");
		exit(1);
	}


#ifdef RS232

	ioctl(plotter,TIOCGETP,&buf);
	buf.sg_ispeed = buf.sg_ospeed = B2400;
	buf.sg_flags = EVENP|ODDP|CRMOD;
	ioctl(plotter,TIOCSETP,&buf);

	PLOTON; HSHK2; XHSHK;
	signal(SIGALRM,alrm);
	WAIT(line);

#endif 
	GERR(error, line);
	if (error < 0) {
		logerr("initport: Can't read plotter error status\n");
		exit(BAD_STATUS_READ);
	}
	line[error] = '\0';
	sscanf(line,"%d",&error);
	logerr("initport: error status=%s\n",line);
	if (error==8) {
		logerr("initport: Out of paper");
		exit(OUT_OF_PAPER);
	}
}
