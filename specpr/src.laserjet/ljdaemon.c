#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <pwd.h>
#include <dirent.h>
/*
 *      ljdaemon.c -- main control routine for LaserJet III plot spooler.
 */

char dpd[]      = "/users/spool/gplot";        /* spooling directory */
char dfname[MAXNAMLEN+1];
char bfname[MAXNAMLEN+1];
char lock[]     = "/users/spool/gplot/lockfile";
char plotter[]  = "/dev/lp_laserjet";
char acctfile[] = "/users/adm/ljplot";
char logfile[]  = "/users/log/ljdaemon";
int cmdfound;
extern errno;
int cnice;

#include "logerr.c"

#define LJPLOT "/users/local/ljplot"
#define PLOTFILE "/users/tmp/plotfile"
#define LJDUMP "/usr/bin/lp"

DIR *df;
struct dirent *dfb;
FILE *f;

struct stat statbuf;
struct tbuffer {
	long proc_user_time;
	long proc_system_time;
	long child_user_time;
	long child_system_time;
}
tsb,teb;
long ts,te;
long time();
int debug = 0;

main(ac,av)
char **av;
{
	int nent;
	unsigned dsize;
	int i,ii,j,jj,status,pid;
	char vector[32],text[32],*bptr,*buf;
	char inline[128],*tptr;

	if (ac > 1) debug=1;

	dem_setup(); 

	cmdfound = 1;

	while(cmdfound){

		cmdfound = 0;
		if (access(lock,0) < 0) {
			logerr("Lock has disappeared.\n");
			dem_dis();
			exit(1);
		}

		df = opendir(dpd);

		if (df == NULL) {
			logerr("Can't open %s\n",dpd);
			dem_dis();
			exit(1);
		}

		if (debug==1) logerr("opening %s\n",dpd);
		
		while((dfb = readdir(df)) != NULL) {

			if (access(lock,0) < 0) {
				logerr("Lock has disappeared.\n");
				dem_dis();
				exit(1);
			}

			if (dfb->d_name[0] != 'a' && dfb->d_name[1] != 'a')
				continue;

			cmdfound++;

			if (debug) logerr("cmdfile = %s\n",dfb->d_name);

			if ((f = fopen(dfb->d_name,"r"))   == NULL){
				strcpy(dfname,dfb->d_name);
				strcpy(bfname,dfname);
				bfname[0] = 'x';
				link(dfname,bfname);
				unlink(dfname);
				logerr("Can't open %s, err=%d.\n",dfname,errno);
				continue;
			}

			if (debug==1) logerr("opened %s, f = %d\n",
							dfb->d_name,fileno(f));

			time(&ts);
			times(&tsb);
			while (fscanf(f,"%s",inline) != EOF){
				if(inline[0] == 'V'){
					tptr = rindex(inline,'/');
					strcpy(vector,++tptr);
				}
				for(jj=0;jj<4;jj++){
					fscanf(f,"%s",inline);
					if(inline[0] == 'T'){
						tptr = rindex(inline,'/');
						strcpy(text,++tptr);
					}
					if(inline[0] == 'R')
						continue;	
				}

				fscanf(f,"%s %d",inline,&i);

				if(debug==1) logerr("v=%s, t=%s\n",vector,text);

				if (access(lock,0) < 0) {
					logerr("Lock has disappeared.\n");
					dem_dis();
					exit(1);
				}

				if ((pid=fork()) == 0) {
					unlink(PLOTFILE);
					stat(dfb->d_name,&statbuf);
					setuid(statbuf.st_uid);
					setgid(statbuf.st_gid);
					execl(LJPLOT,"ljplot",vector,text,NULL);
					logerr("exec failed %s %s\n",
								vector,text);
				}

				if (pid==-1) {
					logerr("ljplot fork failed %s %s\n",
								vector,text);
					logerr("\terr=\n",errno);
				}
				else
					while (pid!=wait(&status));

				if (status != 0) {
					logerr("ljplot failed %s %s\n",
					vector,text);
					logerr("\tstatus was %d\n", status);
					continue;
				}

				unlink(vector);
				unlink(text);

				if (debug) logerr("%d copies\n",i);

				for (j=0; j<i; j++) {

					if ((pid=fork()) == 0){
						stat(dfb->d_name,&statbuf);
						setuid(statbuf.st_uid);
						setgid(statbuf.st_gid);
						execl(LJDUMP,"lp","-or","-onb",
								PLOTFILE,NULL);
					}

					if (pid==-1) {
						logerr(
						"lpr fork failed %s %s\n",
						vector,text);
						logerr("\terr=\n",errno);
					}
					else
						while (pid!=wait(&status));

					if (status!=0) {
						j--;
						logerr("lp failed %d\n",status);
					}
				}
				acct();
			}
			fclose(f);
			unlink(dfb->d_name);
		}
	}
	closedir(dpd);
	dem_dis();
}

dem_dis()
{
	unlink(lock);
}

#define DAEMON 2

dem_setup()
{
	int i,of,die();

	signal(SIGTERM,die);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT,SIG_IGN);
	signal(SIGHUP,SIG_IGN);

	freopen("/dev/null", "r", stdin);
	freopen("/dev/null", "w", stdout);
	freopen("/dev/null", "w", stderr);

	for (of=3; of<_NFILE; of++)
		close(of);

	if (stat(lock,&statbuf)!=-1) {
		logerr("lockfile exists already\n");
		exit(0);
	}
	if ((of = creat(lock, 0)) < 0) {
		logerr("creat failed on lockfile\n");
		exit(1);
	}
	if (fstat(of, &statbuf) < 0 ||  statbuf.st_mode != 0100000){
		logerr("Bad lock file   %s.\n",lock);
		exit(1);
	}
	close(of);
	if (i = fork()) {
		if (i   == -1){
			logerr("Unable to fork.\n");
			unlink(lock);
			exit(1);
		}
		exit(0);
	}
	chdir(dpd);
}

struct {
	short uid;
	long start;
	long finish;
	long ut;
	long st;
}
acctbuf;

#include <time.h>

acct()
{
	struct tm *tp;
	int fd;

	stat(dfname,&statbuf);

	time(&te);
	times(&teb);

	acctbuf.uid = statbuf.st_uid;
	acctbuf.start = ts;
	acctbuf.finish  = te;
	acctbuf.ut = teb.proc_user_time -
	    tsb.proc_user_time +
	    teb.child_user_time -
	    tsb.child_user_time;
	acctbuf.st = teb.proc_system_time -
	    tsb.proc_system_time +
	    teb.child_system_time -
	    tsb.child_system_time;
	fd = open(acctfile,1);
	lseek(fd,0L,2);
	write(fd,&acctbuf,sizeof acctbuf);
	close(fd);
	tp = localtime(&te);
}

die()
{
	signal(SIGTERM,SIG_IGN);
	system("ls /users/spool/gplot >> /users/log/daemon");
	dem_dis();
	logerr("plotdaemon killed\n");
	exit(1);
}
