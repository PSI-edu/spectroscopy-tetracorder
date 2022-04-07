/* 02/29/2002 Randall Dailey
 *   Added IA64HPUX stuffi
 *   Added brackets to single statement ifs
 */
/* added machine dependent names for HPUX 3/26/85. Roger Clark */
#include <stdio.h>

recomp(str)
char str[];
{
	char *s;
	char *re_comp(), *regcmp();

/*BSD
	if((s=re_comp(str)) != 0) printf("ERROR%s",s);
BSD*/

/* NOTE: here, HPUX and Solaris, thus SYSV are same, may also be
         elsewhere.  RNC 10/9/96  */
/*HPUX
	if((s=regcmp(str,0)) == 0) {
		 printf("ERROR%s",s);
	}
HPUX*/
/*IA64HPUX
	if((s=regcmp(str,0)) == 0) {
		 printf("ERROR%s",s);
	}
IA64HPUX*/
/*SYSV
	if((s=regcmp(str,0)) == 0) {
		 printf("ERROR%s",s);
	}
SYSV*/

/*LINUX
	if((s=regcomp(str,0)) == 0) {
		 printf("ERROR%s",s);
	}
LINUX*/

/* debug:
 *	if(s != 0) {
 *		 printf("recomp result=%s\n",s);
 *	}
 */
	return(s);
}

/* NOTE: the following commented to HPUX on 2/17/2001.
 * for linux porting.  Not sure of effect on HP or Sun compiles
 *   - RC
 */

/*HPUX
recomp_(str)
char str[];
{
	recomp(str);
	return;
}
HPUX*/
/*SUNOS
recomp_(str)
char str[];
{
	recomp(str);
	return;
}
SUNOS*/
/*IA64HPUX
recomp_(str)
char str[];
{
	recomp(str);
	return;
}
IA64HPUX*/


/* added machine dependent names for HPUX 3/26/85. Roger Clark */

reexec(scomp,bufstr)

char scomp[];
char bufstr[];
{
	char *s;
	int re_exec();
	int x;

/*BSD
	return(re_exec(bufstr));
BSD*/
/*HPUX
	if((s=regcmp(scomp,0)) == 0) {
		 printf("String Compilation ERROR%s",s);
	}
	x = re_exec(s,bufstr);

	return(x);
HPUX*/
/*IA64HPUX
	if((s=regcmp(scomp,0)) == 0) {
		 printf("String Compilation ERROR%s",s);
	}
	x = re_exec(s,bufstr);
	return(x);
IA64HPUX*/
/*SYSV
	if((s=regcmp(scomp,0)) == 0) {
		 printf("String Compilation ERROR%s",s);
	}
	x = re_exec(s,bufstr);

	return(x);
SYSV*/
/*LINUX
	if((s=regcomp(scomp,0)) == 0) {
		 printf("String Compilation ERROR%s",s);
	}
	x = regexec(s,bufstr);

	return(x);
LINUX*/
}

reexec_(scomp,bufstr)
char scomp[];
char bufstr[];
{
	/* was */
	/* reexec(scomp,bufstr); */
	/* return; */
	return(reexec(scomp,bufstr));
}
