/* 
 @(#) $Revision: 1.1.119.2 $ 
*/       
#ifndef _SGTTY_INCLUDED
#define _SGTTY_INCLUDED

#include <sys/ioctl.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * sgtty.h
 * Structure for stty and gtty system calls.
 *
 * made changes to Bxxx constants and ioctl argument list
 */

# ifndef _SGTTYB_DEFINED
# define _SGTTYB_DEFINED
struct sgttyb {
	char	sg_ispeed;		/* input speed */
	char	sg_ospeed;		/* output speed */
	char	sg_erase;		/* erase character */
	char	sg_kill;		/* kill character */
	int	sg_flags;		/* mode flags */
};
# endif /* _SGTTY_DEFINED */

/*
 * Modes
 */
#define	HUPCL	01
#define	XTABS	02
#define	LCASE	04
#define	ECHO	010
#define	CRMOD	020
#define	RAW	040
#define	ODDP	0100
#define	EVENP	0200
#define ANYP	0300
#define	NLDELAY	001400
#define	TBDELAY	002000
#define	CRDELAY	030000
#define	VTDELAY	040000
#define BSDELAY 0100000
#define ALLDELAY 0177400

/*
 * Delay algorithms
 */
#define	CR0	0
#define	CR1	010000
#define	CR2	020000
#define	CR3	030000
#define	NL0	0
#define	NL1	000400
#define	NL2	001000
#define	NL3	001400
#define	TAB0	0
#define	TAB1	002000
#define	NOAL	004000
#define	FF0	0
#define	FF1	040000
#define	BS0	0
#define	BS1	0100000

/*
 * Speeds
 */
#define B0	0
#define B50	1
#define B75	2
#define B110	3
#define B134	4
#define B150	5
#define B200	6
#define B300	7
#define B600	8
#define B900	9
#define B1200	10
#define	B1800	11
#define B2400	12
#define B3600	13
#define B4800	14
#define B7200	15
#define B9600	16
#define B19200	17
#define B38400	18
#define EXTA	30
#define EXTB	31

/*
 *	ioctl arguments
 */
#define	TIOCGETP	_IOR('t', 8, struct sgttyb)
#define	TIOCSETP	_IOW('t', 9, struct sgttyb)

/* The following ioctl commands are not currently supported by Series 500 */
#define	FIOCLEX		_IO('f', 1)		/* set exclusive use on fd */
#define	FIONCLEX	_IO('f', 2)		/* remove exclusive use */

#if defined(__ia64) && ! defined(_LIBC)
  /* pragmas needed to support -B protected */
#  pragma extern stty, gtty
#endif /* __ia64 */

#if defined(__STDC__) || defined(__cplusplus)
   extern int stty(int, const struct sgttyb *);
   extern int gtty(int, struct sgttyb *);
#else /* not __STDC__ || __cplusplus */
   extern int stty();
   extern int gtty();
#endif /* __STDC__ || __cplusplus */

#ifdef __cplusplus
}
#endif

#endif /* _SGTTY_INCLUDED */
