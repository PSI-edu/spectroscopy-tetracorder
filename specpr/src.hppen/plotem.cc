#include	"hpdaemon.h"

/*
 *	@(#)plotem.c	2.11 06/04/87 17:55:35
 */


static char Sccsid[]="@(#)plotem.c	2.11 06/04/87 17:55:35";

#define	NORMAL		'@'
#define	SUBSCRIPT	'{'
#define	SUPSCRIPT	'}'
#define	GREEK		'!'
#define	MATH		'|'

#define BREAK_CHARS	"{}@!|"
#define	WHITE		" \t"

#define	PI	3.14159
#define	PI2	PI/2

#define NullS   (char*)0

/*  NullS is the "nil" character  pointer.   NULL  would  work  in most
    cases,  but  in  some  C  compilers  pointers and integers may be of
    different sizes, so it is handy to have a nil pointer that  one can
    pass to a function as well as compare pointers against.
 */

char	
plotem()
{
	struct	vector	v;
	char	type;
	char	line[256];
	char	tbuf[256];
	char	*start_p, *next_p;
	char	brk_c;
	float	pangle;
	int		down,across,font,angle;
	int    sympen;   
	int		c_pen, c_down, c_across;
	int		text_count = 0;
	int		symbol_count = 0;
	int	delaycnt;
	float	txt_scale;
	char	mode = NORMAL;
	char	symbol_set = NORMAL;

	max_down = max_across = c_pen = c_down = c_across = 0;
	MOVETO(0,0);
	WAIT(line);

	delaycnt = 0;
	while (fread( (char *)&v, sizeof v, 1, vector)!=NULL) {
		delaycnt++;
		if (delaycnt > 20) {    /* delay so plotter can catch up */
			WAIT(line);
			delaycnt = 0;
		}
		if (v.w!=c_pen) {
			WAIT(line);
			PEN(v.w);
			c_pen = v.w;
		}
		if (v.af>v.at) {
			across = v.af;
			down = v.df;
			v.af = v.at;
			v.df = v.dt;
			v.at = across;
			v.dt = down;
		}
		if (v.df!=c_down || v.af!=c_across) {
			MOVETO(v.df,v.af);
		}
		DRAWTO(v.dt,v.at);
		c_down = v.dt;
		c_across = v.at;
	}
	fseek(vector, 0L, 0);
	WAIT(line);

	while (fgets(line,sizeof line,text)!=(char *)NULL) {
		sscanf(line,"%c%d%d%d%d%[^\n]\n",
			&type,
			&down,
			&across,
			&font,
			&angle,
			tbuf);
		switch(type) {
		case 'T':							/*** text ***/
			WAIT(line);
                                        /* trim trailing blanks */
/*NONHPUX
NONHPUX*/
/*DEC                            strtrim(tbuf, tbuf, WHITE, 1);
DEC*/
/*HPUX                           strtrim(tbuf, tbuf, WHITE, 1);
HPUX*/
/*  RED - For Linux, removed call to strtrim since not in library */
/*LINUX            
LINUX*/
/*IA64HPUX
IA64HPUX*/

			txt_scale = 1.0;
			text_count++;
			if (2!=c_pen) {
			        PEN(2);
				c_pen = 2;
			}
			if (angle==0) pangle = PI2;
			else pangle = PI;
			MOVETO(down,across);
			start_p = tbuf;
			while ((next_p = strpbrk(start_p,BREAK_CHARS))!=NullS) {
				brk_c = *next_p;
				*next_p = NULL;
				TEXTSIZE(txt_scale*font);
				switch (symbol_set) {
				case	NORMAL:
					TEXT(pangle,start_p);
					break;

				case	MATH:
					TEXT(pangle,start_p);
					break;

				case	GREEK:
					greek(pangle,start_p);
					break;

				default:
					break;
				}
				switch(brk_c) {
				case NORMAL:
					switch (mode) {
					case SUBSCRIPT:
						TEXTSIZE(font);
						CP(0.0,0.5);
						break;
					case SUPSCRIPT:
						TEXTSIZE(font);
						CP(0.0,-0.5);
						break;
					default:
						break;
					}
					mode = NORMAL;
					txt_scale = 1.0;
					break;
				
				case SUBSCRIPT:
					TEXTSIZE(font);
					CP(0.0,-0.5);
					mode = SUBSCRIPT;
					txt_scale = 0.6;
					break;

				case SUPSCRIPT:
					TEXTSIZE(font);
					CP(0.0,0.5);
					mode = SUPSCRIPT;
					txt_scale = 0.6;
					break;
				
				case GREEK:
					if (symbol_set != GREEK) {
						symbol_set = GREEK;
						txt_scale = 0.5;
					} else {
						symbol_set = NORMAL;
						txt_scale = 1.0;
					}
					break;

				case MATH:
					if (symbol_set != MATH) {
						symbol_set = MATH;
					} else {
						symbol_set = NORMAL;
					}
					break;

				default:
					break;
				}
				start_p = next_p + 1;
			}
			TEXTSIZE(txt_scale*font);
			switch (symbol_set) {
			case	NORMAL:
				TEXT(pangle,start_p);
				break;

			case	MATH:
				TEXT(pangle,start_p);
				break;

			case	GREEK:
				greek(pangle,start_p);
				break;

			default:
				break;
			}
			break;

		case 'S':							/*** symbol ***/

			WAIT(line);
			symbol_count++;

		sscanf(tbuf,"%d",&sympen);
			if (sympen != c_pen) {
          	         	PEN(sympen);
				c_pen=sympen;
			}
                        TEXTSIZE(font);
                        if (font == 1)
                                TEXTSIZE((font+.5));

			MOVETO(down,across);
			switch(angle) {


			case 0: 		/* square */
				UC("-99 10 20 +99 -20 0 0 -40 20 0 0 40;");
				break;
			case 1:			/* plus sign */
				UC("-99 0 28 +99 0 -56 -99 14 28 +99 -28 0;");
				break;
			case 2: 		/* cross */
				UC("-99 5 18 +99 0 5 -10 0 0 -10 -5 0 0 -20 5 0 0 -10 10 0 0 10 5 0 0 20 -5 0;");
				break;
			case 3: 		/* diamond */
				UC("-99 0 28 +99 -14 -28 14 -28 14 28 -14 28;");
				break;
			case 4:			/* filled square */
				UC("-99 -10 -20 +99 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 2 -20 0 0 2 20 0 0 -40 -20 0 0 40;");
				break;
			case 5: 		/* point */
				DRAWTO(down,across);
				MOVETO(down,across);
				break;
			case 6:			/* triangle down */
				UC("-99 0 -26 +99 14 43 -28 0 14 -43;");
				break;
			case 7:			/* triangle up */
				UC("-99 0 28 +99 -14 -43 28 0 -14 43;");
				break;
			case 8:			/* circle */
				UC("-99 3 -20 +99 -6 0 -3 3 -2 5 -2 7 0 10 2 7 2 5 3 3 6 0 3 -3 2 -5 2 -7 0 -10 -2 -7 -2 -5 -3 -3;");
				break;
			case 9:			/* filled cross */
				UC("-99 -5 -20 +99 10 0 0 2 -10 0 0 2 10 0 0 3 -10 0 0 3 10 0 -99 5 1 +99 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 0 3 20 0 0 3 -20 0 -99 5 1 +99 10 0 0 3 -10 0 0 3 10 0 0 2 -10 0 0 2 10 0 -10 0 0 -10 -5 0 0 -20 5 0 0 -10 10 0 0 10 5 0 0 20 -5 0 0 5;");
				break;
			case 10:		/* filled diamond */
				UC("-99 0 28 +99 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 1 -2 14 28 1 -2 -14 -28 -14 28 14 28 14 -28;");
				break;
			case 11:		/* X */
				UC("-99 -10 20 +99 20 -40 -99 0 40 +99 -20 -40;");
				break;
			case 12:		/* filled triangle up */
				UC("-99 -14 -14 +99 28 0 0 3 -26 0 0 3 24 0 0 3 -22 0 0 3 20 0 0 3 -18 0 0 3 16 0 0 3 -14 0 0 3 12 0 0 3 -10 0 0 3 8 0 0 3 -6 0 0 3 4 0 0 3 -2 0 0 3 -14 -43 29 0 -14 43;");
				break;
			case 13: 		/* filled triangle down */
				UC("-99 -14 17 +99 28 0 0 -3 -26 0 0 -3 24 0 0 -3 -22 0 0 -3 20 0 0 -3 -18 0 0 -3 16 0 0 -3 -14 0 0 -3 12 0 0 -3 -10 0 0 -3 8 0 0 -3 -6 0 0 -3 4 0 0 -3 -2 0 0 -3 1 0 14 43 -29 0 14 -43;");
				break;
			case 14:		/* filled circle */
				UC("-99 -3 -20 +99 6 0 3 3 -12 0 -1 2 14 0 1 3 -16 0 -1 3 18 0 1 3 -20 0 0 1 20 0 0 3 -20 0 0 2 20 0 0 2 -20 0 0 3 20 0 0 1 -20 0 1 3 18 0 -1 3 -16 0 1 3 14 0 -1 2 -12 0 3 3 6 0 3 -3 2 -5 2 -7 0 -10 -2 -7 -2 -5 -3 -3 -6 0 -3 3 -2 5 -2 7 0 10 2 7 2 5 3 3 6 0;");
				break;
			case 15:		/* sample of number(not used) */
				TEXTSIZE(11*font);
				CP(-0.333,-0.25);
				TEXT(PI2,"1");
				break;
			default:		/* plus sign */
				UC("-99 0 28 +99 0 -56 -99 14 28 +99 -28 0;");
				break;



			}
			break;
		}
	}
	if (ferror(text)) logerr("I/O error on text file\n");
	if (ferror(vector)) logerr("I/O error on vector file\n");
	logerr("text_count=%d\n",text_count);
	logerr(" symbol_count=%d\n",symbol_count);
	fseek(text, 0L, 0);
	WAIT(line);

}
