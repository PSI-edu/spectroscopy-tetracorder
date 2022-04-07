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
	int     iangle;
	float	txt_scale;
	char	mode = NORMAL;
	char	symbol_set = NORMAL;

	max_down = max_across = c_pen = c_down = c_across = 0;
	MOVETO(0,0);

	delaycnt = 0;
	while (fread( (char *)&v, sizeof v, 1, vector)!=NULL) {
		delaycnt++;
		if (delaycnt > 20) {    /* delay so plotter can catch up */
			delaycnt = 0;
		}
		if (v.w!=c_pen) {
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
					/* trim trailing blanks */
/*NONHPUX
NONHPUX*/	
/*DEC                            strtrim(tbuf, tbuf, WHITE, 1);
DEC*/
/*HPUX                           strtrim(tbuf, tbuf, WHITE, 1);
HPUX*/
/*   RED - For linux, change to not call strtrim since not in library */
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


                               /* GCS(a,x,v,h,str) = plot string centered on cursor
				*     a = angle
				*     x = point size
				*     v = vertical offset to center
				*     h = horizontal offset to center
				*     str = string to print
				*/

			case 0: 		/* square: symbol 1*/
				GCS(90,font*10,font*38,font*26,"\376");
				break;
			case 1:			/* plus sign: symbol 7*/
				GCS(90,font*7,font*22,font*32,"\053");
				break;
			case 2: 		/* cross: symbol 2 */
				GCS(90,font*7,font*26,font*27,"\130");
				break;
			case 3: 		/* diamond: symbol 3 */
				GCS(90,font*7,font*26,font*34,"\004");
				break;
			case 4:			/* clover: symbol 9 */
				GCS(90,font*7,font*25,font*35,"\005");
				break;
			case 5: 		/* spade: symbol 15 */
				GCS(90,font*7,font*26,font*34,"\006");
				break;
			case 6:			/* triangle down: symbol 5 */
				GCS(90,font*10,font*33,font*35,"\037");
				break;
			case 7:			/* triangle up: symbol 4 */
				GCS(90,font*10,font*26,font*35,"\036");
				break;
			case 8:			/* circle: symbol 6 */
				GCS(90,font*7,font*28,font*35,"\001\010\002");
				break;
			case 9:			/* heart: symbol 10 */
				GCS(90,font*7,font*30,font*35,"\003");
				break;
			case 10:		/* open circle: symbol 11 */
				GCS(90,font*7,font*26,font*25,"\117");
				break;
			case 11:		/* asterisk: symbol 8 */
				GCS(90,font*9,font*41,font*24,"\052");
				break;
			case 12:		/* A: symbol 12 */
				GCS(90,font*7,font*23,font*26,"\101");
				break;
			case 13: 		/* B: symbol 13 */
				GCS(90,font*7,font*25,font*25,"\102");
				break;
			case 14:		/* C: symbol 14 */
				GCS(90,font*7,font*26,font*26,"\103");
				break;
			case 15:		/* D: symbol ? */
				GCS(90,font*7,font*26,font*28,"\104");
				break;
			default:		/* plus sign */
				GCS(90,font*7,font*22,font*32,"\053");
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

}
