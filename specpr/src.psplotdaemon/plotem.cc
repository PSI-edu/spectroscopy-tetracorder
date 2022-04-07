#include	"psdaemon.h"

/*
 *	plotem.c
 */

#define	NORMAL		'@'
#define	SUBSCRIPT	'{'
#define	SUPSCRIPT	'}'
#define	GREEK		'!'
#define	MATH		'|'

#define BREAK_CHARS	"{}@!|"
#define PAREN		"()"
#define	WHITE		" \t"
#define LP		"\\("
#define RP		"\\)"

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
	char	tbuf1[256];
	
	char	*start_p, *next_p, *temp_p;
	char	brk_c;
	int	down,across,font,angle;
	int	sympen;   
	int	c_width, c_down, c_across, c_downf, c_acrossf;
	int	text_count = 0;
	int	symbol_count = 0;
	int	trailblank;
	int	needstroke, curvecnt;
	int	found_PAREN;
	int	txt_scale;
	char	mode = NORMAL;
	char	symbol_set = NORMAL;

	max_down = max_across = c_width = c_down = c_across = 0;

	MOVETO(0,0);
	needstroke=0;
	curvecnt=0;
	
	while (fread( (char *)&v, sizeof v, 1, vector)!=NULL) {

	  if (curveto == 0) { 
		/* Just line between two points */
                if (v.w!=c_width) {
                        if (c_down!=0 && c_across!=0) {
                                STROKE;
                                needstroke=0;
                        } else {
                                needstroke=1;
                        }
                        SETWIDTH(v.w);
                        c_width = v.w;
                }
                if (v.af>v.at) {
                        /* RED ??  */
                        /* This is to switch from and to points */
                        across = v.af;
                        down = v.df;
                        v.af = v.at;
                        v.df = v.dt;
                        v.at = across;
                        v.dt = down;
                }
                if (v.df!=c_down || v.af!=c_across) {
                        if (needstroke==1) {
                                if (c_down!=0 && c_across!=0) {
                                        STROKE;
                                }
                                needstroke=0;
                        }
                        MOVETO(v.df,v.af);
                        LINETO(v.dt,v.at);
                        needstroke=1;
                } else {
                        LINETO(v.dt,v.at);
                        needstroke=1;
                }
                c_down = v.dt;
                c_across = v.at;

	    } else {
		/* curve with three points */

		if (v.w!=c_width) {
			if (c_down!=0 && c_across!=0) {
				STROKE;
				needstroke=0;
			} else {
				needstroke=1;
			}
			SETWIDTH(v.w);
			c_width = v.w;
		}

		if (v.af>v.at) {
			/* RED ??  */
			/* This is to switch from and to points */
			across = v.af;
			down = v.df;
			v.af = v.at;
			v.df = v.dt;
			v.at = across;
			v.dt = down;
		}
		if (v.df!=c_down || v.af!=c_across) {
			if (needstroke==1) {
				if (c_down!=0 && c_across!=0) {
					STROKE;
				}
				needstroke=0;
			}
			MOVETO(v.df,v.af);
			LINETO(v.dt,v.at);
			needstroke=1;
		} else {
			if (v.df==c_down && v.af==c_across) {
				if (curvecnt==0) {
					curvecnt=1;
				} else {
					CURVETO(c_downf,c_acrossf,v.df,v.af,v.dt,v.at);
					curvecnt=0;
				}
			} else {
				LINETO(v.dt,v.at);
				needstroke=1;
			}
		}
		c_down = v.dt;
		c_across = v.at;
		c_downf = v.df;
		c_acrossf = v.af;
	   }
/* DEBUG
	fprintf(stdout,"df %d af %d dt %d at %d w %d\n",v.df,v.af,v.dt,v.at,v.w);
*/
	}
	if (needstroke==1) {
		STROKE;
	}
	fseek(vector, 0L, 0);


/* Now process the text */

	MOVETO(0,0);
        if (2!=c_width) {
		SETWIDTH(2);
                c_width = 2;
        }
	txt_scale=3;

	while (fgets(line,sizeof line,text)!=(char *)NULL) {
		sscanf(line,"%c%d%d%d%d%[^\n]\n",
			&type,
			&down,
			&across,
			&font,
			&angle,
			tbuf);
		switch(type) {
		case 'T':		/*** text ***/
					/* trim trailing blanks */
/*NONHPUX
NONHPUX*/	
/*DEC                            strtrim(tbuf, tbuf, WHITE, 1);
DEC*/
/*HPUX                           strtrim(tbuf, tbuf, WHITE, 1);
HPUX*/
/* RED 02/07/08
   The following was the commented out next statement after the  
   "while (trailblank==1) {" below.  Move to here since, on something other
   than IA64HPUX, it results in a comment inside a comment for which the
   gcc compiler had a problem with.
*/
/*      fprintf(stdout,"H%sH\n",start_p + strlen(tbuf) -1); */ 

/*IA64HPUX
			trailblank=1;
			start_p = tbuf; 
			while (trailblank==1) {	
				next_p = start_p + strlen(tbuf) - 1;
				if (*next_p != ' ') {
					trailblank = 0;
				} else {
					*next_p = NULL;
				}
			}
IA64HPUX*/
/*   RED - For linux, change to not call strtrim since not in library */
/*LINUX          
LINUX*/
/*IA64HPUX        
IA64HPUX*/

                        MOVETO(down,across);
                        TEXTSIZE(txt_scale*font);
                        if (angle==0) {
                                COURIER90;
                        } else if (angle==1) {
                                COURIER180;
                        }

			start_p = tbuf;
			temp_p = tbuf1;
			*temp_p = NULL;
			found_PAREN = 0;


			/* Put a slash before any left or right paren */
			while ((next_p = strpbrk(start_p,PAREN))!=NullS) {
				found_PAREN = 1;
				brk_c = *next_p;
				*next_p = NULL;
				strcat(temp_p,start_p);
				switch (brk_c) {
				case	'(':
					strcat(temp_p,"\\(");
					break;
				case	')':
					strcat(temp_p,"\\)");
					break;
				}
				start_p = next_p + 1;
				

			}
			if (found_PAREN == 1) {
				/* Get the remainder */
				strcat(temp_p,start_p);
				start_p = tbuf1;
			}

			while ((next_p = strpbrk(start_p,BREAK_CHARS))!=NullS) {
				brk_c = *next_p;
				*next_p = NULL;

				switch (symbol_set) {
				case	NORMAL:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
		                        if (angle==0) {
						ROTATE90;
					} else if (angle==1) {
						ROTATE180;
					}
					break;

				case	MATH:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
                                        if (angle==0) {
                                                ROTATE90;
                                        } else if (angle==1) {
                                                ROTATE180;
                                        }
					break;

				case	GREEK:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
                                        if (angle==0) {
                                                ROTATE90;
                                        } else if (angle==1) {
                                                ROTATE180;
                                        }
					break;

				default:
					break;
				}
				switch(brk_c) {
				case NORMAL:
					switch (mode) {
					case SUBSCRIPT:
						TEXTSIZE(txt_scale*font);
						break;
					case SUPSCRIPT:
						TEXTSIZE(txt_scale*font);
						break;
					default:
						break;
					}
					mode = NORMAL;
					txt_scale = 3;
					break;
				
				case SUBSCRIPT:
					mode = SUBSCRIPT;
					txt_scale = 2; /* 0.6 */
					TEXTSIZE(txt_scale*font);
					break;

				case SUPSCRIPT:
					mode = NORMAL;
					txt_scale = 3;
					TEXTSIZE(txt_scale*font);
					break;
				
				case GREEK:
					if (symbol_set != GREEK) {
						symbol_set = GREEK;
						txt_scale = 3;
			                        TEXTSIZE(txt_scale*font);
			                        if (angle==0) {
			                                SYMBOL90;
			                        } else if (angle==1) {
			                                SYMBOL180;
			                        }

					} else {
						symbol_set = NORMAL;
						txt_scale = 3;
						TEXTSIZE(txt_scale*font);
						if (angle==0) {
							COURIER90;
						} else if (angle==1) {
							COURIER180;
						}
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
			}  /* End while next_p not equal null */

			/* Output remaining after any break charcters or none */
			
/*			TEXTSIZE(txt_scale*font); */
			switch (symbol_set) {
				case	NORMAL:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
					break;
	
				case	MATH:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
					break;
	
				case	GREEK:
					TEXTLP;
					TEXT(start_p);
					TEXTRP;
					SHOWNL;
					break;
	
				default:
					break;

			} /* end switch(symbol_set) */

			/* May need to move */
                        if (angle==0) {
                                ROTATE90;
                        } else if (angle==1) {
                                ROTATE180;
                        }


			break; /* Case type T */

			/* Finish up with a trailing newline and rotate */
			
		}  /* end switch(type) */
	}  /* end while gets */

	if (ferror(text)) logerr("I/O error on text file\n");
	if (ferror(vector)) logerr("I/O error on vector file\n");
	logerr("text_count=%d\n",text_count);
	logerr(" symbol_count=%d\n",symbol_count);
	fseek(text, 0L, 0);

}
