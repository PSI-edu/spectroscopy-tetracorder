#include "splot.h"

/*
 *	%W% %G% %U%
 */

static char Sccsid[] = "%W% %G% %U%";

extern errno;

plotem()
{
	vplt();
	logerr("after vplt: max_down=%d\n", max_down);
	tplt();
	logerr("after tplt: max_down=%d\n", max_down);
}


vplt()
{
	struct vector a;

	while (fread(&a, sizeof a, 1, vector) != NULL) {
		line(a.df,a.af,a.dt,a.at,a.w);
	}
}


setbit(d,a)
register int d,a;
{
	if (a < 0 || a > BPL || d > NLINES) return(1);
	else bbuf[(a >> 3)+WPL*(d)] |= 1 << (7-(a & 7));
}

#define isign(x)        ((x == 0)?0:(x > 0)?1:-1)
#define fsign(x)        ((x == 0.0)?0.0:(x > 0.0)?1.0:-1.0)

line(df,af,dt,at,width)
{
      int i,kk;
      register int d,a,ddt; 
	  register int aat,j,l,dd,aa;
      float slope;


	      if (dt < df) swap(&df,&af,&dt,&at);
		  if (dt > max_down) max_down = dt;
		  if (at > max_across) max_across = at;
		  if (af > max_across) max_across = af;

	      if (af != at) slope = (float)(df - dt)/(float)(af - at);
	      else {
		      vline(af,df,dt,width);
		      return(1);
	      }
	      if (df == dt) {
		      hline(df,af,at,width);
		      return(1);
	      }

	      dd = (fabs(slope) < 2.414) ? 1: 0;
	      aa = (int) ((( fabs(slope) > .414)? -1.0: 0.0) * fsign(slope));

	      for(kk= -width/2; kk<=width/2; kk++){
		      j=kk*dd;
		      l=kk*aa;
		      d = df+j;
		      ddt=dt+j;
		      a = af+l;
		      aat=at+l;

		      while (d < ddt || a != aat) {

			      if ( (a-af-l) != 0) {
				      if (((float)(d-df-j)/(float)(a-af-l))*fsign(slope)
						  > slope*fsign(slope))
							  a += isign(aat - a);
				      else d += isign(ddt - d);
			     } else a += isign(aat - a);
			     setbit(d,a);
		      }

      }
}

swap(df,af,dt,at)
register int *df, *af, *dt, *at;
{
      register int t;

      t = *df;
      *df= *dt;
      *dt= t;
      t = *af;
      *af= *at;
      *at= t;
}

hline(df,af,at,width)
{
       register int i,j,k;
       k=df-width/2;

       if (af > at) {
		     j = af;
		     af = at;
		     at = j;
	       }
       do
	   for (i=af; i<=at; i++) {
	       setbit(k,i);
       }
       while (++k <= df+width/2);

}

 vline(af,df,dt,width)
 {
       register int i,j,k;
       k=af-width/2;

       do
	   for (i=df; i<=dt; i++) {
	       setbit(i,k);
       }
       while (++k <= af+width/2);
}


#include <vfont.h> 

int font;                       /* font file */
int down,across,size,angle;
char type,string[100];


struct header header;

struct dispatch dispatch[256];


tplt()
{
	int		sinit=0;
	char	input[256];

	while(fgets(input, sizeof input, text)!=NULL) {
		sscanf(input,"%c%d%d%d%d%s",
			&type,
			&down,
			&across,
	    	&size,
			&angle,
			string);
		
		logerr("tplt: %s\n", string);
		if (down > max_down) max_down = down;
		if (across > max_across) max_across = across;


	    if (angle == 0 && type == 'T') {
			getfont(size,angle,'B');
			plotchar(string);
			sinit=0;
	    } else if (type == 'S') {
			if (sinit==0) syminit();
			sinit=1;
			getsym();
		} else if (type == 'T') {
			getfont(size,angle,'B');
			plotrchar(string);
			sinit=0; 
		} else logerr("Invalid text type\n"); 
	}
}

#define RFONT "/data/lib/vfont/%c.%d"
#define VFONT "/data/lib/vfont/%c.%dr"

char ffile[30];

int cfont  = 0;
int cangle = 0;
char ctype = 'B';    /* current font type B=bold, S=special */
char oldtype = 'B';
int smode = 0;          /* sub&superscripting mode -1=subscript */
			/*                          0=no script */
			/*                         +1=super script  */

int fsize[] = {6,6,6,6,6,6,6,7,8,9,10,11,12,12,14,14,16,16,18,18,20,20};

getfont(size,angle,type)
char type;
{
	int i;

	if (cfont != size || cangle != angle || ctype != type) {
		logerr("new font: angle=%d, size=%d\n", angle, size);
		logerr("        : type=%c\n", type);
		cangle = angle;
		cfont = size;
		ctype = type;
		cfont = (cfont<0)?0:cfont;
		cfont = (cfont>21)?21:cfont;
		close(font);
		if (angle == 0)
			sprintf(ffile,RFONT,type,fsize[size]);
		else sprintf(ffile,VFONT,type,fsize[size]);
		logerr("getfont: %s\n", ffile);
		font = open(ffile,0);
		if (font < 0) {
			logerr(" Can't open %s.\n",ffile);
			exit(6);
		}
		i = read(font, &header, sizeof header);
		if (i!=sizeof header || header.magic!=0436) {
			logerr("Bad font file\n");
			exit(7);
		} 
		i = read(font, dispatch, sizeof dispatch);
		if (i!=sizeof dispatch) {
			logerr("Bad font file\n");
			exit(7);
		} 
/*
		logerr("header: magic=%d\n", header.magic);
		logerr("      : size=%d, xtend=%d\n", header.size, header.xtend);
		logerr("      : maxx=%d, maxy=%d\n", header.maxx, header.maxy);
		for (i=0; i<128; i++) {
			logerr(" Dispatch: %d\n", i);
			logerr("         : addr = %d\n", dispatch[i].addr);
			logerr("         : nbytes = %d\n", dispatch[i].nbytes);
			logerr("         : up,down = %d,%d\n", 
								dispatch[i].up, dispatch[i].down);
			logerr("         : left,right = %d,%d\n",
								dispatch[i].left, dispatch[i].right);
			logerr("         : width=%d\n", dispatch[i].width);
		}
*/
	}
}

#define NSYM 16
#define MSIZE 5
char symbol[NSYM][32]= {

/*0*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x1f, 0xf8, 0x1f, 0xf8, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x1f, 0xf8, 0x1f, 0xf8, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*1*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
	  0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*2*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xc0,
	  0x03, 0xc0, 0x03, 0xc0, 0x1f, 0xf8, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x1f, 0xf8, 0x03, 0xc0, 0x03, 0xc0,
	  0x03, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},


/*3*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x80,
	  0x03, 0xc0, 0x07, 0xe0, 0x0f, 0xf0, 0x1f, 0xf8,
	  0x1f, 0xf8, 0x0f, 0xf0, 0x07, 0xe0, 0x03, 0xc0,
	  0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*4*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x80,
	  0x01, 0x80, 0x03, 0xc0, 0x03, 0xc0, 0x07, 0xe0,
	  0x07, 0xe0, 0x0f, 0xf0, 0x0f, 0xf0, 0x1f, 0xf8,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*5*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18,
	  0x1c, 0x38, 0x0e, 0x70, 0x07, 0xe0, 0x03, 0xc0,
	  0x03, 0xc0, 0x07, 0xe0, 0x0e, 0x70, 0x1c, 0x38,
	  0x18, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*6*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x80,
	  0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*7*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x01, 0xc0, 0x01, 0xc0,
	  0x01, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*8*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x1f, 0xf8, 0x0f, 0xf0, 0x0f, 0xf0, 0x07, 0xe0,
	  0x07, 0xe0, 0x03, 0xc0, 0x03, 0xc0, 0x01, 0x80,
	  0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*9*/   { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x80, 0x01, 0xc0, 0x03, 0xe0,
	  0x01, 0xc0, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*10*/  { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x01, 0xc0, 0x03, 0xe0, 0x03, 0xe0,
	  0x03, 0xe0, 0x01, 0xc0, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*11*/  { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x00, 0x80, 0x03, 0xe0, 0x03, 0xe0, 0x07, 0xf0,
	  0x03, 0xe0, 0x03, 0xe0, 0x00, 0x80, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*12*/  { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	  0x01, 0xc0, 0x03, 0xe0, 0x07, 0xf0, 0x07, 0xf0,
	  0x07, 0xf0, 0x03, 0xe0, 0x01, 0xc0, 0x00, 0x00,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*13*/  { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xc0,
	  0x07, 0xf0, 0x07, 0xf0, 0x0f, 0xf8, 0x0f, 0xf8,
	  0x0f, 0xf8, 0x07, 0xf0, 0x07, 0xf0, 0x01, 0xc0,
	  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},

/*14*/  { 0x00, 0x00, 0x01, 0xc0, 0x07, 0xf0, 0x0f, 0xf8,
	  0x1f, 0xfc, 0x1f, 0xfc, 0x3f, 0xfe, 0x3f, 0xfe,
	  0x3f, 0xfe, 0x1f, 0xfc, 0x1f, 0xfc, 0x0f, 0xf8,
	  0x07, 0xf0, 0x01, 0xc0, 0x00, 0x00, 0x00, 0x00},

/*15*/  { 0x01, 0xc0, 0x03, 0xf0, 0x1f, 0xfc, 0x1f, 0xfc,
	  0x3f, 0xfe, 0x3f, 0xfe, 0x7f, 0xff, 0x7f, 0xff,
	  0x7f, 0xff, 0x3f, 0xfe, 0x3f, 0xfe, 0x1f, 0xfc,
	  0x1f, 0xfc, 0x03, 0xf0, 0x01, 0xc0, 0x00, 0x00}
};

syminit()
{
	int i,j,k,ssize,sym,ocnt;
	static flag = 0;
	char obyte,*inp,*outp;

	outp = &bbuf[32*MSIZE];

	for (ssize=2; ssize<=MSIZE; ssize++)
	    for (sym=0; sym<NSYM; sym++) {
			inp = &symbol[sym][0];
			ocnt=0; obyte=0;
			for (i=0; i<32; i++) {
				for (j=0; j<8; j++) {
					for (k=0; k<ssize; k++) {
						obyte |= ((*inp<<j) & 0x80) >> 7;
						if (ocnt++ == 7) {
							*outp++ = obyte;
							obyte = 0;
							ocnt = 0;
						}
						obyte <<= 1;
					}
				}
				inp++;
			}
	    }
}

int start[] = {
	MSIZE*32,
	(MSIZE+2*NSYM)*32,
	(MSIZE+(2+3)*NSYM)*32,
	(MSIZE+(2+3+4)*NSYM)*32
};

getsym()
{
	int off,tsp1,i,nlines,offset;

	tsp1 = 2*size+1;
	off = 8*size;
	nlines = 16*size;
	offset = -((across-off)&7);

	for (i=0; i<nlines; i++) {
		if (size==1)
		    stuff(&symbol[angle][2*i],
				tsp1-1,
				bbuf,
				offset);
		else
		    stuff(&bbuf[start[size-2]+angle*size*32]+(i/size)*(tsp1-1),
				tsp1-1,
				bbuf,
				offset);
	}
}

int mathflag = 0;
int gflag = 0;

char greek[128] = {
	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /* NUL  - BEL */
	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /*  BS  -  SI */
	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /* DLE  - ETB */
	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /* CAN  - US  */
	0040, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /* ' '  - ''' */
	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,    /* '('  - '/' */
	0060, 0061, 0062, 0063, 0064, 0065, 0066, 0067,    /* '0'  - '7' */
	0070, 0071, 0000, 0000, 0000, 0000, 0000, 0000,    /* '8'  - '?' */
	0000, 0101, 0102, 0116, 0104, 0105, 0125, 0103,    /* '@'  - 'G' */
	0110, 0111, 0000, 0112, 0113, 0114, 0115, 0117,    /* 'H'  - 'O' */
	0120, 0127, 0121, 0122, 0123, 0124, 0000, 0130,    /* 'P'  - 'W' */
	0126, 0107, 0000, 0000, 0000, 0000, 0000, 0000,    /* 'X'  - '_' */
	0000, 0141, 0142, 0156, 0144, 0145, 0165, 0143,    /* '`'  - 'g' */
	0150, 0151, 0000, 0152, 0153, 0154, 0155, 0157,    /* 'h'  - 'o' */
	0160, 0167, 0161, 0162, 0163, 0164, 0000, 0170,    /* 'p'  - 'w' */
	0166, 0147, 0000, 0000, 0000, 0000, 0000, 0000     /* 'x'  - DEL */
};

char math[128] = {
	0000, 0001, 0002, 0003, 0004, 0005, 0006, 0007,    /* NUL - BEL */
	0010, 0011, 0012, 0013, 0014, 0015, 0016, 0017,    /* BS  - SI  */
	0020, 0021, 0022, 0023, 0024, 0025, 0026, 0027,    /* DLE - ETB */
	0030, 0031, 0032, 0033, 0034, 0035, 0036, 0037,    /* CAN - US  */
	0040, 0041, 0042, 0043, 0044, 0045, 0046, 0047,    /* ' ' - ''' */
	0050, 0051, 0071, 0053, 0054, 0055, 0056, 0016,    /* '(' - '/' */
	0060, 0061, 0062, 0063, 0064, 0065, 0066, 0067,    /* '0' - '7' */
	0070, 0071, 0072, 0073, 0074, 0075, 0076, 0077,    /* '8' - '?' */
	0100, 0001, 0002, 0003, 0005, 0006, 0007, 0010,    /* '@' - 'G' */
	0011, 0012, 0013, 0014, 0015, 0032, 0033, 0034,    /* 'H' - 'O' */
	0035, 0036, 0037, 0050, 0052, 0057, 0060, 0061,    /* 'P' - 'W' */
	0063, 0064, 0065, 0133, 0134, 0135, 0136, 0137,    /* 'X' - '_' */
	0140, 0066, 0067, 0070, 0100, 0131, 0132, 0133,    /* '`' - 'g' */
	0134, 0135, 0171, 0172, 0173, 0175, 0156, 0157,    /* 'h' - 'o' */
	0160, 0161, 0162, 0163, 0164, 1065, 0166, 0167,    /* 'p' - 'w' */
	0170, 0171, 0172, 0173, 0174, 0175, 0176, 0177     /* 'x' - DEL */
};


plotchar(ch)
char ch[];
{
	logerr("plotchar: %s\n", ch);

	while (*ch != '\0') {
		switch (*ch) {
		case '-':
			*ch = '\004';
			break;
		case '!':
			if (ctype == 'B') {
				gflag = 1;
				oldtype = ctype;
				getfont(cfont,cangle,'S');
			} else {
				gflag = 0;
				getfont(cfont,cangle,oldtype);
			}
			ch++;
			continue;
		case '|':
			if (ctype == 'B') {
				oldtype = ctype;
				getfont(cfont,cangle,'S');
				mathflag = 1;
			} else {
				mathflag = 0;
				getfont(cfont,cangle,oldtype);
			}
			ch++;
			continue;
		case '\\':
			ch--;
			across -= dispatch[*ch].width;
			ch += 2;
			continue;
		case '@':
			smode = 0;
			getfont(cfont+3,cangle,ctype);
			ch++;
			continue;
		case '{':
			smode = -1;
			getfont(cfont-3,cangle,ctype);
			ch++;
			continue;
		case '}':
			smode = 1;
			getfont(cfont-3,cangle,ctype);
			ch++;
			continue;
		}
		if (mathflag == 1)
			*ch = math[*ch];
		else if (gflag == 1)
			*ch = greek[*ch];
		readchar(*ch);
		if (ctype != 'B' && *ch != ' ') {
			down -= smode*cfont*2;
			ptchar(*ch);
			across -= dispatch[*ch].width-1;
		}
		down -= smode*cfont*2;
		ptchar(*ch++);
	}
}

#define D dispatch[ch]

readchar(ch)
char ch;
{
	long i,ii;

	i =	(long) sizeof header
		+ (long) sizeof dispatch
		+ (long) D.addr;

	logerr("readchar: seek to %d\n", i);

	if (i != lseek(font, i, 0)) {
		logerr("seek error on character %o\n",&ch);
		exit(8);
	}
	i = read(font, cbuf, (int) D.nbytes);
	if (i != D.nbytes) {
		logerr("read error on character %o\n",&ch);
		exit(9);
	}
	if (ch == '.') {
		for (i=0; i<D.nbytes; i++) {
			logerr("readchar: %02x\n", cbuf[i]);
		}
	}
}


ptchar(ch)
char ch;
{
	int nlines,loc_down = down;
	register offset,i,j,llen;

	logerr("ptchar: %c\n", ch);
	logerr("      : left=%d right=%d\n", D.left, D.right);
	logerr("      : up=%d   down=%d\n", D.up, D.down);

	llen = (D.left+D.right+7)/8;
	nlines = D.up+D.down;
	offset = -((across-D.left)&7);
	logerr("      : llen=%d nlines=%d\n", llen, nlines);
	logerr("      : offset = %d\n", offset);

	if (ch=='.') {
		for (i=0; i<nlines; i++) {
			logerr ("line %03d:\n", i);
			for (j=0; j<llen; j++) {
				logerr("        : %d %02x\n", j, cbuf[i*llen + j]);
			}
		}
	}
	for (i=0; i<nlines; i++) {
		stuff(&cbuf[i*llen],
			llen,
			&bbuf[(across-D.left)/8 + (loc_down-D.up)*WPL],
			offset);
		loc_down++;
	}
	across += D.width+2;
	if (ch == ' ') across += (int) 2.777*cfont;

}

plotrchar(ch)
char ch[];
{
	logerr("plotrchar: %s\n", ch);
	logerr("         : down=%d, across=%d\n", down, across);

	while (*ch != '\0') {
		switch (*ch) {
		case '-':
			*ch = '\004';
			break;
		case '!':
			if (ctype == 'B') {
				gflag = 1;
				oldtype = ctype;
				getfont(cfont,cangle,'S');
			} else {
				gflag = 0;
				getfont(cfont,cangle,oldtype);
			}
			ch++;
			continue;
		case '|':
			if (ctype == 'B') {
				oldtype = ctype;
				getfont(cfont,cangle,'S');
				mathflag = 1;
			} else {
				mathflag = 0;
				getfont(cfont,cangle,oldtype);
			}
			ch++;
			continue;
		case '\\':
			ch--;
			down += dispatch[*ch].width;
			ch += 2;
			continue;
		case '@':
			smode = 0;
			getfont(cfont+3,cangle,ctype);
			ch++;
			continue;
		case '{':
			smode = -1;
			getfont(cfont-3,cangle,ctype);
			ch++;
			continue;
		case '}':
			smode = 1;
			getfont(cfont-3,cangle,ctype);
			ch++;
			continue;
		}
		if (mathflag == 1 )
			*ch = math[*ch];
		else if (gflag == 1)
			*ch = greek[*ch];
		readchar(*ch);
		ptrchar(*ch);
		logerr("down = %d, subtract %d\n", down, dispatch[*ch].width+2);
		down -= dispatch[*ch].width+2;
		if (ctype != 'B' && *ch != ' ') {
			down += dispatch[*ch].width-1;
			readchar(*ch);
			ptrchar(*ch);
			down -= dispatch[*ch].width+2;
		}
		if (*ch++ == ' ') down -= (int) 2.777*cfont;
	}
}



char brtab[] = {0,8,4,12,2,10,6,14,1,9,5,11,3,13,7,15};

reverse(ch)
char ch;
{
	int i,nb,nl;
	char temp,*first,*last,brev();

	logerr("Reverse: %c\n", ch);
	logerr("       : left=%d right=%d\n", D.left, D.right);
	logerr("       : up=%d   down=%d\n", D.up, D.down);
	nb = (D.up+D.down+7)/8;
	nl = D.left+D.right;
	logerr("       : nb,nl= %d,%d\n", nb, nl);

	for (i=0; i<nl; i++) {
		first = &cbuf[i*nb];
		last = &cbuf[(i+1)*nb-1];
		while (first < last) {
			temp = brev(*first);
			*first = brev(*last);
			*last = temp;
			first++;
			last--;
		}
		if (first == last)
			*first = brev(*first);
	}
}

char brev(ch)
char ch;
{
	return(brtab[ch&017]<<4 | brtab[(ch>>4)&017]);
}

ptrchar(ch)
char ch;
{
	int nlines; register offset,i,llen;

	reverse(ch);

	logerr("ptrchar: %c\n", ch);
	logerr("       : left=%d right=%d\n", D.left, D.right);
	logerr("       : up=%d   down=%d\n", D.up, D.down);

	nlines = D.left+D.right;
	llen = (D.up+D.down+7)/8;
	offset = -((D.up+2)&7);

	for (i=0; i<nlines; i++) {
		stuff(&cbuf[(nlines-i-1)*llen],
			llen,
			&bbuf[(down-(nlines-i-1))*WPL+across],
			offset);
	}
}
