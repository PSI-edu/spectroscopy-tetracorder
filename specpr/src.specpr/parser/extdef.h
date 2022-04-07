#define DELETED delete_

extern short numchans;					/* number of channels */
extern short error;						/* do error comp flag */
extern struct specrec *stack[50];       /* calculation stack */
extern struct specrec *estack[50];      /* error stack */
extern short tos;                       /* top of stack */
extern char equation[296];              /* equation for manual history */
extern char *lexeqp;                    /* pointer to next char of equation */
extern char inputline[];				/* input buffer */
extern short nextc;						/* next char position in inputline */
extern float delete_;					/* value of deleted points */

extern struct specrec label1_;			/* label1 common */
extern struct blank _BLNK__;			/* blank common */
extern struct lbl4 lbl4_;				/* lbl4 common */
extern struct lbl3 lbl3_;				/* lbl3 common */

#define POP if (ERRORS) free(estack[tos]);              \
            if (NOS->title[0]=='\0')                    \
                for(i=0;i<512;i++)                      \
                        NOS->title[i] = TOS->title[i];  \
            free(stack[tos--]);         /* remove tos after calculation */
#define TOS stack[tos]
#define NOS stack[tos-1]
#define TOES estack[tos]
#define NOES estack[tos-1]
#define ERRORS error=='e'	/* this used to read ' e' ??? (nsg) */
#define TRUE 1
#define FALSE 0
#define READ 0
#define WRITE 1
#define blank_	_BLNK__

typedef struct record {
        char filename;
        int recno;
} RECORD;
