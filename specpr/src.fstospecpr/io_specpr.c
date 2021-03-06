#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "io_specpr.h"


//  KEL(March 2013) - NOTE: include <unistd.h> required by CYGWIN (lseek)


void 
byteswap2(void *s1, void *t1)
{
// byteswap two bytes
    char *s = s1;
    char *t = t1;
    t[0] = s[1];
    t[1] = s[0];
}

void 
byteswap4(void *s1, void *t1)
{
// byteswap four bytes
    char *s = s1;
    char *t = t1;
    t[0] = s[3];
    t[1] = s[2];
    t[2] = s[1];
    t[3] = s[0];
}

char *
decode_time(s,buf)
int s;
char *buf;
{
    int hour;
    int minutes;
    int sec;
    int tsec;

    
    tsec = s/24000;
    hour = tsec/3600;
    minutes = (tsec - hour*3600)/60;
	sec = tsec - hour*3600 - minutes*60;

    sprintf(buf, "%2.2d:%2.2d:%2.2d", hour, minutes, sec);
    buf[8] = '\0';
    return(buf);
}


char *
decode_date(jday,buf)
int jday;
char *buf;
{
    double ab,a,b,c,d,e,m,y,f;
    double day;
    int iday;

    day = (double)jday/10.0;
    iday = jday/10;
    jday = iday;

    f = day - (double)(iday) + 0.5;

    if (f >= 1.0){
        jday = jday + 1;
    }

    ab = floor((double)(jday/36524.25)-51.12264);
    a = (double)jday + 1.0 + ab - floor(ab/4.0);
    b = a + 1524.0;
    c = floor((b/365.25)-0.3343);
    d = floor(365.25*c);
    e = floor((b-d)/30.61);
    d = b - d - floor(30.61*e);
    m = e-1.0;
    y = c-4716.0;

    if (e > 13.5){
        m = m-12;
    }
    if (m < 2.5) {
        y=y+1;
    }
    if (y > 1900) y -= 1900;
    if (y < 0) y = 0;

    sprintf(buf, "%2.2d/%2.2d/%2.2d", (int) m, (int) d, (int) y);
    buf[8] = '\0';
    return(buf);
}

max_rec(fd)
int fd;
{
/*
    struct stat buf;
    stat(path, &buf);
*/
    int i;
    int f;
    f = dup(fd);
    i = lseek(f, 0, SEEK_END);
    close(f);
    return(i / LABELSIZE -1);
}

read_record(fd, i, label)
int fd;
int i;
char *label;
{
    if (lseek(fd, LABELSIZE * i, 0) == -1) {
        /* some error */
        return(-1);
    }
    if (read(fd, label, LABELSIZE) != LABELSIZE) {
        return(-2);
    }
    return(check_bit(((int*)label)[0], 0));
}

/**
 ** read_specpr() - read a specpr record.
 **
 ** returns: 	1 on success
 ** 			0 on failure (specified record is a continuation record)
 **/

read_specpr(fd, i, label, data)
int fd;
int i;
struct _label *label;
char **data;
{
    struct _label label2;
    int count = 0;
    int j;
    int size;
    struct _tlabel *tlabel;
    char *ptr;

    if ((j = read_record(fd, i, label)) == 0) {
        tlabel = (struct _tlabel *)label;
        if (check_bit(label->icflag,1)) {
            /* text */
            size = 1476;
            *data = (char *)malloc(size);
            memcpy(*data,tlabel->itext, size);
        } else {
            size = 256*sizeof(float);
            *data = (char *)malloc(size);
            memcpy(*data, label->data, size);
        }
        while(read_record(fd, ++i, &label2)==1) {
            count++;
            *data = (char *)realloc(*data,(size+count*1532));
            ptr = (char *)&label2;
            memcpy(*data+(size+(count-1)*1532), ((char *)&label2)+4, 1532);
        }

        return(count+1);
    }
    return((j == 1 ? 0 : j));
}


write_record(fd, i, label)
int fd;
int i;
struct _label *label;
{
    int iout;

    if (i < 0) {
        if ((i = lseek(fd, 0, SEEK_END)) == 0) {
            char buf[LABELSIZE];
            memset(buf, ' ', LABELSIZE);

            // define SPECPR header record (rec 0)
            strcpy(&buf[0],SPECPR_STAMP);

            write(fd, buf, LABELSIZE);
            i = lseek(fd, 0, SEEK_END);
        }
        i /= LABELSIZE;
    } else if (lseek(fd, LABELSIZE * i, 0) == -1) {
        /* some error */
        return(-1);
    }

    if (check_bit(label->icflag, 0) == 0 && check_bit(label->icflag,1) == 0) {
        label->irecno = i;
    }

#ifndef BIGENDIANBO
    // if NOT defined, then byteswap SPECPR record flags
    specpr_byteswap(label);
#endif /* BIGENDIANBO */

    if (write(fd, label, LABELSIZE) != LABELSIZE) {
        printf("SPECPR output write error\n");
        return(-2);
    }
    return(1);
}


specpr_open(path)
char *path;
{
    int fout;
    char *p;

    if ((fout = open(path, O_RDONLY)) >= 0) {
        close(fout);
        /* fout = open(path, O_WRONLY | O_CREAT | O_APPEND);   FAILS in 2020 - R. Clark */
        fout = open(path, O_WRONLY |  O_APPEND);
        return(fout);
    } else {
        /* it doesn't exist.  Create it. */
        /**
         ** this needs to prepend the SPECPR_MAGIC cookie to record 0
         **/

        /* we really shouldn't allow this -- the file should already exist.  R. Clark 01/2020 */

	printf ("specpr file does not exist, exit 1\n");   /* added 01/06/2020 RNC */
	exit (1);

        fout = open(path, O_WRONLY | O_CREAT | O_APPEND, 0777);
        if (fout < 0) return(fout);

        p = (char *)malloc(LABELSIZE);
        memset(p, '\0', LABELSIZE);
		memcpy(p, SPECPR_STAMP, strlen(SPECPR_STAMP));
        write(fout, p, LABELSIZE);
        free(p);
        return(fout);
    }
}


write_specpr(fd, i, label, data)
int fd;
int i;
struct _label *label;
char *data;
{
    int type;
    int chans;
    struct _tlabel *tlabel;
    struct _tlabel tmpl;
    int size;
    int offset;
    int count;

    tlabel = (struct _tlabel *)label;

    switch(check_bit(label->icflag,1)) {
        case 0:
            /* data */
            size = label->itchan*sizeof(float);
            offset = 256*sizeof(float);
            set_bit(tmpl.icflag,1,0);
            if (data != NULL) {
                memcpy(label->data, data, offset);
            }
            break;
        case 1:
            /* text */
            size = tlabel->itxtch;
            offset = 1476;
            set_bit(tmpl.icflag,1,1);
            if (data != NULL) {
                memcpy(tlabel->itext, data, offset);
            }
            break;
    }

// WRITE 1st record of SPECTRA
    write_record(fd, i, label);

// WRITE DATA continuation records of SPECTRA
    count = 0;
    size -= offset;
    // set_bit(tmpl.icflag,0,1);
    while(size > 0) {
        set_bit(tmpl.icflag,0,1);
        set_bit(tmpl.icflag,1,0);
        set_bit(tmpl.icflag,2,0);
        set_bit(tmpl.icflag,3,1);
        set_bit(tmpl.icflag,4,1);
        set_bit(tmpl.icflag,5,1);
        memcpy(((char *)&tmpl)+4, data+(offset+1532*count), 1532);
        write_record(fd, (i < 0 ? i : i+count+1), &tmpl);
        size -= 1532;
        count++;
    }
}

struct _label *
make_label(npixels, waves, title, ahist, mhist)
char *title;
char *ahist;
char *mhist;
int npixels,waves;
{
    int i;
    struct _label label, *lbl;

    label.icflag = 0;
    set_bit(label.icflag, 3, 1);
    set_bit(label.icflag, 4, 1);
    set_bit(label.icflag, 5, 1);

    sprintf(label.usernm,"%s",getenv("USER"));
    julian_date(&label.iscta,&label.jdatea);
    label.iscta = label.isctb = label.iscta * 24000;
    label.jdateb = label.jdatea;
    label.istb = 0;
    label.isra = label.isdec = 0;
    label.itchan = npixels;
    label.irmas = 0;
    label.revs = label.iband[0] = label.iband[1] = 1;
    label.irespt = label.itpntr = 0;
    label.siangl = label.seangl = label.sphase = 0;
    label.itimch = label.xnrm = label.scatin = label.timint = 1;
    label.tempd = 273;

    label.irwav = waves ;/* wavelengths pointer */

    label.irecno = 0; /* record number pointer */
    
    label.ihist[0] = '\0';
    label.mhist[0] = '\0';
    label.ititl[0] = '\0';

    if (title != NULL) 
        for (i = 0 ; i < 40 ; i++)
            label.ititl[i] = (i >= strlen(title) ? ' ' : title[i]);
    if (ahist != NULL) 
        for (i = 0 ; i < 60 ; i++)
            label.ihist[i] = (i >= strlen(ahist) ? ' ' : ahist[i]);
    if (mhist != NULL) 
        for (i = 0 ; i < 296 ; i++)
            label.mhist[i] = (i >= strlen(mhist) ? ' ' : mhist[i]);

    label.iwtrns = label.nruns = npixels; /* number of blocks averaged */
    label.data[0] = 0; /* averaged spectra data */

    lbl = (struct _label *)malloc(sizeof(struct _label));
    memcpy(lbl, &label, sizeof(struct _label));
    return(lbl);
}

julian_date(secs, date)
int *secs;
int *date;
{
/* sets date to Julian day * 10 and time to secs since 0:00 hours UT */
    int jda, jsec, nday, isec;

    jda = 24405875;
    jsec = time(0);

    nday = jsec/(3600*24);
    isec = jsec - (nday*3600*24);
    jda = jda+nday*10;

    *date = jda;
    *secs = isec*24000;
}

set_julian_date(itime, secs, date)
int itime;
int *secs;
int *date;
{
/* sets date to Julian day * 10 and time to secs since 0:00 hours UT */
    int jda, jsec, nday, isec;

    jda = 24405875;
    jsec = itime;

    nday = jsec/(3600*24);
    isec = jsec - (nday*3600*24);
    jda = jda+nday*10;

    *date = jda;
    *secs = isec*24000;
}

/**
 ** is_specpr() - detect the magic cookie for specpr files.
 ** returns:
 **		0: on failure
 **		1: on success
 **/
is_specpr(FILE *fp)
{
	int len;
	char buf[16];

	rewind(fp);
	len = fread(buf, 1, strlen(SPECPR_MAGIC), fp);
	return (len == strlen(SPECPR_MAGIC) && !strncmp(buf, SPECPR_MAGIC, len));
}

specpr_byteswap(label)
struct _label *label;
{
// byteswaps values within a SPEPCR structure
    int iout;
    int ibs;
    float ftmp;
    float fout;
    int offset;

// test for SPECPR data records (bit 1 = 0)
    if(check_bit(label->icflag,1) == 0) {

// test for SPECPR 1st data record verses data continuuation record
        switch(check_bit(label->icflag,0)) {
            case 0:
                /* 1st data record (1536 bytes) */

                // printf("reached specpr_byteswap: case 0\n");

//byteswap header values
                byteswap4(&label->icflag, &iout);
                    label->icflag = iout;
                byteswap4(&label->iscta, &iout);
                    label->iscta = iout;
                byteswap4(&label->isctb, &iout);
                    label->isctb = iout;
                byteswap4(&label->jdatea, &iout);
                    label->jdatea = iout;
                byteswap4(&label->jdateb, &iout);
                    label->jdateb = iout;
                byteswap4(&label->istb, &iout);
                    label->istb = iout;
                byteswap4(&label->isra, &iout);
                    label->isra = iout;
                byteswap4(&label->isdec, &iout);
                    label->isdec = iout;
                byteswap4(&label->itchan, &iout);
                    label->itchan = iout;
                byteswap4(&label->irmas, &iout);
                    label->irmas = iout;
                byteswap4(&label->revs, &iout);
                    label->revs = iout;
                byteswap4(&label->iband[0], &iout);
                    label->iband[0] = iout;
                byteswap4(&label->iband[1], &iout);
                    label->iband[1] = iout;
                byteswap4(&label->irwav, &iout);
                    label->irwav = iout;
                byteswap4(&label->irespt, &iout);
                    label->irespt = iout;
                byteswap4(&label->irecno, &iout);
                    label->irecno = iout;
                byteswap4(&label->itpntr, &iout);
                    label->itpntr = iout;
                byteswap4(&label->nruns, &iout);
                    label->nruns = iout;
                byteswap4(&label->siangl, &iout);
                    label->siangl = iout;
                byteswap4(&label->seangl, &iout);
                    label->seangl = iout;
                byteswap4(&label->sphase, &iout);
                    label->sphase = iout;
                byteswap4(&label->iwtrns, &iout);
                    label->iwtrns = iout;
                byteswap4(&label->itimch, &iout);
                    label->itimch = iout;
                byteswap4(&label->xnrm, &fout);
                    label->xnrm = fout;
                byteswap4(&label->scatin, &fout);
                    label->scatin = fout;
                byteswap4(&label->timint, &fout);
                    label->timint = fout;
                byteswap4(&label->tempd, &fout);
                    label->tempd = fout;

/* call to byteswap4 NOT USED here, Byte-swap occurs in specio.c
// this for loop works well
//byteswap data
                for (ibs = 0; ibs < 256; ibs++) {
                    ftmp = label->data[ibs];
                    byteswap4(&ftmp, &fout);
                    label->data[ibs] = fout;
                }
*/

                break;

            case 1:
                /* continuation data records (1536 bytes) */

                // printf("reached specpr_byteswap: case 1\n");

                byteswap4(&label->icflag, &iout);
                label->icflag = iout;

/* call to byteswap4 NOT USED here, byteswap occurs in specio.c
   //  Note: this byteswap loop is under construction still (not working)
   //  The 'tmp1' continuing record structure needs to be implimented here.
   // works on arrays of 256, but not 383 (out of bounds)
//byteswap data
                //for (ibs = 0; ibs < 383; ibs++) {  383 is required
                for (ibs = 0; ibs < 256; ibs++) {
                    ftmp = label->data[ibs];
                    byteswap4(&ftmp, &fout);
                    label->data[ibs] = fout;
                }
*/

                break;

        }

    }

}
