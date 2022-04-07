/******************************************************************************
Modifications:
  - K. Eric Livo       10 March 2013  (modification of Feb.23,2004 sourcecode)
    Conversion of data (byte swaps) to run on Intel CPUs.
     {specpr data storage byte order: Motorola (HP), ASD-FR byte order (Intel)

    Note: all input HEADER data in this file is byte-swapped (if required) for use on the
     current cpu architecture.  Specpr structured HEADER data is byteswapped (if
     required) during the write to the 'spdxxxx' file within io_specpr.c.  Specio.c
     and io_specpr.c default for use on Intel CPUs (Little Endian: low-order byte
     stored in memory at the lowest address) unless the #define BIGENDIANBO
     is uncommented (near the top of this file).

    All spectral data is byte-swapped to Big Endian format in this 'specio.c' file.

    SPECPR file (header and spectral) data are stored in a Big Endian format.  ASD Field
     Spectrometer file data are stored in Little Endian (Intel-PC) format.


  - Raymond F. Kokaly  23 Feb 2004
    Altered code to include byteswapping switch for linux.

  - Raymond F. Kokaly  02 May 1997
    Additions made to the information that is printed to the specpr manual
     history (instr_num,cal_num,splice1,splice2,nsamples).
    Additional fields in the specpr record are also filled in (revs,nruns,
     iwtrns,itimch,xnrm,scatim,timint).    
******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include "specio.h"
#include "io_specpr.h"

// Uncomment following 'define' line for use on unix workstations (HP, Sun-SPARC, etc)
// #define BIGENDIANBO

#ifdef BIGENDIANBO

// Big Endian Byte Order (UNIX Workstation) CPUs
void 
swap2(void *s1, void *t1)
{
    char *s = s1;
    char *t = t1;
    t[0] = s[1];
    t[1] = s[0];
}

void 
swap4(void *s1, void *t1)
{
    char *s = s1;
    char *t = t1;
    t[0] = s[3];
    t[1] = s[2];
    t[2] = s[1];
    t[3] = s[0];
}

#else

// Little Endian Byte Order (Intel) CPUs
void 
swap2(void *s1, void *t1)
{
    char *s = s1;
    char *t = t1;
    t[0] = s[0];
    t[1] = s[1];
}

void 
swap4(void *s1, void *t1)
{
    char *s = s1;
    char *t = t1;
    t[0] = s[0];
    t[1] = s[1];
    t[2] = s[2];
    t[3] = s[3];
}
#endif /* BIGENDIANBO */

// pad end of string-line with spaces, after removing new-lines and carriage returns
void lnspaces (char strline[], int imaxlen)
{
    int i;
    int j;
    for (i = 0; i < imaxlen; i++) {
        if (strline[i] == '\n')
            strline[i] = '\0';
        if (strline[i] == '\r')
            strline[i] = '\0';
    }
    i = (int)strlen(strline);
    if (i < imaxlen) {
        for (j = i; j < imaxlen; j++) {
            strline[j] = ' ';
        }
    }
}

static char *usage  = "\
usage: %s infile outfile [-na | -av | -w | -wav]\n\
Options:\n\
     -na:  no spectral average  (default)                (2151 channels)\n\
     -av:  spectral average every 3 channels             ( 717 channels)\n\
     -w:   output ASD fieldspectrometer wavelengths only (2151 channels)\n\
     -wav: output averaged ASD fieldspectrometer wavelengths only (717chan)\n\
";

main(int argc, char *argv[])
{
    char buf[484];
    int ifd, ofd;
    float scan_time = 0.1;
    float f1, f2, splice1, splice2;
    short g1, g2, o1, o2, instr_num, cal_num, nsamples;
    int itime;

    // unsigned int g1, g2, o1, o2, instr_num;
    // unsigned int cal_num, nsamples;
    // unsigned long itime;

    int average = 1;
    char *prog = argv[0];
    struct _label label;
    float f;
    int count;
    char *infile, *outfile;
    int i, j;
    float *fdata;
    struct tm t2;
    struct tm_s t;
    int tval;
    int waves = 0;
    char *ptr;
    int imaxlen;
    char gps_data[56];


//
//test for program arguments
//
    if (argc < 3) {
		fprintf(stderr, usage, prog);
        exit(1);
    }

    infile = argv[1];
    outfile = argv[2];

    if (argc > 3) {
        for (i = 3; i < argc; i++) {
            if (!strcmp(argv[i], "-na"))
                average = 1;                    // default is 1 (no average)
            else if (!strcmp(argv[i], "-av"))
                average = 3;
            else if (!strcmp(argv[i], "-w"))
                waves = 1;                      // default is 0 (no wavelengths)
            else if (!strcmp(argv[i], "-wav")) {
                waves = 1;                      // default is 0
                average = 3;                    // default is 1
                }
            else {
                fprintf(stderr, "%s: unrecognized argument: %s\n", prog, argv[i]);
            }
        }
    }

//
// open files
//
    if ((ifd = open(infile, O_RDONLY)) < 0) {
        fprintf(stderr, "%s: Unable to open input file: %s\n", prog, infile);
        exit(1);
    }
    if ((ofd = open(outfile, O_RDWR | O_CREAT, 0777)) < 0) {
        fprintf(stderr, "%s: Unable to open output file: %s\n", prog, outfile);
        exit(1);
    }

//
// read ASD data header (484 bytes)
//
    read(ifd, buf, 484);
    //printf("buf = %s\n", buf);

    if (!waves) fprintf(stderr, "%s\n", buf + COMMENTS);

//
// parse ASD header for key values
//

//  FOR Motorola [Speclab1 HPUX] byte swap, data collected on Intel Windows computer)
//     no swap performed when code is running on Intel CPUs
    swap2(buf + WHEN + 0, &t.tm_sec);
    swap2(buf + WHEN + 2, &t.tm_min);
    swap2(buf + WHEN + 4, &t.tm_hour);
    swap2(buf + WHEN + 6, &t.tm_mday);
    swap2(buf + WHEN + 8, &t.tm_mon);
    swap2(buf + WHEN + 10, &t.tm_year);
    swap4(buf + CH1_WAVEL, &f1);
    swap4(buf + WAVEL_STEP, &f2);
    f1 /= 1000;
    f2 /= 1000;
    swap2(buf + CALIBRATION, &cal_num);
    swap2(buf + INSTRUMENT_NUM, &instr_num);
    swap4(buf + IT, &itime);
    swap2(buf + SWIR1_GAIN, &g1);
    swap2(buf + SWIR2_GAIN, &g2);
    swap2(buf + SWIR1_OFFSET, &o1);
    swap2(buf + SWIR2_OFFSET, &o2);
    swap4(buf + SPLICE1_WAVELENGTH, &splice1);
    swap4(buf + SPLICE2_WAVELENGTH, &splice2);
    splice1 /= 1000;
    splice2 /= 1000;
    swap2(buf + SAMPLE_COUNT, &nsamples);


/*  NEEDS DEBUGGED  - to replace swap above for Intel CPUs (var. types still wrong)
// For Intel CPUs
    t.tm_sec = *(buf + WHEN + 0);
    t.tm_min = *(buf + WHEN + 2);
    t.tm_hour = *(buf + WHEN + 4);
    t.tm_mday = *(buf + WHEN + 6);
    t.tm_mon = *(buf + WHEN + 8);
    t.tm_year = *(buf + WHEN + 10);
    f1 = *(buf + CH1_WAVEL);				//float  (4 bytes)
    f2 = *(buf + WAVEL_STEP);				//float
    // f1 /= 1000;
    // f2 /= 1000;
    cal_num = *(buf + CALIBRATION);			//uint  (2 bytes)
    instr_num = *(buf + INSTRUMENT_NUM);		//uint
    itime = *(buf + IT);				//ulong  (4 bytes)
    g1 = *(buf + SWIR1_GAIN);				//uint
    g2 = *(buf + SWIR2_GAIN);				//uint
    o1 = *(buf + SWIR1_OFFSET);			//uint
    o2 = *(buf + SWIR2_OFFSET);			//uint
    splice1 = *(buf + SPLICE1_WAVELENGTH);	//float
    splice2 = *(buf + SPLICE2_WAVELENGTH);	//float
    // splice1 /= 1000;
    // splice2 /= 1000;
    nsamples = *(buf + SAMPLE_COUNT);		//unsigned (2 bytes)
*/

//
// display settings of program arguments and key data values
//
    if (waves) printf("wavelengths only output\n");
    printf ("channel numbers to average; set to: %d\n", average);

    // if (!waves) {
        fprintf(stderr, "%d:%d:%d %d/%d/%d\n",
                t.tm_hour,
                t.tm_min,
                t.tm_sec,
                t.tm_mon + 1,
                t.tm_mday,
                t.tm_year + 1900);
    // }

    // if (!waves) {
        fprintf(stderr,"ASD FR Field Spectrometer, Instrument: %d, Calibration: %d\n", instr_num, cal_num);

    if (!waves) {
        fprintf(stderr, "Start wavelength: %f, step: %f, Integration time: %d ms\n", f1, f2, itime);
        fprintf(stderr, "Detector 1, gain: %d, offset: %d, Detector 2, gain: %d, offset: %d\n", g1, o1, g2, o2);
        fprintf(stderr, "Splice 1: %f,  Splice 2: %f, Samples in Avg: %d\n", splice1, splice2, nsamples);
    }

//
// enter key (ASD) values into SPECPR-format structures (but NOT byte-swapped yet [if required])
//
printf("to GPS");






//
// Byte Swap spectral DATA here to Big Endian (SPECPR file) format
//   presently used for both Workstations and Intel PCs
//
//   HERE FOR INTEL CPUs only (see above for UNIX Workstations)
//
//#ifndef BIGENDIANBO
    for (i = 0; i < 2151; i++) {
        f = fdata[i];
//        byteswap4(fdata + i, &f);
        fdata[i] = f;
    }
//#endif /* BIGENDIANBO */

printf("to t2.tm");
        /**
         ** put in time values.
         **/
    t2.tm_sec = t.tm_sec;
    t2.tm_min = t.tm_min;
    t2.tm_hour = t.tm_hour;
    t2.tm_mday = t.tm_mday;
    t2.tm_mon = t.tm_mon;
    t2.tm_year = t.tm_year;
    t2.tm_isdst = t.tm_isdst;

    tval = mktime(&t2);
    set_julian_date(tval, &label.iscta, &label.jdatea);
    set_julian_date(tval, &label.isctb, &label.jdateb);

//
// write out spectral data to a SPECPR record
//
    //i = write_specpr(ofd, -1, &label, fdata);
    printf("Wrote record of %d channels to %s\n", (2151/average), outfile);

//
// close files and terminate program
//
    close(ifd);
    close(ofd);
}
