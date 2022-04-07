/*****************************************************************************
Header      :specio.h
Created By  :Steve Simms
Description :Header for ASD spectrum files
Modified    :
    21 Jan 93
        Modified all write_ routines so they accept overwrite mode.
    5 Feb 92    Simms
        Upgraded to v3.2 supporting axis mins and maxs, ip_numofbits
        and xmode. 
    NOTE! -- Modification 3 --
        This is the modification num of the specio package including:
        specio.* fileio.* and head.*. If ANY of these change, this
        modification number should be updated. Eventually these will 
        all be in a library with the "version" number idea being follow 
        through!

    10 Feb 93   Mike        -- Modification 4 --
        Changed scaling of integer-format reflectance files to keep
        4095 as 100% instead of 150%.
        
    17 Feb 93   Mike        -- Modification 5 --
        Removed code from head_Update so that when x and y scale limits
        are not known (ie, old PSII files), they are left all equal to 0.0.
        Thus, when a scale's difference (max-min) is 0.0, the limits weren't
        saved to the file.

    15 Apr 93   Mike
        Created file version 3.4 removing ratio_file[] field and replacing
        with expanded GPS structure. File version 3.3 contained a new 'flags'
        structure. Changed sample_count to an integer instead of a uchar.

    20 Sep 93   Mike    Header for version 1.06
        Removed IP.H and DETECTOR_SIZE dependencies

    Dec 1, 1993 Steve   
        Created file version 4.0 for fieldspec instruments
        Changed so now the size of the spectrum array is
        acquired from the header.
        Added the field - uchar instrument - and the define *_INSTRUMENT.
        Removed GPS_APP field.
            
    May 24 94 Steve v4.01
        Added absorbance and transmittance modes

Copyright (C) 1990. Analytical Spectral Devices (ASD).
*****************************************************************************/



#define uchar unsigned char
#define ushort unsigned short
#define ulong unsigned long
#define time_t long

struct tm_s {
    short tm_sec;       /* second (0-61, allows for leap seconds) */
    short tm_min;       /* minute (0-59) */
    short tm_hour;      /* hour (0-23) */
    short tm_mday;      /* day of the month (1-31) */
    short tm_mon;       /* month (0-11) */
    short tm_year;      /* years since 1900 */
    short tm_wday;      /* day of the week (0-6) */
    short tm_yday;      /* day of the year (0-365) */
    short tm_isdst;     /* non-0 if daylight savings time is in effect */
};


#define NO_APP      0           /*  No application information stored */
#define BANDS_APP   1           /* bands application */
#define CI_APP      2           /* colorimetry application */
#define CALIB_APP   3           /* calibration application */

typedef struct color_data {
    float nx2, nx10;
    float ny2, ny10;
    float u_prime_2, u_prime_10;
    float v_prime_2, v_prime_10;
    char ill_file[12];
} COLOR_DATA;

typedef struct band_data {
    float bands[8];
} BAND_DATA;

typedef struct cal_data {
    char nothing;
} CAL_DATA;

    /** a union takes on the size of its largest element.  This filler
        makes sure that we have room for the other structure to grow. **/
typedef struct {
    char data[128];
} FILLER;

    /* and this is the union to create one generic name for the application-
       specific data */
typedef union {
    COLOR_DATA ci_data;
    BAND_DATA  band_data;
    CAL_DATA   cal_data;
    FILLER     filler;
} APP_DATA;

#define COMMENT_SIZE 157        /*  length of the comment field */

typedef struct sp_hdr {         /*  ASD spectrum header description  */
    char    co[3];              /*  Three character company name  */
    char    comments[ COMMENT_SIZE];    /*  comment field  */
    struct  tm_s when;            /*  time when spectrum was saved  */
    uchar   program_version;    /*  version of the program which created  */
                                /*  this file. major version in upper nibble  */
                                /*  minor version in lower     */
    uchar   file_version;       /*  spectrum file format version  */
    uchar   itime;              /*  Not used after v2.00  */
    uchar   dc_corr;            /*  1 if DC subtracted, 0 if not  */
    time_t  dc_time;            /*  Time of when last dc was taken  */
    uchar   data_type;          /*  see *_TYPE below  */
    time_t  ref_time;           /*  Time of when last wr was take  */
    float   ch1_wavel;          /*  calibrated starting wavelength in nm  */
    float   wavel_step;         /*  calibrated wavelength step in nm  */
    uchar   data_format;        /*  format of spectrum. See *_FORMAT  */
    uchar   old_dc_count;       /*  Num of DC measurements in the avg */
    uchar   old_ref_count;      /*  Num of WR in the average */
    uchar   old_sample_count;   /*  Num of spec samples in the avg */
    uchar   application;        /*  Which application created APP_DATA */
    ushort  channels;           /*  Num of channels in the detector  */
    APP_DATA  app_data;         /*  Application-specific data */
    char    gps_data[56];       /*  GPS position, course, etc. */
    ulong   it;                 /*  The actual integration time in ms  */
    short   fo;                 /*  The fo attachment's view in degrees  */
    short   dcc;                /*  The dark current correction value  */
    ushort  calibration;        /*  calibration series  */
    ushort  instrument_num;     /*  instrument number  */
    float   ymin;               /*  setting of the y axis' min value  */
    float   ymax;               /*  setting of the y axis' max value  */
    float   xmin;               /*  setting of the x axis' min value  */
    float   xmax;               /*  setting of the x axis' max value  */
    ushort  ip_numbits;         /*  instrument's dynamic range    */
    uchar   xmode;              /*  x axis mode. See *_XMODE  */
    uchar   flags[4];           /*  Flags (0 = AVGFIX'ed)  */
    ushort  dc_count;           /*  Num of DC measurements in the avg */
    ushort  ref_count;          /*  Num of WR in the average */
    ushort  sample_count;       /*  Num of spec samples in the avg */
    uchar   instrument;         /*  Instrument type. See defs below  */
    ulong   bulb;               /*  The id number of the cal bulb  */
    ushort  swir1_gain;         /*  gain setting for swir 1  */
    ushort  swir2_gain;         /*  gain setting for swir 2  */
    ushort  swir1_offset;       /*  offset setting for swir 1   */
    ushort  swir2_offset;       /*  offset setting for swir 2  */
    float   splice1_wavelength; /*  wavelength of VNIR and SWIR1 splice   */
    float   splice2_wavelength; /*  wavelength of SWIR1 and SWIR2 splice  */
    uchar   spare[32];          /*  fill to 484 uchars  */
} HDR;

#define BASE				0
#define CO                  BASE                +0
#define COMMENTS            CO                  +3
#define WHEN                COMMENTS            +157
#define PROGRAM_VERSION     WHEN                +18
#define FILE_VERSION        PROGRAM_VERSION     +1
#define ITIME               FILE_VERSION        +1
#define DC_CORR             ITIME               +1
#define DC_TIME             DC_CORR             +1
#define DATA_TYPE           DC_TIME             +4
#define REF_TIME            DATA_TYPE           +1
#define CH1_WAVEL           REF_TIME            +4
#define WAVEL_STEP          CH1_WAVEL           +4
#define DATA_FORMAT         WAVEL_STEP          +4
#define OLD_DC_COUNT        DATA_FORMAT         +1
#define OLD_REF_COUNT       OLD_DC_COUNT        +1
#define OLD_SAMPLE_COUNT    OLD_REF_COUNT       +1
#define APPLICATION         OLD_SAMPLE_COUNT    +1
#define CHANNELS            APPLICATION         +1
#define APP_DATA            CHANNELS            +2
#define GPS_DATA            APP_DATA            +128
#define IT                  GPS_DATA            +56
#define FO                  IT                  +4
#define DCC                 FO                  +2
#define CALIBRATION         DCC                 +2
#define INSTRUMENT_NUM      CALIBRATION         +2
#define YMIN                INSTRUMENT_NUM      +2
#define YMAX                YMIN                +4
#define XMIN                YMAX                +4
#define XMAX                XMIN                +4
#define IP_NUMBITS          XMAX                +4
#define XMODE               IP_NUMBITS          +2
#define FLAGS               XMODE               +1
#define DC_COUNT            FLAGS               +4
#define REF_COUNT           DC_COUNT            +2
#define SAMPLE_COUNT        REF_COUNT           +2
#define INSTRUMENT          SAMPLE_COUNT        +2
#define BULB                INSTRUMENT          +1
#define SWIR1_GAIN          BULB                +4
#define SWIR2_GAIN          SWIR1_GAIN          +2
#define SWIR1_OFFSET        SWIR2_GAIN          +2
#define SWIR2_OFFSET        SWIR1_OFFSET        +2
#define SPLICE1_WAVELENGTH  SWIR2_OFFSET        +2
#define SPLICE2_WAVELENGTH  SPLICE1_WAVELENGTH  +4
#define SPARE               SPLICE2_WAVELENGTH  +4
#define END                 SPARE               +32


                                /*  THE INSTRUMENT TYPE THAT CREATED THE SPEC */
#define UNKNOWN_INSTRUMENT      (uchar)0
#define PSII_INSTRUMENT         (uchar)1
#define LSVNIR_INSTRUMENT       (uchar)2
#define FSVNIR_INSTRUMENT       (uchar)3
#define FSFR_INSTRUMENT         (uchar)4
#define FSNIR_INSTRUMENT        (uchar)5

                                /*  available format of the save spectrum */
#define FLOAT_FORMAT    (uchar)0
#define INTEGER_FORMAT  (uchar)1
#define DOUBLE_FORMAT   (uchar)2
#define UNKNOWN_FORMAT  (uchar)3
                                /*  The types of spectrum that can be calc'ed */
#define RAW_TYPE        (uchar)0
#define REF_TYPE        (uchar)1
#define RAD_TYPE        (uchar)2
#define NOUNITS_TYPE    (uchar)3
#define IRRAD_TYPE      (uchar)4
#define QI_TYPE         (uchar)5
#define TRANS_TYPE      (uchar)6
#define UNKNOWN_TYPE    (uchar)7
#define ABS_TYPE        (uchar)8

#define WAVELENGTH_XMODE    (uchar)0
#define CHANNEL_XMODE       (uchar)1
#define UNKNOWN_XMODE       (uchar)2

