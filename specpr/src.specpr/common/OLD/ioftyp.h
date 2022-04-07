#*******************************************************************
#   TITLE:         3D FILE PARAMETER ARRAY COMMON BLOCK            *
#   PROGRAMMER:    Barry J. Middlebrook                            *
#------------------------------------------------------------------*
#   DESCRIPTION:                                                   *
#               This is the common statements for the 3-d file     *
#               reading routine.  All of the file parameters are   *
#               loaded into the arrays present here.  The array    *
#               <filtyp> contains 11 parameters describing the file *
#               characteristics.  <ftptr> is a pointer array for   *
#               the 11 parameters and other file attributes.  The  *
#               <filreq> array has the pixel coordinates and in-   *
#               dicates the extraction direction.                  *
#                                                                  *
#------------------------------------------------------------------*
#   VARIABLES:                                                     *
#            filtyp      - 12x5 array containing 5 sets of file    *
#                        parameters which are as follows:          *
#                                                                  *
#                     1   specpr file or not                       *
#                                         0 - specpr normal        *
#                                         3 - 3d data file         *
#                     2   file header length in bytes              *
#                     3   record length in bytes                   *
#                     4   record header length in bytes            *
#                     5   DN offset (integer)                      *
#                     6   x - dimension in pixels                  *
#                     7   y - dimension in pixels                  *
#                     8   z - dimension in pixels                  *
#                     9   data type (i2, i4, r4)                   *
#                                         1 - half word integer    *
#                                         2 - full word integer    *
#                                         3 - full word real       *
#                     10  file organization                        *
#                                         1 - BIL                  *
#                                         2 - BIP                  *
#                                         3 - BSQ                  *
#                     11  point deletion flag                      *
#                       * currently, AVIRIS deletion flag = 4096   *
#                                                                  *
#             ftptr      - pointer array for the above parameters  *
#                                                                  *
#             filreq     - coordinates of the pixel and extraction *
#                        direction for the spectra                 *
#                                                                  *
#             fiobox     - box size to extract (default=1)         *
#                                                                  *
#             dnscal     - data number (DN) scale array, this num- *
#                        ber is used as follows:                   *
#                        Reflectance = DNscale*(DN+DNoffset)       *
#                         (real*4)     (real*4)(i2+i4)             *
#                                                                  *
#             NOTE:  The following arrays are 2-d arrays with      *
#                    5 sets of data, one for each file             *
#             when     - integer*4 array with info on time         *
#                     1   Civil or Universal time in seconds       *
#                         scaled by 24000                          *
#                     2   Date in Julian Day number x 10           *
#                                                                  *
#             i4info   - integer*4 array with other pertinent info *
#                     1   Declination coordinate (set to 0)        *
#                     2   Number of channels extracted             *
#                     3   Equivalent atmospheric thickness         *
#                         (airmass x 1000)   (set to 0)            *
#                     4   Number of independent scans made to get  *
#                         spectra  (set to 1)                      *
#                     5   Channel number which defines band nor-   *
#                         malization (set to 1)                    *
#                     6   Record number of wavelength file (set to *
#                         1)                                       *
#                     7   Resolution record (set to 0)             *
#                     8   First record 3d file read encounters     *
#                     9   Text data record pointer (set to 0)      *
#                     10  Number of runs summed together to get    *
#                         resulting spectra (set to 1)             *
#                     11  Angle of incidence of illuminating rad-  *
#                         iation (set to 0)                        *
#                     12  Angle of emission of illuminating radia- *
#                         tion (set to 0)                          *
#                     13  Phase angle between 13 and 14 (set to 0) *
#                     14  Weighted number of runs (set to 1)       *
#                                                                  *
#             titl3d     - record title for extracted spectra      *
#                        to include the pixel coordinates          *
#                                                                  *
#             r4info     - real*4 value information comprised of   *
#                     1   Band normalization factor                *
#                     2   Time (in secs) for one entire spectrum   *
#                         scan                                     *
#                     3   Total integration time (in seconds)      *
#                     4   Temperature in degrees Kelvin            *
#                     5   Time observed in the sample beam for     *
#                         each half chop in milliseconds           *
#                                                                  *
#             autohs     - program automatic 60 character history  *
#                                                                  *
#             manhis     - manual history (4 lines of 74 charac-   *
#                        ters each                                 *
#                                                                  *
#*******************************************************************

#  Set variable type
	INTEGER*4 filtyp(12,5),ftptr(19),filreq(3),when(2,5),fiobox(3)
	REAL*4    dnscal(5)
	CHARACTER*24 titl3d(5)
	COMMON    /ioftyp/filtyp,ftptr,filreq,dnscal,when,titl3d,fiobox
