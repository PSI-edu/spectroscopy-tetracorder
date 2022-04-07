.\" use ms macros
.\" this header is for putting the page numbers at the bottom of the page
.\" set line length, page offset, vertical spacing
.nr LL 6.3i
.ll 6.3i
.nr PO 1.0i
.po 1.0i
.nr VS 12
.vs 12
.\" set header and footer margins (default is 1.0i)
.nr FM 1.00i
.fm 1.00i
.nr HM 1.00i
.hm 1.00i
.\" set no date here, MUST be before CF definition, because ND sets CF
.\" set date, so don't do .ND
.\" LH CH RH are left, center, right header strings
.\" LF CF RF are left, center, right footer strings
.\" default is CH is defined as: .ds CH "\(hy \\n(PN \(hy
.ds LH "SPECPR Users' Manual
.ds CH
.ds RH "Page B.\\n(PN
.\" setup complete
.nf
SPECPR Users' Manual                                                   Page B.1













.ce
.B
APPENDIX B
.R





.ce
.B
HOW TO OBTAIN SPECPR
.R




.fi
.bp
.B
HOW TO OBTAIN SPECPR
.R
.PP
The specpr software, other software and spectral libraries are published
as a series of USGS Open File Reports.  The hardcopy spectral library,
and users manuals are available from:

.nf
.in 15n
USGS/Dept. of the Interior
Books and Open-File Reports Section
U.S. Geological Survey
Box 25425, Federal Center
Denver, CO  80225


USGS Books, Open File Reports and Maps:
     Phone number: (303) 236-7476
.in 0n
.fi


The relevant Open-File documents are:

.nf
.in 15n
93-595 SPECtrum Processing Routines Users Manual Version 3 (Program
       SPECPR), 202 pages, (this document).  (Clark, 1993)

93-592 Spectral Library paper version, 1326 pages.
       Clark et al, 1993).  This document will also
       be published as a USGS Bulletin.  See below
       for digital data.

93-594 Spsearch Users Manual, 23 pages.
       (Gorelick and Clark, 1993)

93-593 Spview manual, approximately 15 pages.
       (Includes the digital spectral library and
       spectral library reader software on 3.5-inch
       floppy disks for IBM-PC compatible computers.)
       (Livo et al., 1993)
.in 0n
.fi


The specpr software, related programs, the digital data for the USGS
spectral library and digital copies of the above manuals are available
via anonymous ftp on the internet:

.nf
.in 15n
ftp speclab.cr.usgs.gov

login as anonymous
password is your userid@machine

get README    (Note: examine this file for what is available.)

cd specpr
binary        (set binary mode for copy)
mget *

              (In order to get the USGS Digital Spectral Library:)
              (note this is cd ../pub/spectral.library from the
                    specpr directory)

cd pub/spectral.library

get README

.fi
Follow instructions in the README file for obtaining the data.  The
pub/spectral.library directory will contain all the different versions
listed in Table 5, as well as additional ones as they become available
(again see the README file for details).

Similarly, obtain the spsearch software in the pub/specpr and
pub/spsearch directories.  The specpr distribution also includes an
independent Fortran program, spprint, that reads a specpr format file
and prints titles.  For independent subroutines in C that read a specpr
file, see the README file.

.in 0n

After you have retrieved specpr, or the library, please send mail to
rclark@speclab.cr.usgs.gov with your name, address, phone number and
email address.  We will put you on a mailing list for future
announcements and updates.

.fi
.PP
Alternatively, contact the author at his address, or send
electronic (internet) mail to:

.in 15n
.nf
rclark@speclab.cr.usgs.gov
.in 0n

.fi
if you have questions.

