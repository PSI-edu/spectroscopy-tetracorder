	subroutine rstbgn
	implicit integer*4 (i-n)

#ccc  name: rstbgn
#ccc  version date: 
#ccc  author(s): Barry Middlebrook
#ccc  language: ratfor
#ccc
#ccc  short description: do a restart and begin stuff for specpr
#ccc			related programs.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#ccc****************************************************************
#cccTITLE:        SPECPR RESTART FILE INIT MODULE                  *
#ccc                                                               *
#cccPROGRAMMER:   Barry J. Middlebrook                             *
#ccc              USGS Branch of Geophysics                        *
#ccc              Denver West Offices                              *
#ccc                                                               *
#ccc---------------------------------------------------------------*
#cccDESCRIPTION:                                                   *
#ccc            This is a specpr restart module set up for use     *
#ccc            with programs using specpr restart files.  The     *
#ccc            prerequisites for using are:                       *
#ccc                                                               *
#ccc            1)  Include these files in the                     *
#ccc         ../specpr/src.specpr/common directory:                 *
#ccc                cmdarg                                         *
#ccc                cmd                                            *
#ccc            2)  Program must have the statements:              *
#ccc                #HPUX 	PROGRAM mypg (ach1,ach2,ach3)      *
#ccc                #HPUX	CHARACTER*80    ach1,ach2,ach3     *
#ccc                #HPUX	charg1 = arg1                      *
#ccc                #HPUX	charg2 = arg2                      *
#ccc                #HPUX	charg3 = arg3                      *
#ccc                            CALL getcmdargs                    *
#ccc                            CALL rstmod                        *
#ccc                NOTE:  Everything must be commented out ex-    *
#ccc                cept for the two subroutine calls for com-     *
#ccc                patibility with Sun computers.                 *
#ccc                                                               *
#ccc---------------------------------------------------------------*

#  Set variable type
	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/filenames"
	include 	"../specpr/src.specpr/common/ioftyp"

	character*1536 dummy
 	equivalence (dummy,ititl)
	character*80 ach1, ach2, ach3
	integer*4 fsize
	integer*4 recnum
	integer*4 idummy, filsiz, icrst
	integer*2 chkbit,ibit,bitnum
	real*4 param(18),pturb,fitcri,stndev,min,num
	character*40 xntitl(9), xktitl(9), xititl
	character*8 file1,namwav
	character*1 iform
	integer*4 iargc, nn, ilen, idlt(512)
 
###???	include "lmrefl.h"
###???	include "convh.h"
 
	maxrec = 999999
	maxchn = 4852
	maxtxt = 19860
	iline=0

#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .false.
	cndx = 0
	icopy = 0
##########################################################
#       call initialization routine                      #
##########################################################
	icrst=2
	call rstart (icrst)
#############################################################
#       set command counter to beginning of file            #
#############################################################
	icoman = 1
	redire = .true.
	cndx = 0
	icopy = 0

	call taprw
	call prochk
	call eralph
	call whedr

# Return to calling routine
	return
	end
