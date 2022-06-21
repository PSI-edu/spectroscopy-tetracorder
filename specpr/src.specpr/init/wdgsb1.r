	subroutine wdgsb1(iwdflg)
	implicit integer*4(i-n)
#ccc  version date: 04/05/85
#ccc
#ccc  author(s):  roger clark & jeff hoover
#ccc  language:   fortran
#ccc
#ccc  short description:  this subroutine opens the files and
#ccc                      initializes different parameters.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc          crtin,wjfren,getstr,rstart,initt,tname,setfil,er
#ccc  argument list description:
#ccc       argument:
#ccc        iwdflg
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include	"../common/blank"
	include	"../common/lbl7"
	include	"../common/lbl6"
	include	"../common/lbl4"
	include	"../common/label1"
	include	"../common/labl2"
	include	"../common/lbl3"
	include	"../common/label3"
	include	"../common/labelf"
	include	"../common/lblg"
	include	"../common/info"
	include	"../common/lblvol"
	include	"../common/lblprt"
	include	"../common/cmd"
	include "../common/cmdarg"
	include	"../common/hptrm"
	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/filenames"
	include "../common/iocontrol"
	include "../common/ioftyp"

	logical         iwdflg

	integer*4 idummy

	iwdflg = .false.
	ititl = ' '
	icnt = 80
	write(ttyout,30)
	write(ttyout,31)  # print machine dependent massage about machine type
1	write(ttyout,32)
#
	call crtin
#	write (*,*) 'DEBUG: wdgsb1 start'
#
#
	ix = 0
	i = 1
	repeat {
		call wjfren(i,x,il)
#		write (*,111) i, il
#111		format ('DEBUG: wdgsb1, wjfren: i=',i3,' il=',a)
		if (il==ihu) {
			write (*,*) "New Restart File"
			go to 10
		}
		if (il==ihr)
			break 1
		if (i>=80)
			go to 1
	}
#
#     do a restart
#
		close(1,iostat = idummy)
		close(2,iostat = idummy)
		close(3,iostat = idummy)
		close(4,iostat = idummy)
		close(7,iostat = idummy)
		close(8,iostat = idummy)
		close(9,iostat = idummy)
		close(10,iostat = idummy)
		close(12,iostat = idummy)
		close(13,iostat = idummy)
		close(15,iostat = idummy)
		close(16,iostat = idummy)
		close(17,iostat = idummy)
		close(18,iostat = idummy)
		close(19,iostat = idummy)
	repeat {
#-----
		call getstr(i,irfl,ierr)
		if (ierr==2) {
			write(ttyout,40)
			i = 1
			call crtin
		}
	} until(ierr!=2)
	ic = 2
	call rstart(ic)
	icoman = 1
	if (ic==-1)
		stop
	ititl(1:1) = 'a'
	return

10	call getstr(i,irfl,ierr)
		if (ierr==2) {
			write(ttyout,40)
			i = 1
			call crtin
		}
	# if error, go to 10 and try again
#RED changed from ier to ierr in if statement
	if (ierr == 2) go to 10
#
	write (ttyout,12) irfl
12	format (' restart file name=',a)
#     open the scratch file speadd (used by wegadd, extnct)
#
20  if (igrmod==0) igrmod = 99
	call initt(igrmod)
	# recl = maxchn *4
	if (maxchn < 4852) {
		write (ttyout,*) "wdgsb1: maxchn=", maxchn
		call crtin (i,x,IL)
	}
	irecl=maxchn*4
	open(addlun,status='scratch',access='direct',recl=irecl,form='unformatted', iostat=ier)
	if ( ier != 0) {
		write (ttyout,*) "wdgsb1: open error on addlun:", ier
	}
#
#     allocate the title file
#
	open(ttllun,file=TITLE,access='direct',recl=128,form='unformatted', iostat=ier)
	if ( ier != 0) {
		write (ttyout,*) "wdgsb1: open error on ttllun:", ier
	}
#
#
#     assign restart parameter file
#
#	KEL: changing open form, so now make open call in rstart only
#	open(rlun,file=irfl,access='direct',recl=2048,form='unformatted')
	call rstart(4)
#
#     assign plot scratch file
#
	open(pltlun,status='scratch',access='direct',recl=1536,form='unformatted')
#
#     3 = savefile compliment
#     4 = rawfile compliment
#     5 = crt input
#     6 = crt output
#     7 = workfile
#     8 = savefile
#     9 = rawfile
#    10 = addition scratch file
#    11 =                   not used
#    12 = line printer
#    13 = plot scratch file
#    14 =                   not used
#    15 = title storage file
#    17 = starpack
#    18 = restart parameter file
#
#     open all files and devices (all assigned logical unit numbers)
#
#
	mag0 = -1
	mag1 = -1
	isavf = 1
	iwjf = 1
	iwrkf = 1
	istrf = 1
	ilpt = 1
	ipp = 1
	isvcu = 1
	iwjcy = 1
	idevc(1) = -1
	idevc(2) = -1
	idevc(3) = -1
	idevc(4) = -1
	iprtu = 0
	iprty = 0
	iprtd = 0
	iprtv = 0
	iprtw = 0
	iprts = 0
#
#***  temporary initialization: nchans, ibnrm1, ibnrm2, alat
#
	nchans = 120
	ibnrm1 = 30
	ibnrm2 = 38
	ibncon = 0
	wmina = 0
	wmaxa = 0
	alat = 0
#
#
	itrol(1) = ihcc
	itrol(2) = 256
	itrol(3) = ihh
	infmth = 0
	infopr = 1
	inftrn = 0
	ier = 2
	ifilex = 9999
	icon = 1

# set io error message control flags

	rferfl(1) = 1
	rferfl(2) = 0
	rferfl(3) = 0

# set file types to specpr default

	filreq(1) = 0
	filreq(2) = 0
	filreq(3) = 0

	do i = 1, 5 {
		do j = 1, 12 {
			filtyp(j,i) = 0
		}
	}

#

	call eralph
	return

30  format(1x,72('*'),/,
	1x,21('*'),' SPECtrum Processing Routines ',21('*'),/,
	1x,72('*'),/,
	1x,18('*'), ' Current program version: 06/20/2022 ',19('*'))

31 format (1x,29('*'),'            ',31('*'))

32	format (1x,72('*'),//, 1x,72('*'),/,
	'  published papers for which this program ',
			'played a major processing',/,
	1x,'role could reference:',//,
	1x, 'Clark, R. N. (1980).  A Large Scale Interactive One Dimensional Array',/,
	7x,'Processing System.   Pub. A. S. P., 92, 221-224.',/,
	1x,72('*'),///,
	1x,'Type:  u  and filename for NEW RESTART file and do NEW SETUP,',/,
	1x,'       r  and filename to  RESTART with a PREVIOUS SETUP',/)

40  format(1x,'type filename to be created',/)
	end
