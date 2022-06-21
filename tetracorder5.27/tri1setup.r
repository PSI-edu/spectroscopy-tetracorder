	subroutine tri1setup

	implicit integer*4(i-n)

#ccc  name:         tri1setup
#ccc  version date:
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: setup stuff for tetracorder
#ccc                     program start info, history file
#ccc                     and wavelength set.
#ccc
#ccc  algorithm description:
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:


	include "../specpr/src.specpr/common/spmaxes"   # max parameters, must be first

	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl3"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lbl7"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/ioftyp"
	include 	"../specpr/src.specpr/common/blank"
	include		"../specpr/src.specpr/common/lblvol"

# basic tetracorder parameters

        include "multmap.h"

	include "tri1.h"

	integer*4 iwtmpf
#RED just to be consisent
	integer iwidok  # function

# local variables
	real*4 a, b, x
	integer*4 icrst, ik, il, itmp, jend, length, lnb
	integer*4 ier, i, ic
	integer*4 ihour, imm, mm, dd, yy, jda
	real*4 ss
	integer*4 cmdverbose   # function cmdverbose

	logical*4       fexist     # file exists: true, false if doesn't

	ihbcksl = char(92)  # this is the backslash character

	call fuser(uname)  # get the user name

# Print program information to user

	write (ttyout,115) iversion
115     format (20x,
'===== Program Tetracorder (version',f5.2,') =====',//,
'     Roger N. Clark, Planetary Science Institute, Tucson AZ/Lakewood CO',/,
'     rclark@psi.edu',//,
'     This program maps multiple materials using multiple spectral', /,
'     spectral features per material in an imaging spectrometer' /,
'     data set.',/,
'     or with individual spectra',//,
'     The image cube data sets MUST be BIL Integer*2 format.',//,
'     The image cube output is 3 byte images per material:',/,
'              weighted fit',/,
'              weighted band depth',/,
'              weighted fit times band depth.',/,
'     The input image cube must be assigned as a specpr file',//,
'     Analysis results from individual spectra ',
	'are located in the results file',/)

	write (ttyout,114) maxmat, maxfeat, imaxch, maxpix, maxnotfeat
114	format ('     The current limits on mapping are:',/,
		11x,i5, ' materials,',13x,i5,
			' features/material,',/,
		11x,i5, ' channels/spectrum,',5x,i5,
			' pixels/scan line.',/,
		11x,i5, ' "NOT" features/material',/)

44	if (cmdverbose(-1) <= 1) write (ttyout,45)
45	format (' Enter the name of the history file')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	lengthhist = length
	if (length > 80) length = 80
	jend = i + length - 1
	histfile(1:length) = iopcon(i:jend)      # check for blanks
	itmp = index(histfile(1:length),' ')
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,184)
184		format (' ERROR: blanks are not allowed in file names',/)
		go to 44
	}
	itmp = index(histfile(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
185		format (' ERROR: tabs are not allowed in file names',/)
		go to 44
	}

	inquire (file=histfile(1:length),exist=fexist)

	if (fexist) {

		write (ttyout,183) histfile(1:length)
183		format ('Note: history file already exists, appending to',
			/,a)

		open (unit=lunhist, file=histfile(1:length),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'history', histfile
187			format (' OPEN ERROR',i5,' on ',a,' file:',/,a)
			go to 44
		}
		call flushseqfile(lunhist,histfile(1:length),ier)

		if (ier != 0) go to 44

		write (lunhist,190) ihbcksl,ihbcksl,ihbcksl,
					ihbcksl,ihbcksl

	} else {

		open (unit=lunhist, file=histfile(1:length),
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'history', histfile
			go to 44
		}
	}

# write beginning history
	write (lunhist,3000) ihbcksl, iversion, histfile(1:length)
3000	format (a,'# PSI Tetracorder, version',f5.2,'  history',/, a)

# Enter results file

144	if (cmdverbose(-1) <= 1) write (ttyout,145)
145	format (' Enter the name of the results file')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	if (length > 80) length = 80
	jend = i + length - 1
	resultfile(1:length) = iopcon(i:jend)      # check for blanks
	itmp = index(resultfile(1:length),' ')
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,184)
		go to 144
	}
	itmp = index(resultfile(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
		go to 144
	}

	inquire (file=resultfile(1:length),exist=fexist)

	if (fexist) {

		write (ttyout,189) resultfile(1:length)
189		format ('Note: results file already exists, appending to',
			/,a)

		open (unit=lunresult, file=resultfile(1:length),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'results file', resultfile
			go to 144
		}

		call flushseqfile(lunresult,resultfile(1:length),ier)

		if (ier != 0) go to 144

		write (lunresult,190) ihbcksl,ihbcksl,ihbcksl,
					ihbcksl,ihbcksl
190		format (a1,'#',/,a1,'#',78('='),/,
			a1,'# ',11(' NEWRUN'),/,a1,'#',78('='),/,a1,'#')

	} else {
		open (unit=lunresult, file=resultfile(1:length),
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'results file', resultfile
			go to 144
		}
	}

# write beginning results
	call jdatim(jda,isec)
	call todms (isec,1,ihour,imm,ss)
	call frjuld (yy,mm,dd, jda)
	write (lunresult,3003) ihbcksl, iversion, ihbcksl,
				mm,dd,yy,ihour,imm,ss,
				uname, ihbcksl, ihbcksl,
				resultfile(1:length),
				ihbcksl, ihbcksl,
				histfile(1:lengthhist)
3003	format (a,'# PSI Tetracorder, version',f5.2,/,
		a1,'# Analysis started on ',
		i2,'/',i2,'/',i4,' (mm/dd/yyy) ',
		i2,':',i2,':',f3.0,' UT   by user:',a,/,
		a1,'# results in file:',/, a1,'# ',a,/,
		a1,'# history in file:',/, a1,'# ',a)

	write (lunhist,3004)   ihbcksl,
				mm,dd,yy,ihour,imm,ss,
				uname, ihbcksl,
				resultfile(1:length)

3004	format (a1,'# Analysis started on ',
		i2,'/',i2,'/',i4,' (mm/dd/yyy) ',
		i2,':',i2,':',f3.0,' UT   by user:',a,/,
		a1,'# results in file:',/,a)


50	if (cmdverbose(-1) <= 1) write (ttyout,116)
116	format (1x,
'    Enter wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

140	call crtin
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
165     		format (' *** ERROR -- ',
				'invalid input re-enter ***:',/)

			go to 50
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 50
			}
			itrol(1) = iwtmpf
			itrol(2) = irecw
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call closef
		stop
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 50
			}
	}


	call namdwv(itrol(1),inamwi)  #name of input wavelength set
	if (nchans > imaxch) {
		call what (-1)
		write (ttyout,*) 'ERROR: number of channels:',nchans	
		write (ttyout,*) '     is larger than limit:',imaxch	
		go to 50
	}

# write history
	write (lunhist,3010) iwtmpf,irecw,ihbcksl
3010	format (a1,i7,14x,a,'# Wavelength set to use')

200	if (cmdverbose(-1) <= 1) write (ttyout, 201)
201	format (1x, 'Type:  temperature  mintemperature    maxtemperature  units (K or C)',/,
		1x, 'Example:  temperature  200 400 K',/)

210	call crtin
	i=1
220	call wjfren (i,x,il)
	if (il == iht) {
		i=i-1
		if ( iopcon(i:i+10) == 'temperature' ) {

			i = i+11
			call wjfren (i,x,il)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: minimum temperature not found, re-enter'
				go to 200
			}
			dtemperature(1) = x

			call wjfren (i,x,il)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: maximum temperature not found, re-enter'
				go to 200
			}
			dtemperature(2) = x

			call wjfren (i,x,il)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: temperature units not found, re-enter'
				go to 200
			}
			xtfact= 0.0  # kelvin offset
			if ( il == ihc || il == ihcc ) xtfact=273  # centigrade

			# internal temperatures are stored in Kelvin
			dtemperature(1) = dtemperature(1) + xtfact
			dtemperature(2) = dtemperature(2) + xtfact

		} else {
			write (ttyout,*) 'ERROR: temperature keyword not found, re-enter'
			call what (i)
			go to 200
		}

        } else  if (il==ihx || il==ihe)  {
                ic=il
                icrst = 1
                call rstart(icrst)
                call closef
                stop
        } else {
		write (ttyout,*) 'ERROR: temperature not found, re-enter'
		call what (i)
		go to 200
	}


400	if (cmdverbose(-1) <= 1) write (ttyout, 401)
401	format (1x, 'Type:  pressure  minpressure    maxpressure  units=bars',/,
		1x, 'Example:  pressure  0.05 400 bars',/)

410	call crtin
	i=1
420	call wjfren (i,x,il)
	if (il == ihp) {
		i=i-1
		if ( iopcon(i:i+7) == 'pressure' ) {

			i = i+8
			call wjfren (i,x,il)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: pressure not found, re-enter'
				go to 400
			}
			dpressure(1) = x

			call wjfren (i,x,il)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: pressure not found, re-enter'
				go to 400
			}
			dpressure(2) = x

			call wjfren (i,x,il)   # scan here for pressure units for (future)
			if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: pressure not found, re-enter'
				go to 400
			}

		} else {
			write (ttyout,*) 'ERROR: pressure not found, re-enter'
			go to 400
		}

        } else  if (il==ihx || il==ihe)  {
                ic=il
                icrst = 1
                call rstart(icrst)
                call closef
                stop
        } else {
		write (ttyout,*) 'ERROR: temperature not found, re-enter'
		go to 400
	}

##################### tetracorder mode  ##################################

600	if (cmdverbose(-1) <= 1) write (ttyout, 601)
601	format (1x, 'tetracorder mode is either cube or singlespectrum',/,
		1x, '     mode cube also includes singlespectrum',/,
                1x, 'Type:       mode cube ',/,
		1x, ' or type:   mode singlespectrum sound|nosound',/)

610	call crtin
	tetmode = 0   # tetracorder mode
	i=1
620	call wjfren (i,x,il)
	if (il == ihm) {
		i=i-1
		if ( iopcon(i:i+3) == 'mode' ) {

			i = i+4
			call wjfren (i,x,il)
			if (il == ihc) {           # mode cube
				i=i-1
				if ( iopcon(i:i+3) == 'cube' ) {
					tetmode = 2    # tetracorder mode = cube + singlespectrum
					write (lunhist,*) 'mode cube'
				} else {
					call what(i)
					write (ttyout,*) 'ERROR: mode cube or mode singlespectrum not found, re-enter'
				}				
			} else if (il == ihs) {    # mode singlespectrum
				i=i-1
				if ( iopcon(i:i+13) == 'singlespectrum' ) {
					tetmode = 1    # tetracorder mode = singlespectrum only

					i = i+14
					call wjfren (i,x,il)
					i=i-1
					if ( iopcon(i:i+6) == 'nosound' ) {
						soundenable = 0
						write (lunhist,*) 'mode singlespectrum nosound'
						write (ttyout,*) 'DEBUG: mode singlespectrum nosound'
					} else {
						soundenable = 1
						write (lunhist,*) 'mode singlespectrum sound'
						write (ttyout,*) 'DEBUG: mode singlespectrum sound'
					}
				} else {
					call what(i)
					write (ttyout,*) 'ERROR: mode cube or mode singlespectrum not found, re-enter'
				}
			} else if (il==ihx || il==ihe)  {
				call what (i)
				write (ttyout,*) 'ERROR: mode cube or mode singlespectrum not found, re-enter'
				go to 600
			} else {
				write (ttyout,*) 'ERROR: mode cube or mode singlespectrum not found, re-enter'
				go to 600
			}

		} else {
			write (ttyout,*) 'ERROR: mode cube or mode singlespectrum not found, re-enter'
			go to 600
		}

        } else  if (il==ihx || il==ihe)  {
                ic=il
                icrst = 1
                call rstart(icrst)
                call closef
                stop
        } else {
		write (ttyout,*) 'ERROR: mode not found, re-enter'
		go to 600
	}

	return
	end

