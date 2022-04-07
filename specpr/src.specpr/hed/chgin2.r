	subroutine chgin2 (inxt)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine displays and changes the following
#ccc         header information:
#ccc         civil time a & b,siderial time b,date a & b,
#ccc         right ascension,declination
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    er,scrn0l,wjfren,dtchng,getdt,timchg,getime
#ccc  argument list description:
#ccc        argument: inxt
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#######################################################################
#                                                                     #
#   this routine displays & changes the following header information: #
#        civil time a & b, siderial time b, date a & b,           #
#        right ascension, declination                                 #
#                                                                     #
#   subroutines used:  er,scrn01,wjfren,dtchng,getdt,timchg,getime    #
#                                                                     #
#######################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lbl4"
	include "../common/label3"
	include "../common/lundefs"
	include "../common/alphabet"

	character*1     idummy
	integer*4       decl(3)
	integer*4	mm,dd,yy,hr,minute,deg,scale
	real*4		sec
	integer*2	chkbit, ibit

	ibit = 1
	if (chkbit(icflag,ibit) == 1) {     # text data record
		inext = 10
		return
	}

	repeat {
		inxt=0
		call scrn01
		call crtin
		i=1
		call wjfren (i,x,il)
		if (x!=0) {
			call what(i)
			next
		}
		if (il==ihx || il==ihe || il==ihr || il==ihg) {
			inxt = il
			return
		}
		if (x==0 && il==0) {
			inxt=3
			return
		}
		ier = 1
		
			if (il == iha) {
				while (ier!=0) {
					write(ttyout,101)
					call crtin
					call getime (hr,minute,sec,ier)
					if (ier==2) go to 900
				}
				isgn = 1   # result is positive
				call frdms (iscta,24000,hr,minute,sec,isgn)

			} else if (il == ihb) {
				while (ier!=0) {
					write(ttyout,201)
					call crtin
					call getime (hr,minute,sec,ier)
					if (ier==2) go to 900
				}
				isgn = 1   # result is positive
				call frdms (isctb,24000,hr,minute,sec,isgn)
		
			} else if (il == ihd) {
				while (ier!=0) {
					write(ttyout,401)
					call crtin
					call getime (hr,minute,sec,ier)
					if (ier==2) go to 900
				}
				isgn = 1   # result is positive
				call frdms (istb,24000,hr,minute,sec,isgn)
		
			} else if (il == ihf) {
				while (ier!=0) {
					write(ttyout,501)
					call crtin
					call getdt (mm,dd,yy,ier)
					if (ier==2) go to 900
				}
				call tojuld(yy,mm,dd,jdatea)
		
			} else if (il == iho) {
				while (ier!=0) {
					write(ttyout,601)
					call crtin
					call getdt (mm,dd,yy,ier)
					if (ier==2) go to 900
				}
				call tojuld(yy,mm,dd,jdateb)
		
			} else if (il == ihh) {
				ibit = 3
				if (chkbit(icflag,ibit) == 0) {        #RA
					while (ier!=0) {
						write(ttyout,701)
						call crtin
						call getime (hr,minute,sec,ier)
						if (ier==2) go to 900
					}
					isgn = 1   # result is positive
					call frdms(isra,1000,hr,minute,sec,isgn)
				} else {			   #Longitude
					while (ier!=0) {
						write(ttyout,702)
						call crtin
						call getang(deg,minute,sec,ier)
						if (ier==2) go to 900
					}
					isgn = 1   # result is positive
					call frdms(isra,1000,deg,minute,sec,isgn)
				}

			} else if (il == ihi) {
				ibit = 3
				if (chkbit(icflag,ibit) == 0) {
					write(ttyout,801)
				} else {
					write(ttyout,802)
				}
				call crtin
				i=1
				call getstr(i,idummy,ier)
				if (ier==2)
					break
				if (idummy=='-') isign =  -1
				else isign = 1
				i = 1
				call wjfren (i,x,il)
				if (il!=0 || x<-89. || x>89.) go to 810
				deg = abs(x)
				call wjfren (i,x,il)
				if (il!=0 || x<0. || x>59.) go to 810
				minute = abs(x)
				call wjfren (i,x,il)
				if (il!=0 || x<0. || x>60.) go to 810
				sec = abs(x)
				isgn = 1
				if (idummy == '-') isgn =-1
				call frdms(isdec,1000,deg,minute,sec,isgn)
				next
			} else {
810				if (il==ihr) break
				write(ttyout,811)
		}
	}
900     continue
#
101     format (' enter UT/Civil time when data processed:',/,
		' hh mm ss.sssss',/)
201     format (' enter UT/Civil time at start data acquisition:',/,
		' hh mm ss.sssss',/)
401     format (' enter siderial time at start data acquisition:',/,
		' hh mm ss.sssss',/)
501     format (' enter data processing date (year must be full 4 digits):',/,
		' mm dd yyyy',/)
601     format (' enter data acquisition date (year must be full 4 digits):',/,
		' mm dd yyyy',/)
701     format (' enter right ascension:',/,
		' hh mm ss.sss',/)
702	format (' enter Longitude:',/,
		' dd mm sss.sss',/)
801     format (' enter declination:',/,
		' dd mm ss.sss',/)
802	format (' enter Latitude:',/,
		' dd mm ss.sss',/)
811     format (' *** error ***',/)
	end
