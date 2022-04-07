	subroutine f46(ic)
	implicit integer*4 (i-q)

#ccc  name: f46
#ccc  version date: June 30, 1987
#ccc  author(s):  Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Prints the Feature Analysis Data
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
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl3"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
	include "../common/spfeat"

	integer*4 ijunk(32)
	character inam*8,inamwi*8,icomm*20,afile*80
	integer*4 zlun,last,ier
	equivalence (ijunk,sptitl)

	zlun=0

#     *** switch back to alpha mode and erase screen ***

	call eralph
	write(ttyout,10)

#     *** if user didn't give file, hard exit ***
	if (ictrl == -1) {
		write(ttyout,20)
		call crtin
		ic = ihx
		return
	}

#     *** tell user which file working with (should be in <dataa>) ***
	write(ttyout,50) idv1,ifl1,ititl
	ifile = ifl1

#     *** see if user wants to continue ***
	write(ttyout,60)
	call crtin
	i = 1
	call wjfren(i,x,ic)
	if ((ic == ihe) | (ic == ihx)) return

#
#	get output file name
#
30	write (ttyout,200)
	call crtin
	i=1
	call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
	afile=iopcon(1:20)
	ier=0
	last=0
	call spopen(afile,last,ier)
	if (ier>0) go to 99
	if (ier==-3) {
		write (ttyout,201)
		go to 30
	}
	if (ier==-4) {
		write (ttyout,202)
		go to 30
	}
	if (ier==-5) {
		write (ttyout,203)
		go to 30
	}
	call namdev (idv1,inam)
	call namdwv(itrol(1),inamwi)  #name of input wavelength set
#	
#	get comments
#
	write (ttyout,301)
	call crtin
	i=1
	call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
	icomm=iopcon(1:20)
#
#	write 'em
#
#	these are constant for all feature
#
	irecn=ifl1
	fname=inam
	icomnt=icomm
	sptitl=ititl
	do k=1,itchan,9 {
#
#	these aren't
#
		center=dataa(k)
		width=dataa(k+1)
		depth=dataa(k+2)
		errd=dataa(k+3)
		asym=dataa(k+4)
		contin=dataa(k+5)
		iter=dataa(k+6)
		xfract=dataa(k+7)
		qwidth=dataa(k+8)
# since it's writing to a binary file, we just tell it to write the
# first variable of the common block, and it'll write that, and the
# next 128 bytes too. 
		write (zlun,rec=last+1,iostat=ier) ijunk

#	check for i/o errors
		if (ier!=0) go to 99
		last=last+1
	}
99	if (ier!=0) {	
		call ertyp('f46',zlun,ier)
		ier=4
		return
	}
	ic=ihx
	close (zlun,iostat=ier,err=99)
	return
10      format(' special function f46:'/' routine outputs feature analysis',
	  ' data '/' in a readable format.'//)
20      format(' no spectrum was inputted. press return to',
	   ' hard exit'/)
50      format(' printing ',a,i4,':'/1x,a/)
60      format(' press return to continue or e or x to exit.'/)
105	format(/,2x,'Center',t18,'Width',t34,'Depth',t50,'Error',t66,
		'Asymmetry',t82,'Contin Val',t96,'It#',t105,'Fraction',
		t120,'1/4 width'/)
200 	format(' Enter output filename',/)
201 	format(' Error in reading file name, try again ',/)
202 	format(' Cannot have imbedded blanks in file name, try again',/)
203	format(' That file exists, and is not compatable with this'/,
		'type of output, try again',/)
301	format(' Enter comments (up to 20 char)',/)
	end
