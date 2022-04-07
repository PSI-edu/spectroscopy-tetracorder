	subroutine f47(ic)
	implicit integer*4 (i-q)
	implicit real*4 (x-y)

#ccc  name: f47
#ccc  version date: June 30, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: RATFOR
#ccc
#ccc  short description: Reconstructs spectra from feature analysis data
#ccc			In a winged triangle fit.  ie-1-hfmax-1/4max-center
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
	include "../common/label1"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"
#RED
	integer*4 iwidok    # function
	integer*4 fadnum,itnum,itlast,var
	real*4 contin,asym,depth,width,ewidth,center,lftop,rttop,a,b
	real*4 hfmax,ehfmax,a2,b2
	character inam*8,inamwi*8

#	load data and wavelengths
	if (ictrl==-1) {
		ic=ihx
		write(ttyout,55)
		return
	}
	do i=1,maxchn {
		data(i)=0.0
		datab(i)=dataa(i)
		datac(i)=0.0
	}


	call eralph
	call whedr2
	write(ttyout,115) idv1,ifl1,ititl

	fadnum=itchan
140	call crtin
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
			go to 140
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
			itrol(1) = iwtmpf
			itrol(2) = irecw
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		ic=il
		return
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 140
			}
	}
#	band Number 
	write (ttyout,300)
	call crtin
	i=1
	a=0
	call wjfren (i,a,il)
		if (il==ihx || il==ihe) {
			ic=il
			return
		}
	if (a>0) {
		var=a
	} else {
		var=0
	}

	do i=1,nchans {
		data(i)=1.0
		datac(i)=1.0
	}
	do j=fadnum,1,-9 {
		if (var!=0) {
			if (j!=var*9) next
		}
		ewidth=datab(j)
		asym=datab(j-4)
		depth=datab(j-6)
		width=datab(j-7)
		center=datab(j-8)

		b=ewidth/(asym+1.0)
		a=ewidth-b	
		b2=width/(asym+1.0)
		a2=width-b2
		hfmax=1.0-depth/2.0	
		ehfmax=1.0-(depth*(3.0/4.0))

#   1st time ewidth:

		x1=center
		y1=1.0-depth
		x2=center-a
		y2=ehfmax
		x3=center-a2
		y3=hfmax
		x4=center+b
		y4=ehfmax
		x5=center+b2
		y5=hfmax
		x6=center-2.0*a2
		x7=center+2.0*b2

		do i=1,nchans {
			if (dataa(i)>=x6 && dataa(i)<=x3) {
				data(i)=((y3-y1)/(x3-x1))*(dataa(i)-x1)+y1
			}
			if (dataa(i)>=x3 && dataa(i)<=x2) {
				data(i)=((y3-y2)/(x3-x2))*(dataa(i)-x2)+y2
			}
			if (dataa(i)>=x2 && dataa(i)<=x1) {
				data(i)=((y2-y1)/(x2-x1))*(dataa(i)-x1)+y1
			}
			if (dataa(i)>=x1 && dataa(i)<=x4) {
				data(i)=((y4-y1)/(x4-x1))*(dataa(i)-x1)+y1
			}
			if (dataa(i)>=x4 && dataa(i)<=x5) {
				data(i)=((y5-y4)/(x5-x4))*(dataa(i)-x4)+y4
			}
			if (dataa(i)>=x5 && dataa(i)<=x7) {
				data(i)=((y5-y1)/(x5-x1))*(dataa(i)-x1)+y1
			}
		}
		do i=1,nchans {
			datac(i)=datac(i)*data(i)
			data(i)=1.0
		}
	}	
	if (var==0) {
		do i=1,nchans {
			datac(i)=datac(i)*datab(6)
		}
	}
#  history
#  -------
	call namdwv(itrol(1),inamwi)  #name of input wavelength set
599	call namdev (idv1,inam)
	write(ihist,506) inam,ifl1,var
	write(mhist(1:74),487) inamwi,itrol(2)
600	call rstart(1)
	return

#  formats
#  -------

55      format (' *** error -- invalid file status ***',/,
'     program will exit.',/)

115     format (' ',
'Function f47: Feature Analysis Spectrum Recreation',//,
'     This function will re-create the spectrum analized by feature ', //,
'     analysis, working on: ',a,i4,':',a,//,/,
'     Enter new wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

165     format (' *** error -- invalid input re-enter ***:',/)

300	format ('Enter number of single band to recreate, or 0 for all')

487   format('**f47: wavelength set: ',a,' r',i6)

506     format ('f47: ',a8,' r',i7,' band#=',i3)

	END
