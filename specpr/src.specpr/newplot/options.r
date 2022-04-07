	integer*4 function option(i,datoff,datmul,xoffset)
	implicit integer*4 (i-n)

#ccc  version: %W% %G% %U%
#ccc  author(s): J.A.Hoover
#ccc  language:RATFOR
#ccc
#ccc  short description: 
#ccc			this routine decodes the plotting option from
#ccc            the users input line.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc			wjfren,crtin,dltpts,gettxt
#ccc  argument list description:
#ccc			i --- starting position in iopcon
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

	include "../common/lundefs"
	include "../common/pltcnt"
	include "../common/alphabet"
	include "../common/lbl4"
	include "../common/lbl7"
	include "../common/label1"
	include "../common/iocontrol"

	logical gettxt
	real*4	x
	real*4 datoff     # data offset
	real*4 datmul     # data multiplier
	real*4 xoffset    # xoffset offset
	integer*4  ihstar
        integer*4    lntype  
        logical     blksympen

        lntype =0
        blksympen=.false.
	datoff = 0.0
	datmul = 1.0
	xoffset = 0.0

	ihstar=ihchar('*')

	pltopt = iopcon(i:80)
	repeat {
		call wjfren(i,x,il)
		if (i>=80) break

		if (il ==  ihcv | il == ihcw | il == ihcd | il == ihcu | il == ihcy) {
			iwtmpf = il
			call wjfren(i,x,il)
			if (il==ihx) {
				option=-1
				return
			}
			if (x<1.0 || x>maxrec) {
				write(ttyout,100)
				option=1
				return
			}
			wavrec = x
			call redwav(iwtmpf,wavrec,ier)
			if (ier != 0) {
				write (ttyout,100)
				option=1
				return
			}
			wavid = iwtmpf
			if (il!=0) i=i-1

		} else if (il ==  ihandp) {     # auto wavelength get
			itrol(1) = ihandp  # set for autowav
			itmp=0
			call autowv (itmp) #get autowav 
			iwtmpf = itrol(1)
			wavrec = itrol(2)
			call redwav(iwtmpf,wavrec,ier) #read waves
			if (ier != 0) {
				write (ttyout,100)
				option=1
				return
			}
			wavid = iwtmpf

		} else if (il ==  iht) {
			txtflg = .true.

#      set blk symbol flag 
                } else if (il ==  ihcb) {
                        blksympen=.true.

		} else if (il ==  ihp) {
			call wjfren(i,x,il)
			ptsize = -1
			if (x>=0 && x<6) ptsize = x
			if (il!=0) i=i-1

		} else if (il ==  ihl) {
			call wjfren(i,x,il)
			if (il==ihx) {
				option=-1
				return
			}
			linetp = 1
                        if (penplt == 0){
                                   if (x>=9) x=0
                        }
			linetp = x
			if (penplt == 0) linetp = ((linetp-1)/2)*2+1
			if (linetp<0) linetp = 0
			if (il!=0) i=i-1


#       split the old option  'l' (line type/line color) into
#       l = line color, L= line type.  Then at end of this loop
#       calculate linetp 
                } else if (il ==  ihcl) {
                        call wjfren(i,x,il)
                        if (il==ihx) {
				option=-1
				return
			}
                        if (x<0 || x>6) {
                               lntype= 0
                        }else{
                               lntype=x
                        }
                        if (il!=0) i=i-1

		} else if (il ==  ihc) {
			conct = 1

		} else if (il ==  ihcc) {
			conct = 2

		} else if (il ==  ihe) {
			errbar = 1

		} else if (il ==  ihce) {
			errbar = 2

		} else if (il ==  ihd) {
			delete = .true.

		} else if (il ==  ihn) {
			nolim = .true.

		} else if (il ==  ihg) {
			newplt = .true.

		} else if (il ==  ihs) {
			call wjfren(i,x,il)
                        if (x>0 && x<16) symtyp = x
			if (il!=0) i=i-1

		} else if (il ==  ihm) {
			call wjfren(i,x,il)
			if (x>0) more = x
			if (more>9) more = 3
			if (il!=0) i=i-1

		} else if (il ==  iho) {
			call wjfren(i,x,il)
			datoff = x           # data offset
			write (ttyout,"('debug: datoff=',f14.5)") datoff
			if (il!=0) i=i-1

		} else if (il ==  ihstar) {
			call wjfren(i,x,il)
			datmul = x           # data offset
			write (ttyout,"('debug: datmul=',f14.5)") datmul
			if (il!=0) i=i-1

		} else if (il ==  ihx) {
			call wjfren(i,x,il)
			xoffset = x           # x-axis offset
			write (ttyout,"('debug: xoffset=',f14.5)") xoffset
			if (il!=0) i=i-1

		} else {
			write(ttyout,200) il
		}
	} until (i>=80)

#        if line type option picked , then combine with linetp
#        hpdaemon will seperate linetp
         if (lntype >0 && lntype <=6) {
                lntype = lntype*10
                linetp = linetp+lntype
         }	

#	if black symbols then make linetp greater than 100
#	doplot.r will readjust linetp
        if (blksympen) {
         	linetp=linetp+101
        }

	if (delete) {
		write(ttyout,300)
		call crtin
		i = 1
		j = 0
		n = maxchn
		call dltpts(i,j,ldel,n,il)
		if (il==ihe || il==ihx) {
			option=-1
			return
		}
	}

	if (txtflg)
		if (gettxt(ii)) {
			option=0
			return
		}
		else {
			option=-1
			return
		}
	else {
		option=0
		return
	}

100     format(' INCORRECT ENTRY: reenter',45('-'))

200     format(' UNKNOWN OPTION ',a,'. option ignored.')

300     format(' Type in channels to be DELETED.  Type  c  to continue')

	end
