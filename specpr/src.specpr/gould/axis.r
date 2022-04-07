	subroutine axis(dn1,acr1,dn2,acr2,wlabel)
	implicit integer*4 (i-n)
#ccc    name: axis
#ccc    version date: 8/1/83
#ccc    author(s): Jeff Hoover
#ccc    language: RATFOR
#ccc
#ccc    short description: 
#ccc		This routine draws an axis for the gould plotting routines.
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called: swap,snorm,ticks,signif,fnb,line,catn,symbol
#ccc    argument list description:
#ccc       dn1     input   vertical coordinate of 1st end of axis
#ccc       dn2     input      "         "      "  2nd  "  "   "
#ccc       acr1    input   horizontal   "      "  1st  "  "   "
#ccc       acr2    input      "         "      "  2nd  "  "   "
#ccc       wlabel  input   label for axis
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc
	include "fdefs.h"
	include "../common/plot01"
	include "../common/plot02"

#RED
	real*4 centim       # function centim
	integer*4 lnb     # function lnb

	real*4    xlabel(6)
	character*(*)   wlabel
	character*12    ilabel(6),ctmp
	character*80    label
	integer*4       fnb  # function fnb  

	dfrom = dn1
	afrom = acr1
	dto   = dn2
	ato   = acr2

#ccc    if dfrom=dto then draw x-axis
	if (dfrom == dto) {
		if (afrom > ato ) call swap(afrom,ato)
		call snorm(afrom,ato,sf,n)
		call ticks(afrom,ato,xlabel,strt,wincr,tincr)
		for(i=1; i<= 6; i=i+1) if(xlabel(i)!=-1.23e34)
			xlabel(i) = xlabel(i)/sf
		call signif(xlabel,ilabel)
		dsize = dfrom + centim(2)/dscale
		tik = 19./(dscale*factor)
		if (wlabel==" ") tik=-tik
		down = dfrom - 2.*tik
		i = 1
		for (x=strt; x<ato & i<=6; x=x+wincr) {
        		call line(down,x,dto,x,1)
			if (wlabel!=" ") {
				j = fnb(ilabel(i))
				jj = lnb(ilabel(i))
				asize = (centim(jj-j+1)+0.4)/(2.*ascale)
				call symbol(dsize,x-asize,16,
					ilabel(i)(j:jj),90.)
			}
			i = i+1
		}
		x = strt - tincr
		down =  dfrom - tik
		while(x >= afrom) {
        		call line(down,x,dto,x,1)
			x = x-tincr
		}
		x = strt + tincr
		while (x < ato) {
         	call line(down,x,dto,x,1)
			x = x+tincr
		}

        	call line (dfrom,afrom,dto,ato,1)
		if (wlabel!=" ") {
			call catn(label,wlabel,n)
			down = dsize+(centim(1)+0.5)/dscale
			across = (afrom+ato)/2.-
				centim(lnb(label)-fnb(label))/(2.*ascale)
			if (wlabel == "Wavelength (!m!m)") {
				across=across + 2/(2.*ascale)
			}
			if (wlabel != label) across=across + 1/(2.*ascale)
			call symbol(down,across,16,label(fnb(label):lnb(label)),90.)
		}
	}

#ccc    if afrom=ato then draw y-axis
	else if (afrom == ato) {
		if (dfrom > dto) call swap(dfrom,dto)
		call snorm(dfrom,dto,sf,n)
		call ticks(dfrom,dto,xlabel,strt,wincr,tincr)
		for(i=1; i<= 6; i=i+1) if(xlabel(i)!=-1.23e34)
			xlabel(i) = xlabel(i)/sf
		call signif(xlabel,ilabel)
		dsize = centim(1)/(2.*dscale)
		tik = 19./(ascale*factor)
		if (wlabel!=" ") tik=-tik
		across = afrom - 2.*tik
#
# draw major tick marks and find the bounds on the tick mark labels
#
		i=1
		jmin = 12
		jmax = 0
		for (x=strt; x<dto & i<=6; x=x+wincr) {
        		call line(x,across,x,ato,1)
			if (wlabel!=" ") {
				j = fnb(ilabel(i))
				jj = lnb(ilabel(i))
				if (j  < jmin) jmin = j
				if (jj > jmax) jmax = jj
			}
			i=i+1
		}
		if (jmax > 12) jmax = 12
		if (jmin < 1)  jmin = 1
		if (jmax < jmin) jmax = jmin
#
# now actially label the tick marks with proper number
#
		i = 1
		for (x=strt; x<dto & i<=6; x=x+wincr) {
			asize=afrom-(centim(jmax-jmin+1)+0.4)/ascale
			if (wlabel!=" ") {
# now right-register the characters
				ctmp=ilabel(i)
				ilabel(i)='            '
				jj = jmax
				do itmp = jmax, jmin, -1 {
					if (ctmp(itmp:itmp) == ' ') next
					ilabel(i)(jj:jj) = ctmp(itmp:itmp)
					jj = jj -1
				}
# now print it out on plot stuff
				call symbol(x+dsize,asize,16,
						ilabel(i)(jmin:jmax),90.)
			}
			i=i+1
		}
		x = strt-tincr
		across = afrom - tik
		while (x >= dfrom) {
        		call line(x,across,x,ato,1)
			x = x - tincr
		}
		x = strt + tincr
		while (x < dto) {
        		call line (x,across,x,ato,1)
			x = x+ tincr
		}
        	call line (dfrom,afrom,dto,ato,1)
		if (wlabel!=" ") {
			call catn(label,wlabel,n)
			down = (dfrom+dto)/2.+
				centim(lnb(label)-fnb(label)+2)/(2.*dscale)
# was this:		across = asize-centim(1)/ascale
# next this:		across = amaxsz-(centim(1)-.4)/ascale
			across = asize-(centim(1)-.4)/ascale
			call symbol(down,across,16,label(fnb(label):lnb(label)),180.)
		}
	}
	else {
		write(6,100)
100             format(' ** axis must be horiz. or vertical')
		return
	}

	return
	end
