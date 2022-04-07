	logical function getplt (ii)
	implicit integer*4 (i-n)
#ccc  name: getplt
#ccc  version date: 8/15/83 %W% %G% %U%
#ccc  author(s): J.A. Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: This routine gets the plotting information
#ccc			from the user.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: eralph,vaxis,haxis,whedr,sclfct,gbldel,gtrecs
#ccc  argument list description: ii --- dummy argument
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

	include "../common/alphabet"
	include "../common/lundefs"
	include "../common/label1"
	include "../common/lblg"
	include "../common/pltcnt"

	logical haxis,vaxis,sclfct,gbldel
	integer*4 gtrecs
	integer*4	i,l
	real*4		x

	repeat {
		call eralph
		call whedr
		nplots = 0

		penplt =4 			# default = HPGL
		repeat {
			write(ttyout,100)
			call crtin
			i = 1
			call wjfren(i,x,l)
			if (i>=80) break
			else if (l == ihg) {
				penplt = 0     	    #   = gould
				break;
			} else if (l == ihh) {
				penplt = 1          #   = hp9872
				break;
			} else if (l == ihj) {
				penplt = 2          #   = HP7550
				break;
			} else if (l == ihp) {
				penplt = 3          #   = Postscript
				break;
			} else if (l == iht) {
				penplt = 5          #   = Tgif
				break;
			} else if (l == ihe || l == ihx) {
				getplt=.false.
				return
			}
		}
		if (penplt == 0) {
			write (ttyout,"('Output to a Gould Plotter',/)")
		} else if (penplt == 1) {
			write (ttyout,"('Output to a HP9872 type Plotter',/)")
		} else if (penplt == 2) {
			write (ttyout,"('Output to a HP7550 type Plotter',/)")
		} else if (penplt == 3) {
			write (ttyout,"('Output to a Postscript file .postscript.plot',/)")
		} else if (penplt == 4) {
			write (ttyout,"('Output to a HPGL file .hpglplot then printed',/)")
		} else if (penplt == 5) {
			write (ttyout,"('Output to a TGIF file .tgifplot.obj',/)")
		}
				
		if (!haxis(ii)) {
			getplt=.false.
			return
		}

		if (!vaxis(ii)) next

		if (!sclfct(ii)) next

		if (!gbldel(ii)) next

		nplots = gtrecs(ii)

		if (nplots==-1) next
		else {
			getplt=.true.
			return
		}
	}
100	format('** Spectrum Plotting Routines **'/,
	'      Type  p  to make Postscript plots,',/,
	'            t  to make a TGIF plot file,',/,
	'      or    return to make HP7550 plots',//,
	'  SITE SPECIFIC PLOTTERS:',//,
	'      Type  g  to make GOULD plots,'/,
	'            h  to make HP9872 plots,'/,
	'            j  to make HP7550 plots',//,
	'      Type  e or x to EXIT to main menu',/)

	end
