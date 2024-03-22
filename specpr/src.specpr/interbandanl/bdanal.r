subroutine bdanal(lbnd,diff,xmax,xmin,wavshift)
#
#ccc  name:
#ccc  version date:
#ccc  author(s): M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
# This subroutine calculates the fwhm of a band given two points
# defining a continum and a point defining the band center
#
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
# 			lbnd: the lower vertical plot bound
#			diff: the height of the vertical scale
# 			xmax: the upper bound of the x axis
#			xmin: the lower bound o the x axis 
#                       wavshift: amount of wavelength shift to apply
#                                 (set by user in wriout)
#ccc  common description:
#			every common block in the world or so it
#			seems is included
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:  This program was updated 6/18/87, because it was found
#ccc	to have a rather nasty round off error.  sqrt(c/a) was
#ccc	replaced by (-b/2*a) in finding the center wavelength.  This does
#ccc	not drastically affect any previous computations, but it will
#ccc	probably change the 4th decimal point, and beyond.  -ngorelic
#ccc
	include "../common/spmaxes"   # max parameters, must be first

  include "../common/alphabet"
  include "../common/blank" 
  include "../common/cmd"
  include "../common/dscrch" 
  include "../common/hptrm"
  include "../common/label1"                
  include "../common/label3"
  include "../common/labelf"
  include "../common/labl2"
  include "../common/lbl3"
  include "../common/lbl4"
  include "../common/lbl6"
  include "../common/lbl7"
  include "../common/lblg"
  include "../common/lundefs"
  
#
#
#
 
#  Partial variable description 
# logicals
#
# found[a-z]: these variables represent whether the appropriate 
# 	      full width has been found 
# character 
#
# hopefully these can be figured out 
#
# integers
# variables beginning with the letter g refer to hpgraphics
# coordinate variables the second letter generally indicates
# whether the variable is an x or y graphics coordinate after
# that your on your own  ( We need perfect hash functions to
# avoid this 6 character limit stuff !! )
#
# anything containing the letters ch,chn,chan may have some chance
# of being some sort of chanel 
#
# reals 
# 	bndwth: the band width
# 	bnddep: the band depth
#	asymet: the assymetry parameter

#RED
  integer*4 mindat     # function mindat

  logical found1,found2,found,foundq,founde,founds,foundt,foundf
  character*1 method,below 
  character*1 bar
  character*8 inam
  character*15 coment
  character*20 title
  character*80 atemp
  character*80 filnam
  character*80 outline	# this is for X-windows
  integer*4 gxpos1,gypos1,chan1,gxpos2,gypos2,chan2
  integer*4 gxtmp,gytmp,chtmp
  integer*4 chanmn,chanmx
  integer*4 gxbctr,gybctr
  integer*4 gxcen,gycen,cenchn,minchn
  integer*4 gxhflt,gyhflt,gxhfrt,gyhfrt
# RED
  integer*4 chnrht
  integer*4 gxhf1,gyhf1,chnhf1,gxhf2,gyhf2,chnhf2
  integer*4 gxerr1,gyerr1,chner1,gxerr2,gyerr2,chner2
  integer*4 gxcctr,gycctr
  integer*4 idv,il 
  integer*4 oiprom
  integer*4 colpos,i,wflag,flflag,crflag,chanel,igo,side
  integer*4 gxctra,gyctab,gyctac
  integer*4 chxfwa
  integer*4 qchans,chnqt1,chnqt2 
  integer*4 gxqrt1,gyqrt1,gxqrt2,gyqrt2
  integer*4 chxqwa,gxqrta,gyqrtc,gyqrtb
  integer*4 nchanl,nchanr,gxsixa,gysixc,gxthra,gythrc,gxeiga,gyeigc
  integer*4 divnum,chnlft,chnfht,chndif
  integer*4 gxwth1,gywth1,gxwth2,gywth2
  integer*4 gxdiva,gydivc,gydivb
  integer*4 gxlstb,gylstb,gylstc
  integer*4 gxsfra,gysfrc
  integer*4 lchhf1,lchhf2,lfcchn,rtcchn
  integer*4 chnerl,chnerr, iscale
  real*4 xdata(SPMAXCHAN), ydata(SPMAXCHAN), lbnd, diff
  real*4 xmin,xmax

  real*4 a,b,c,d,e,f
  real*4 refsca,refmin,refmax
  real*4 bndctr,refctr 
  real*4 bnddep
  real*4 refhfl,refhfr
  real*4 bndwth
  real*4 horerr 
  real*4 xfwhm1,yfwhm1,xfwhm2,yfwhm2,xfwhma
  real*4 oubnd,olbnd,owvmin,owvmax
  real*4 bfwhm1,bfwhm2,mfwhm1,mfhwm2
  real*4 fbdctr,frfctr
  real*4 reffwa,bfwavg,mfwavg 
  real*4 mfwqm1,mfwqm2,bfwqm1,bfwqmr2
  real*4 xfwqm1,xfwqm2,xfwqma,refqwa 
  real*4 mqwavg,bqwavg
  real*4 asymet
  real*4 oxband,orband,rbnda,orcont,refwth,xwdtha,xwdlft,xwdrht
  real*4 oxwthl,oxwthr,refhlf
  real*4 rflast
  real*4 mband,bband, wavshift

  real*4 conrdivl, rcbblc, lcbbrc, ctmp, ctmp2

  equivalence (xdata,dataa)
  equivalence (ydata,datac)
# 
#
#

#RED Initialize to false

  founde=.false.
  founds=.false.
  foundq=.false.
  foundt=.false.
  foundf=.false.

#REE Initialize to 0
  lfcchn=0

  iscale=ihc # no plot autoscale

# Check to see if routine is being run on graphics terminal

	if (igrmod >=99) {
		call movabs(0,369)
		write(ttyout,100)
100		format('Cannot execute this routine on a nongraphics',
		       ' terminal ',20(' '))
		
		return
	
	}

# check to see if input is being redirected 
	if (redire)  {
		call movabs(0,369) 
		call sb(0)
		write(outline,101) char(0)
		call gwrite(outline)
101		format('cannot do this, exiting ',a1)
		return
	}	
	
# save original crtin prompt and set new prompt to null
	oiprom=iprom
	iprom=0


# no initial file to be written to
	flflag=0

1000	atemp(1:1)=' '	# if a read error occurs, iopcon must be defined
	
	call serase(0,310*2,511*2,388*2)
	call movabs(0,379*2)
	call sb(0)
	write(outline,103) char(0)
	call gwrite(outline)
	write(outline,104) char(0)
	call gwrite(outline)
	write(outline,105) char(0)
	call gwrite(outline)
	write(outline,106) char(0)
	call gwrite(outline)
	write(outline,107) char(0)
	call gwrite(outline)
103	format(20x,'Interactive Band Analysis',a1)
104	format('enter option',a1)
105	format(3x,'o to open a new file',a1)
106	format(3x,'c to change plot scale',a1)
107	format(3x,'a to analyze features',a1) 
	call movabs(0,309*2)
	call sb(0)
	call crtin

	atemp=iopcon
       	colpos=1
        call wjfren(colpos,x,il) 

#  Make hasty retreat
        if (il==ihx | il==ihe ) {
          iopcon=atemp
	  call movabs(150,327*2)
	  call serase(0,310*2,511*2,378*2)
          call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,
		      olbnd,oubnd,datsc2,nchans)
	  iprom=oiprom 
 	  return
	}

# erase option
	call serase(0,310*2,511*2,378*2)

#  Change plot scale
        if (il==ihc) {

	  igo=6
	  call crtpsc(igo,lbnd,ubnd,nchans,ydata,error,wmina,wmaxa,i,dataa,iscale)

# call the nonexistant routine and hope for the best 
#
#	
	 wvmin=wmina
	 wvmax=wmaxa
	 diff=ubnd-lbnd
	 call er 
	 call replot(ifl1,xmin,xmax,lbnd,xdata,ydata,chan1,chan2,wavshift)
# store parameters 
 
	olbnd=lbnd
	oubnd=diff+lbnd
	owvmin=xmin
	owvmax=xmax

	}

# open file and get title to be written  
	if (il==iho) {
# erase option
		call serase(0,310*2,511*2,378*2)
		call opfile(title)
		flflag=1
	}




# begin analysis 
       	if (il==iha) {
              
# store intial values of plot parameters
# these are used to replot the spectrum after a band has been
# analyzed
	olbnd=lbnd
	oubnd=diff+lbnd
	owvmin=xmin
	owvmax=xmax


# copy original ydata into scratch array
		do chanel=1, nchans  {
			datsc1(chanel)=ydata(chanel)
# copy original error into a scratch array array
			datsc2(chanel)=error(chanel) 
		}

#
# Get first continum position 
#

1		call serase(0,310*2,511*2,378*2)

		call movabs(0,379*2)      # Banner: Interactive Band Analysis
		call sb(0)
		write (outline,103) char(0)
		call gwrite(outline)

		call movabs(0,369*2)
		call sb(0)
		if (igrmod >= 50 & igrmod <= 53) {
			write(outline,203) char(0)
		} else {
			write(outline,202) char(0)
		}
		call gwrite(outline)

202	format(' enter first point on continum (using graphics cursor keys)',a1)
203	format(' enter first point on continum (using left mouse button)',a1)

		call movabs(0,369*2)
		ier = 0
		repeat {        # wait for use input
			call gcrpos(gxpos1,gypos1,txpos,typos,xmax,xmin,
				lbnd,diff,iopcon,ier)
			call bcchan(gxpos1,gypos1,txpos,typos,chan1,nchans,
				xdata,datsc1,ier)
		} until (ier != -1)

#  draw box around the first point 

		side=8	
		call gconvt(gxpos1,gypos1,xdata(chan1),datsc1(chan1),
		       lbnd,diff,xmin,xmax)
		call box(gxpos1,gypos1,side)

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,204)  char(0)
		call gwrite(outline)
204		format(' enter return to continue, r to redo ',a1)
		call movabs(0,354*2)
		call sb(0)
		call crtin
       		atemp=iopcon	
		colpos=1
        	call wjfren(colpos,x,il) 
#  make hasty retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		   call serase(0,310*2,511*2,378*2)
		   call movabs(150*2,327*2)
	 	   call sb(0)
	    	   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,
		   	       olbnd,oubnd,datsc2,nchans)
 	  	   return
		}
# check to see if user wants to redo first point 
	 	if (il==ihr) {
			goto 1
		}

# get second point of continum  
5		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
      	        call sb(0)
		write(outline,206) char(0)
		call gwrite(outline)
206		format(' enter second point on continum',a1)
		call movabs(gxpos1,gypos1)
		call rbline(1)
		repeat {
			call gcrpos(gxpos2,gypos2,txpos,typos,
				xmax,xmin,lbnd,diff,iopcon,ier)
			call bcchan(gxpos2,gypos2,txpos,typos,
				chan2,nchans,xdata,datsc1,ier)
		} until (ier != -1)

# draw box around second point

		call rbline(0)
		side=8
		call gconvt(gxpos2,gypos2,xdata(chan2),datsc1(chan2),
		       lbnd,diff,xmin,xmax)
		call box(gxpos2,gypos2,side)

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,205)  char(0)
		call gwrite(outline)
205	format(' enter r to redo second point only, return to continue',a1)
       	        call movabs(0,354*2)
		call sb(0)	
		call crtin

		atemp=iopcon 
		colpos=1
        	call wjfren(colpos,x,il) 
#  make hasty retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		   call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
 	  	   iprom=oiprom 
                   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,
		  	       olbnd,oubnd,datsc2,nchans)
		   return
		}
# check to see if user wants to redo second  point 
	 	if (il==ihr) {
			goto 5
		}

# Check to see what space we are in 
		wflag=1
		if (itrol(3)==ihn) {
			wflag=0 
		}
#	
# Check to see if the user is happy with this 
#
		
# draw continuum 
		call movabs(gxpos1,gypos1)
		call drwabs(gxpos2,gypos2)

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2) 
       		call sb(0)
     	        write(outline,208) chan1, chan2, char(0)
		call gwrite(outline)
208     	format('enter r to redo both points, return to continue ',
                   'continuum chans=', i6,',', i6,a1)

       	        call movabs(0,354*2)
		call sb(0)	

		call crtin

		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 

#  make hasty retreat
       		if (il==ihx | il==ihe ) {
      		    iopcon=atemp
		    call serase(0,310*2,511*2,378*2)
	 	    call sb(0)
		    iprom=oiprom
          	    call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,
		   	        olbnd,oubnd,datsc2,nchans)
 		    return
		}

# redo 

		if (il==ihr) {
	          call er
		  call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)
		  goto 1
 		}

# else continue on 

# check to see if wavelengths entered are the same 
# if so redo continuum point number two
		if (xdata(chan1)==xdata(chan2) ) goto  5 


# Determine which continuum point is at  the larger wavelength,
# channel or energy  

		if (gxpos1 > gxpos2 ){
			gxtmp=gxpos1
			gxpos1=gxpos2
			gxpos2=gxtmp
                        gytmp=gypos1
                        gypos1=gypos2
                        gypos=gytmp
			chtmp=chan1
			chan1=chan2
			chan2=chtmp
				
		}
# Define continuum equation y=d*x+e in (original) data coordinates


		xtmp = xdata(chan1)-xdata(chan2)
		if (abs(xtmp) < 0.1e-30) xtmp= 0.1e-30
                d=(datsc1(chan1)-datsc1(chan2))/xtmp
		e=datsc1(chan1)-(d*(xdata(chan1)))

# derive continuum ratios

		ctmp=ydata(chan2)
		if (abs(ctmp) < 0.1e-30) ctmp =0.1e-30
		conldivr = ydata(chan1)/ ctmp          # left/right ratio

		ctmp=ydata(chan1)
		if (abs(ctmp) < 0.1e-30) ctmp =0.1e-30
		conrdivl = ydata(chan2)/ ctmp          # right/left ratio


# rescale reflectance data in selected channel range and put
# result in the scratch array datsc1 ( common/dscrch) 
# this will make the continuum have a reflectance of 1
		do chanel=1 , nchans {

                 	   if (datsc1(chanel)!=-1.23e34 ) {
			     xtmp = d*xdata(chanel)+e
			     if (abs(xtmp) < 0.1e-30) xtmp= 0.1e-30
			     datsc1(chanel)=datsc1(chanel)/xtmp
# rescale error	
            		     error(chanel)=error(chanel)/xtmp
			   }
		}




# Find maximum and minimum reflectances in selected channel range 			
		chanmn=chan1
		chanmx=chan2	
	
		refmax=-.9e36
		refmin=.9e36
		
		do chanel=chan1, chan2  {
			if (datsc1(chanel)!=-1.23e34) {
			   if (datsc1(chanel) > refmax) refmax=datsc1(chanel)
		   	   if (datsc1(chanel) < refmin) refmin=datsc1(chanel)
			}
		} 

# adjust reflectances by 10%
		ubnd=refmax+(.1*(refmax-refmin))
		lbnd=refmin-(.1*(refmax-refmin))


# replot spectrum with new vertical axis
		call er
		call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)

# redraw title 
		call serase(0,310*2,511*2,378*2)
		call movabs(0,379*2)
		call sb(0)
		write(outline,103) char(0)
		call gwrite(outline)
#
# gxpos1 and gxpos2 are expected to be scaled to their proper
# values for the new plot with "continuum removed "


# 
# convert reflectance and wavelength coordinates to new pixel
# coordinates
		diff=ubnd-lbnd
		call movabs(0,369*2)
		call gconvt(gxpos1,gypos1,xdata(chan1),datsc1(chan1),
		       lbnd,diff,xmin,xmax)
		call gconvt(gxpos2,gypos2,xdata(chan2),datsc1(chan2),
		       lbnd,diff,xmin,xmax)


#
# Draw continum 
#
	
		call movabs(gxpos1,gypos1)
		call drwabs(gxpos2,gypos2)


# Redraw boxes


#  draw box around the first point 

		side=8	
		call box(gxpos1,gypos1,side)

# draw box around second point

		side=8
		call box(gxpos2,gypos2,side)

# Check to see if continum is ok
209		call movabs(0,369*2)
		call sb(0)
		write(outline,210) char(0)
		call gwrite(outline)
210	format(' enter r to redo, m to do another continum ',
	'or return to continue ',a1)
		call movabs(0,354*2)
		call sb(0)
		write(outline,2101) chan1, chan2,xdata(chan1), xdata(chan2), char(0)
		call gwrite(outline)
2101	format(' continuum chans=', i6,',', i6, '   waves=', f12.6, ',', f12.6, a1)

       	        call movabs(0,354*2)
		call sb(0)	
 		call crtin
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 


#  make hasty retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		   call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
    	  	   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,owvmax,
		  	       olbnd,oubnd,datsc2,nchans)
		   return
		}
# check to see if user wants to redo both points
	 	if (il==ihr) {
		        call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)	
			goto 1
		}

# do another continuum using data currently on screen
		if (il==ihm) {
		      goto 1
		}
# change scale 
		if (il==ihc) {

# replot 
			igo=6
	  		call crtpsc(igo,lbnd,ubnd,nchans,ydata,
				    error,wmina,wmaxa,i,dataa,iscale)
			call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)
# 
# convert reflectance and wavelength coordinates to new pixel
# coordinates
			diff=ubnd-lbnd
			call gconvt(gxpos1,gypos1,xdata(chan1),datsc1(chan1),
			           lbnd,diff,xmin,xmax)
        		call gconvt(gxpos2,gypos2,xdata(chan2),datsc1(chan2),
 	    			   lbnd,diff,xmin,xmax)

# redraw boxes


#  draw box around the first point 

			side=8	
			call box(gxpos1,gypos1,side)

# draw box around second point

			side=8
			call box(gxpos2,gypos2,side)


#
# Draw continum 
#
	
		call movabs(gxpos1,gypos1)
		call drwabs(gxpos2,gypos2)

		goto 209
			

		}
# Draw fitted band center and center as determined by the fwhm points 
# of the fitted band center as well as the fwhm points and connecting
# line 

#
# Find band center by fiting quadratic to three lowest points in the
# band  
#  


# find channel of minimum data point in the band 	


		 	minchn=mindat(datsc1,chan1,chan2)
			
# set the central channel for the error calculation to minchn
			cenchn=minchn

# fit determine coefficients of quadratic fitted about minimum 
# reflectance channel which determines the x coordinate
# See the routine quad for a full explanation because
# its just horrible 		
			call quad(a,b,c,minchn,chan1,chan2,xdata,datsc1)


# determine the central wavelength of the quadratic 
# using (-b)/(2*a)
#
			xtmp = 2*a
			if (abs(xtmp) < 0.1e-30) xtmp= 0.1e-30
			xtmp = -b / xtmp
			if (xtmp < 0.0) xtmp= 0.0
		       fbdctr=xtmp
# compute the reflectance see quad if questions exist as to why
# it is calculated the way it is 

		       frfctr=(a*fbdctr**2+b*fbdctr+c)+1
    
# plot vertical line at band center from band bottom to continum 

# convert to  graphics coordinates 

		call gconvt(gxbctr,gybctr,fbdctr,frfctr,lbnd,diff,xmin,xmax)
		call gconvt(gxcctr,gycctr,fbdctr,1.0,lbnd,diff,xmin,xmax)
		call movabs(gxbctr,gybctr)
		call drwabs(gxcctr,gycctr)

# reassign refctr and bndctr to be the fit selected reflectance and 
# wavelenght of the band center 

                refctr=frfctr
    	        bndctr=fbdctr
		




# Automatically select fwhm 
		  
		  call findwd(xdata,datsc1,bndctr,1.0,refctr,xfwhma,
			      xfwhm1,xfwhm2,refhlf,chan1,chan2,
		    	      chnhf1,chnhf2,xmin,xmax,lbnd,diff,found)      
		  if (.not.(found)) {
		           refhlf=(1.0+refctr)/2.0
# convert coordinates of line at fwhm 
			   call gconvt(gxhflt,gyhflt,xdata(chan1),refhlf,
				       lbnd,diff,xmin,xmax)
			   call gconvt(gxhfrt,gyhfrt,xdata(chan2),refhlf,
				       lbnd,diff,xmin,xmax)

# draw line 
			   call movabs(gxhflt,gyhflt)
		           call drwabs(gxhfrt,gyhfrt)



# Have user select two points along the line at half the band depth

# get first continuum point 
7			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,122) char(0)
			call gwrite(outline)
122			format(' enter first point of intersection',a1)
			repeat {
				call gcrpos(gxhf1,gyhf1,txpos,typos,
					xmax,xmin,lbnd,diff,iopcon,ier)
				call bcchan(gxhf1,gyhf1,txpos,typos,
					chnhf1,nchans,xdata,datsc1,ier)
			} until (ier != -1)
# draw box around first continum point 
			side=5
			call box(gxhf1,gyhf1,side)

			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,204) char(0)
			call gwrite(outline)
	
			call crtin 
			atemp=iopcon
       			colpos=1
       			call wjfren(colpos,x,il) 

#  make hasty retreat
       			if (il==ihx | il==ihe ) {
       			   iopcon=atemp
			   call serase(0,310,511,378)
		 	   call sb(0)
 		  	   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
			   return
			}
# check to see if user wants to redo first point 
		 	if (il==ihr) {
				goto 27 
			}

# get second point of continum  

9			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,126) char(0)
			call gwrite(outline)
126			format(' enter second point on continum',a1)
			call movabs(gxhf1,gyhf1)
			call sb(0)
			repeat {
				call gcrpos(gxhf2,gyhf2,txpos,typos,
					xmax,xmin,lbnd,diff,iopcon,ier)
				call bcchan(gxhf2,gyhf2,txpos,typos,
					chnhf2,nchans,xdata,datsc1,ier)
			} until (ier != -1)

# draw box around second continum point 
			side=5
			call box(gxhf2,gyhf2,side)


			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,204) char(0)
			call gwrite(outline)
       		
			call crtin 
			atemp=iopcon
			colpos=1
       		 	call wjfren(colpos,x,il) 
#  make hasty retreat
       			if (il==ihx | il==ihe ) {
       			   iopcon=atemp
			   call serase(0,310*2,511*2,378*2)
		 	   call sb(0)
			   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
 		  	   return
			}
# check to see if user wants to redo second  point 
		 	if (il==ihr) {
			goto 9 
			}

		call dconvt(xfwhm1,refhlf,gxhf1,gyhf1,lbnd,diff,xmin,xmax)
		call dconvt(xfwhm2,refhlf,gxhf2,gyhf2,lbnd,diff,xmin,xmax)
	        xfwhma=(xfwhm1+xfwhm2)/2.0	



		} 




# convert coordinates of line at fwhm 
		call gconvt(gxhf1,gyhf1,xfwhm1,refhlf,lbnd,diff,
			   xmin,xmax)
		call gconvt(gxhf2,gyhf2,xfwhm2,refhlf,lbnd,diff,
			    xmin,xmax)

# draw boxs around points 
# draw box around first continum point 
		side=5
		call box(gxhf1,gyhf1,side)

# draw box around second continum point 
		side=5
		call box(gxhf2,gyhf2,side)


# draw line 
		call hpline(4)
		call movabs(gxhf1,gyhf1)
		call drwabs(gxhf2,gyhf2)


# determine the center of the intersection points and the
# reflectance value where it intersects the data curve
		

		reffwa=findrf(xdata,datsc1,chan1,chan2,xfwhma)
		call gconvt(gxctra,gyctac,xfwhma,1.0,lbnd,diff,
			   xmin,xmax)
		call gconvt(gxctra,gyctab,xfwhma,reffwa,lbnd,diff,
			   xmin,xmax)

		call movabs(gxctra,gyctab)
		call drwabs(gxctra,gyctac)
# 
# change line type back
		call hpline(1)
#
#
# determination of the "full width quarter max" and subsequent 
# widths if possible 	
                qchans=10
		divnum=2
		chnlft=chnhf1
		chnrht=chnhf2 
		found=.true.
		oxband=xfwhma
		orcont=reffwa
		orband=refhlf
		while (abs((chnlft-chnrht)) >=qchans & found & divnum<7) { 
	  	  call findwd(xdata,datsc1,oxband,orcont,orband,
		              xwdtha,xwdlft,xwdrht,refwth,chnlft,
			      chnrht,nchanl,nchanr,xmin,xmax,lbnd, 
			      diff,found) 
		  if (found) {	
# find the reflectance of at the center point of the new band
# center
	    	    rbnda=findrf(xdata,datsc1,nchanl,nchanr,xwdtha)
		    if (divnum == 2) {
			#  case of the quarter width 
			#  determine xcoordinate in pixel coordinates 
			call gconvt(gxqrta,gyqrtc,xwdtha,refwth,
				    lbnd,diff,xmin,xmax)
			foundq=found
		    }
		    else if (divnum == 3) {
			#  case of the eigth width 
			#  determine xcoordinate in pixel coordinates 
			call gconvt(gxeiga,gyeigc,xwdtha,refwth,lbnd,
				    diff,xmin,xmax)
			founde=found
		    }
		    else if (divnum == 4) {
			#  case of the sixteenth width 
			#  determine xcoordinate in pixel coordinates 
			call gconvt(gxsixa,gysixc,xwdtha,refwth,lbnd,
			            diff,xmin,xmax)
			founds=found
		    }
		    else if (divnum == 5) {
			#  case of the thirtysecondth width 
			#  determine xcoordinate in pixel coordinates 
			call gconvt(gxthra,gythrc,xwdtha,refwth,lbnd,
				    diff,xmin,xmax)
			foundt=found
		    }
		    else if (divnum == 6) {
			#  case if the sixtyfourth width 
			call gconvt(gxsfra,gysfrc,xwdtha,refwth,lbnd,
				    diff,xmin,xmax)
			foundf=found
    	 	     }
# increment to indicate that we are looking for the next width
# maxima
   		     divnum=divnum+1
# reassign all the values necessary to determine the next width
# maxima
		     chnlft=nchanl
		     chnrht=nchanr
		     oxband=xwdtha
		     orband=rbnda
		     orcont=refwth




# set these to plot the width line on the graphics display 

		     oxwthl= xwdlft
		     oxwthr= xwdrht 

		} 		# end if 

        } 		        #  while loop  



# define the channel difference
	 chndif=0
	 do chanel=chnlft, chnrht {
	    if (ydata(chanel)!=-1.23e34 ) {
		chndif=chndif+1
	    }
	}
# determine which width was last drawn
	 below='h'
	 if (foundq) {
		below='q'
		if (founde) {
		   below='e'
		   if (founds) {
			below='s'
		   	if (foundt) {
				below='t'
				if (foundf) {
					below='S'
				}
			}
		   }
		}
	}

# check to see if any of the quarter or higher order widths
# have been calculated if so plot the last one that was found
# as well as its center 
		if (divnum>2) {
# convert coordinates of line at the last width 
			call gconvt(gxwth1,gywth1,oxwthl,orcont,lbnd,diff,
			   xmin,xmax)
			call gconvt(gxwth2,gywth2,oxwthr,orcont,lbnd,diff,
			    xmin,xmax)

# determine the center of the intersection points and the
# reflectance value where it intersects the data curve
		
			rbnda=findrf(xdata,datsc1,chnlft,chnrht,oxband)
			call gconvt(gxdiva,gydivc,oxband,1.0,lbnd,diff,
				   xmin,xmax)
			call gconvt(gxdiva,gydivb,oxband,rbnda,lbnd,diff,
				   xmin,xmax)
		
# draw boxes and lines
			call hpline(1)
			side=3 
			call box(gxwth1,gywth1,side)	
			call box(gxwth2,gywth2,side)
# change line type
			call hpline(7)
# draw width line 
			call movabs(gxwth1,gywth1)
			call drwabs(gxwth2,gywth2)
# draw center line
			call movabs(gxdiva,gydivc)
			call drwabs(gxdiva,gydivb)




		  } 		# end if divnum >2

 
# change line type back
			call hpline(1)

# set method of band selection to be that of the fit
# change if appropriate options are given 
	method='f'	

# Fetch band center 


211		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,212) char(0)
		call gwrite(outline)
212		format( ' enter s to select band center manually ',
			'or return to use fitted center',a1)
       	        call movabs(0,354*2)
		call sb(0)	
 		call crtin
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 


#  retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		   call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
    	  	   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
			       owvmax,olbnd,oubnd,datsc2,nchans)
		   return
		}

# user wants to use own selected center 
	 	if (il==ihs) {

					
# change line type back 
213		       call hpline(1)
			call serase(0,310*2,511*2,378*2)
		       call movabs(0,369*2)
		       call sb(0)
		       write(outline,215) char(0)
			call gwrite(outline)
215		       format('Select band center ',a1)

			call movabs(gxbctr,gycctr)
# get band center from cursor position 

21			repeat {
				call gcrpos(gxcen,gycen,txpos,typos,
					xmax,xmin,lbnd,diff,iopcon,ier)
				call bcchan(gxcen,gycen,txpos,typos,
					cenchn,nchans,xdata,datsc1,iier)
			} until (ier != -1)

# determine whether selected band center is a the average of 
# the fwhm points as determined by the fit if it is set 
# method=h if not method=r
			method='r'
			if (gxcen==gxbctr) method='f'
			if (gxcen==gxctra) method='h'
			if (gxcen==gxqrta) method='q'
			if (gxcen==gxeiga) method='e'
			if (gxcen==gxsixa) method='s'
 			if (gxcen==gxthra) method='t'
			if (gxcen==gxsfra) method='S'

#  Now use selected band center
			   call dconvt(bndctr,refctr,gxcen,gycen,lbnd,
				       diff,xmin,xmax)
#
#
# check to see if band center is within the two continuum points 
			    if (bndctr<=xdata(chan1) |
			        bndctr>=xdata(chan2) )  {
				call movabs(0,369*2)
				call sb(0)
				write(outline,216) char(0)
				call gwrite(outline)
216				format(' band center out of band, redo ',a1)
				goto 213 
			    }
	
		   	    if (bndctr<=xdata(cenchn)) {
				lfcchn=cenchn-1
				while (datsc1(lfcchn) ==-1.23e34 &
				       lfcchn>chan1) {
					lfcchn=lfcchn-1
				}

				mband=(datsc1(lfcchn)-
					datsc1(cenchn)) / (
					xdata(lfcchn) -xdata(cenchn))
                       		 bband=datsc1(cenchn)-mband*xdata(cenchn)
		         	 refctr=mband*bndctr+bband
		     	     } 
		
		 	   if (bndctr>xdata(cenchn)) {

				rtcchn=cenchn+1
				while (datsc1(rtcchn) ==-1.23e34 &
				       rtcchn<chan2) {
					rtcchn=rtcchn+1
				}
			 	mband=(datsc1(rtcchn)-
					datsc1(cenchn)) / (
					xdata(rtcchn) -xdata(cenchn))
                       	        bband=datsc1(cenchn)-mband*xdata(cenchn)
		       	        refctr=mband*bndctr+bband

		     	    } 


# plot vertical line at band center from band bottom to continum 
#

# convert to  graphics coordinates 
			call gconvt(gxbctr,gybctr,bndctr,refctr,lbnd,
			            diff,xmin,xmax)
			call gconvt(gxcctr,gycctr,bndctr,1.0,lbnd,diff,
			            xmin,xmax)

			call movabs(gxbctr,gybctr)
	   		call drwabs(gxcctr,gycctr)


# check to see if band center which was drawn is ok 

			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,214) char(0)
			call gwrite(outline)
214			format( ' Enter return if ok or r to redo center ',a1)

			call movabs(0,354*2)
			call sb(0)
			call crtin
			atemp=iopcon
       			colpos=1
       			call wjfren(colpos,x,il) 

	

       			if (il==ihx | il==ihe ) {
       			   iopcon=atemp
			   call serase(0,310*2,511*2,378*2)
	 		   call sb(0)
    	  		   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
			   return
			}
			crflag=0
			if (il==ihr) {
				goto 213

			}






# compute reflectance level of line at half the band depth
# the furthest most left and furthest most right reflectances
# are the same they are used for readability (ha,ha) 

		refhfl=.5+refctr/2.0
		refhfr=.5+refctr/2.0
		

# Ask about automatic fwhm or manual 

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,221) char(0)
		call gwrite(outline)
		write(outline,1221) char(0)
		call gwrite(outline)
221		format(' enter f to manually select fwhm',a1)
1221		format(' return to have fwhm selected automatically ',a1)
		call movabs(0,354*2)
                call sb(0)
		call crtin 
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 


       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
			call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
 	  	   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
			       owvmax,olbnd,oubnd,datsc2,nchans)
		   return
		}


# manual selection

25	 	if (il==ihf) {

# convert coordinates of line at fwhm 
		call gconvt(gxhflt,gyhflt,xdata(chan1),refhfl,lbnd,diff,
			   xmin,xmax)
		call gconvt(gxhfrt,gyhfrt,xdata(chan2),refhfr,lbnd,diff,
			    xmin,xmax)

# draw line 
		call movabs(gxhflt,gyhflt)
		call drwabs(gxhfrt,gyhfrt)


# Have user select two points along the line at half the band depth


# get first continuum point 
27		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,222) char(0)
		call gwrite(outline)
222		format(' enter first point of intersection',a1)
		repeat {
			call gcrpos(gxhf1,gyhf1,txpos,typos,
				xmax,xmin,lbnd,diff,iopcon,ier)
			call bcchan(gxhf1,gyhf1,txpos,typos,
				chnhf1,nchans,xdata,datsc1,ier)
		} until (ier != -1)
# draw box around first continum point 
		side=3
		call box(gxhf1,gyhf1,side)

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,204) char(0)
		call gwrite(outline)

		call crtin 
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 

#  make hasty retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
 	  	   iprom=oiprom
              	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
			       owvmax,olbnd,oubnd,datsc2,nchans)
		   return
		}
# check to see if user wants to redo first point 
	 	if (il==ihr) {
			goto 27 
		}

# get second point of continum  

31		call movabs(0,369*2)
		call sb(0)
		write(outline,226) char(0)
		call gwrite(outline)
		write(outline,227) char(0)
		call gwrite(outline)
		call gwrite(outline)
226		format(' enter second point on continum',40(' '),a1)
227		format(79(' '),a1)
		call movabs(gxhf1,gyhf1)
		call sb(0)
		repeat {
			call gcrpos(gxhf2,gyhf2,txpos,typos,
				xmax,xmin,lbnd,diff,iopcon,ier)
			call bcchan(gxhf2,gyhf2,txpos,typos,
				chnhf2,nchans,xdata,datsc1,ier)
		} until (ier != -1)

# draw box around second continum point 
		side=3
		call box(gxhf2,gyhf2,side)


		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,204)  char(0)
		call gwrite(outline)
       		
		call crtin 
		atemp=iopcon
		colpos=1
        	call wjfren(colpos,x,il) 
#  make hasty retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
		   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
		    	       owvmax,olbnd,oubnd,datsc2,nchans)
 	  	   return
		}
# check to see if user wants to redo second  point 
	 	if (il==ihr) {
			goto 31 
		}

		call dconvt(xfwhm1,yfwhm1,gxhf1,gyhf1,lbnd,diff,xmin,xmax)
		call dconvt(xfwhm2,yfwhm2,gxhf2,gyhf2,lbnd,diff,xmin,xmax)




		}		# end of manual option 


# Automatically select fwhm 


		else   {

		  call sfwhm(xdata,datsc1,bndctr,refhfl,chan1,chan2,
			     chnhf1,chnhf2,found)
		  if (.not.(found)) {
		     il=ihf		# set select manual option 
	             call er
		     call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)
		     goto 25 
		   } 


# find the xcoordinates of the points of intersection between 
# the horizontal line at half the band depth (refhfl) and the data 
# first find two lines in the form y=mx+b

		 lchhf1=chnhf1-1
	 	 while (ydata(lchhf1) ==-1.23e34 &
		       lchhf1>chan1) {
			lchhf1=lchhf1-1
		 }
		  mfwhm1=(datsc1(lchhf1)-datsc1(chnhf1))/(xdata(lchhf1)-
			xdata(chnhf1))
                  bfwhm1=datsc1(chnhf1)-mfwhm1*xdata(chnhf1) 

		  lchhf2=chnhf2-1
		  while (ydata(lchhf2) ==-1.23e34 &
		       lfcchn>cenchn) {
		       lchhf2=lchhf2-1
		   }
		  mfwhm2=(datsc1(lchhf2)-datsc1(chnhf2))/(xdata(lchhf2)-
			xdata(chnhf2))
                  bfwhm2=datsc1(chnhf2)-mfwhm2*xdata(chnhf2)
                  
# compute intersection points 

		   xfwhm1=(refhfl-bfwhm1)/mfwhm1
		   xfwhm2=(refhfl-bfwhm2)/mfwhm2

# convert coordinates of line at fwhm 
		call gconvt(gxhf1,gyhf1,xfwhm1,refhfl,lbnd,diff,
			   xmin,xmax)
		call gconvt(gxhf2,gyhf2,xfwhm2,refhfr,lbnd,diff,
			    xmin,xmax)

# draw boxs around points 
# draw box around first continum point 
		side=5
		call box(gxhf1,gyhf1,side)

# draw box around second continum point 
		side=5
		call box(gxhf2,gyhf2,side)


# draw line 
		call movabs(gxhf1,gyhf1)
		call drwabs(gxhf2,gyhf2)
# inquire about the correctness of line 

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,229) char(0)
		call gwrite(outline)
229		format(' enter return to continue r to redo manually ',a1)
		call movabs(0,354*2)
		call sb(0)
       		
		call crtin 
		atemp=iopcon
		colpos=1
        	call wjfren(colpos,x,il) 
#  retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
		   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
		   	       owvmax,olbnd,oubnd,datsc2,nchans)
 	  	   return
                 }
    
		  if (il==ihr) {
		     il=ihf		# set select manual option 
	             call er
		     call replot(ifl1,xmin,xmax,lbnd,xdata,datsc1,chan1,chan2,wavshift)
		     goto 25 
		   } 

		}		# end automatic selection of fwhm

	}			# end manual selection of fwhm 

# 		determine band depth in (orginal) data coordinates
#
		bnddep=(1-refctr)

# have found  everything necessary to find fwhm 
# look to see what space we are in and compute it 
		
		bndwth=abs(xfwhm1-xfwhm2)


# compute asymmetry parameter
		asymet=abs(bndctr-xfwhm1)/abs(xfwhm2-bndctr)

# prepare for output

		call serase(0,310*2,511*2,378*2)


# inquire about error

		call movabs(0,369*2)
		call sb(0)
		write(outline,232) char(0)
		call gwrite(outline)
232		format(' enter m to select error or press return to find default error ',a1)

# output continuum channels and  ratios

		call movabs(0,354*2)
		call sb(0)
		write(outline,2101) chan1, chan2,xdata(chan1), xdata(chan2), char(0)
		call gwrite(outline)

		call movabs(0,344*2)
		call sb(0)
		write(outline,2102) conldivr, conrdivl, char(0)
		call gwrite(outline)
2102	format(' continuum ratios:  left/right=', f9.4, ',     right/left=', f9.4, a1)


# derive pre-continuum removed asymmetry ("shoulderness")

		# rcbblc = (rc-bb)/(lc-bb)

		ctmp=ydata(chan2) - ydata(cenchn)
		if (abs(ctmp) < 0.1e-14) ctmp =0.1e-14
		ctmp2 = ydata(chan1) - ydata(cenchn)
		if (abs(ctmp2) < 0.1e-14) ctmp2 =0.1e-14
		rcbblc = ctmp / ctmp2

		# lcbbrc = lc-bb)/(rc-bb)

		ctmp=ydata(chan1) - ydata(cenchn)
		if (abs(ctmp) < 0.1e-14) ctmp =0.1e-14
		ctmp2 = ydata(chan2) - ydata(cenchn)
		if (abs(ctmp2) < 0.1e-14) ctmp2 =0.1e-14
		lcbbrc = ctmp / ctmp2

# output shoulderness

		call movabs(0,334*2)
		call sb(0)
		write(outline,2103) lcbbrc, rcbblc, char(0)
		call gwrite(outline)
2103	format('     shoulderness:    lc-bb-rc=', f9.4, ',       rc-bb-lc=', f9.4, a1)

#  Output band center, width, depth results

		call movabs(0,320*2)
		call sb(0)
		write(outline,230)bndctr,bndwth,bnddep,asymet,char(0)
		call gwrite(outline)
230		format(' Center:',f10.4,'   Width:', f8.4,
			'   Depth:',f7.4,'   Asymmetry:',f8.4,a1)

		call movabs(0,309*2)
		call sb(0)

		write(ttyout,2101) chan1, chan2,xdata(chan1), xdata(chan2), " "  # also write results to terminal
		write(ttyout,2102) conldivr, conrdivl,  " "        # also write results to terminal
		write(ttyout,2103) lcbbrc, rcbblc, " "             # also write results to terminal
		write(ttyout,230)bndctr,bndwth,bnddep,asymet, " "  # also write results to terminal

		call crtin 
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 

# user selected error
       		if (il==ihm ) {
		


# get first error point 
37			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,233) char(0)
			call gwrite(outline)
233			format(' enter first point of error',a1)
			repeat {
				call gcrpos(gxerr1,gyerr1,txpos,typos,
					xmax,xmin,lbnd,diff,iopcon,ier)
				call bcchan(gxerr1,gyerr1,txpos,typos,
					chner1,nchans,xdata,datsc1,ier)
			} until (ier != -1)
			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,204)  char(0)
			call gwrite(outline)

			call crtin
       			colpos=1
			atemp=iopcon
       		 	call wjfren(colpos,x,il) 
# retreat
       			if (il==ihx | il==ihe ) {
       			   iopcon=atemp
			call serase(0,310,511,378)
		 	   call sb(0)
			   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
    		  	   return
			}
# check to see if user wants to redo first point 
		 	if (il==ihr) {
				goto 37 
			}
	
# get second point of error  

41			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,234) char(0)
			call gwrite(outline)
234			format(' enter second of error',a1)
			call movabs(gxerr1,gyerr1)
			call sb(0)
			repeat {
				call gcrpos(gxerr2,gyerr2,txpos,typos,
					xmax,xmin,lbnd,diff,iopcon,ier)
				call bcchan(gxerr2,gyerr2,txpos,typos,
					chner2,nchans,xdata,datsc1,ier)
			}until (ier != -1)
			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0)
			write(outline,204)  char(0)
			call gwrite(outline)
       		
			call crtin	
			colpos=1
			atemp=iopcon
       		 	call wjfren(colpos,x,il) 
			atemp=iopcon
#  retreat
       			if (il==ihx | il==ihe ) {
       			   iopcon=atemp
			call serase(0,310*2,511*2,378*2)
		 	   call sb(0)
			   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
 		  	   return
			}
# check to see if user wants to redo second  point 
	 		if (il==ihr) {
				goto 41 
			}
#
# calculate error 
# 	
			horerr=abs(xdata(chner2)-xdata(chner1))

		} else {

# get error from the spectrum's data 
			chnerl=cenchn-1
			while ((xdata(chnerl)==-1.23e34 |
			  datsc1(chnerl)==-1.23e34) & chnerl>chan1) {
			  chnerl=chnerl-1
			}

			chnerr=cenchn+1
			while ((xdata(chnerr)==-1.23e34 |
			  datsc1(chnerr)==-1.23e34) & chnerr<chan2) {
			  chnerr=chnerr+1
			}
			horerr=abs(xdata(chnerr)-xdata(chnerl))/4.
		}	


# check to see if user wishes to write data to file 

		call serase(0,369*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,238) char(0)
		call gwrite(outline)
238	format(' do you wish to write this to a file (y/n) default is no ',a1)
	
		call  movabs(0,354*2)
		call sb(0)

		call crtin
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 


# retreat
       		if (il==ihx | il==ihe ) {
       		   iopcon=atemp
		call serase(0,310*2,511*2,378*2)
	 	   call sb(0)
		   iprom=oiprom
          	   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
		   	       owvmax,olbnd,oubnd,datsc2,nchans)
    	  	   return
		}
# user wants to  write data to file 
	 	if (il == ihy) {
			if (flflag==0) {
				call opfile(title)
			        flflag=1
			}
# get comment 

			call serase(0,310*2,511*2,378*2)
			call movabs(0,369*2)
			call sb(0) 
			write(ttyout,239)
			write(ttyout,1239)
239			format('enter comment')
1239			format('--------------|')
			call movabs(0,329*2)
			call sb(0)
			read(ttyin,240) coment
240			format(a)

# get device name, letter 
			call namdev(idv1,inam)
		
	
# translate integer letter to normal letter
# define the bar 
	bar='|'			 
	
			write(unit=0,fmt=750)title,bar,bndctr,horerr,bndwth,
					bnddep,asymet,coment,inam,ifl1,method,
					chndif,below
	
# format for the write of the title (20 characters), the | (1
# character), the horizontal axis (f8.4), 1 spaces, the error (f7.4),
# 1 spaces, the fwhm (f7.4), 1 space, the band depth (f6.4), asymmetry (f7.3),
# 2 spaces, the comment field  (15 spaces), 2 spaces, the name of the specpr
# file where the data was absconed,
# the file number, method used to select center, number of channels
# below the level given by the last character.

750 format(a,a,f8.4,1x,f7.4,1x,f7.4,1x,f6.4,1x,f7.3,2x,a,2x,a,1x,i6,1x,a,
	   i4,a)

		}		# end write data to a file 

		call serase(0,310*2,511*2,378*2)
		call movabs(0,369*2)
		call sb(0)
		write(outline,242) char(0)
		call gwrite(outline)
242		format(' enter return to begin again, e to exit ',a1)

	call movabs(0,354*2)
	call sb(0)	
		call crtin
		atemp=iopcon
       		colpos=1
       		call wjfren(colpos,x,il) 
		atemp=iopcon
# check exit
       		if (il==ihx | il==ihe ) {
       	   		iopcon=atemp
			call serase(0,310*2,511*2,378*2)
			   call sb(0)
	   		   iprom=oiprom
          		   call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
				       owvmax,olbnd,oubnd,datsc2,nchans)
 	  		   return
		}
# loop back to the begining 

        call restab(wvmin,wvmax,lbnd,ubnd,diff,error,owvmin,
	            owvmax,olbnd,oubnd,datsc2,nchans)
	call er
# use restored plot parameters to replot  
	call replot(ifl1,xmin,xmax,lbnd,xdata,ydata,chan1,chan2,wavshift)

	} 		# if analysis is desired (il==iha)


	goto 1000 



2000  call serase(0,310*2,511*2,378*2)
      call movabs(0,369*2)
      call sb(0) 
      write(outline,2100) char(0)
	call gwrite(outline)
2100  format(' Error reading terminal input or something else, returning ',a1)
      iprom=oiprom
      return

end
