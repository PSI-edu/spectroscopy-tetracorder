subroutine plot2(icall,nch1,nch2,ipoint)
implicit integer*4(i-n)
#ccc  CRT plotting routine modified from wriout
#ccc  subroutines called:
#ccc            scaling,crtpsc,movabs,sb,wavlng,alplty,chplta
#ccc            iwplta,crtplt,sb,movabs,unglic,what,crtin,wjfren
#ccc            dltpts,redhed,lprpct,setspool,chginf,er,pdata,
#ccc            dumpspool

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/hptrm"
	include "../common/lundefs"
	include "../common/alphabet"
	include "../common/dscrch"

	real*4 lbnd
	character*8 aname
	common /plot1/xmax,xmin,lbnd,diff
	common /cplot1/idwrf,idrf,irwrf,irrf
	character*80 outline	# X window writes
	integer*4 iscale  # = a autoscale, A= autoscale +2% range, B= autoscale, keep 0

	iscale=ihc  # no autoscale on plot


#     calculate parameters for the plot
#
	call er
            
	if (igrmod >= 99) {
		write(ttyout,8) 
 8 		format('Routine not supported on this terminal')
		go to 30
	   }
	if (igrmod < 99) {
10		lbnd = bbnd
		diff = ubnd-lbnd
		call movabs(0,295)
#
#
#     draw the plot
#
		call alplty(lbnd,ubnd)
		diff = ubnd-lbnd
		if (itrol(3)==iha)
			call wvplta(nch2,wvmax,wvmin,iline)
		if (itrol(3)==ihn)
			call iwplta(nch2,wvmax,wvmin,iline)
		xmax = wvmax
		xmin = wvmin
		call crtplt(nch1,xmax,xmin,lbnd,diff,datsc3,datsc2,7)
		call crtplt(nch2,xmax,xmin,lbnd,diff,dataa,datac,3)
		call movabs(0,360)
		call sb(0)

#
#     write lower line information on plot
#
		call movabs(250,0)
		call sb(0)
		write(outline,50) idv1,ifl1,char(0)
		call gwrite(outline)
50		format(1x,'File to Register: ',a1,i5,a1)
		call movabs(0,0)
		call sb(0)
		write(outline,52) idrf,irrf,char(0)
		call gwrite(outline)
52     		format(1x,'Reference Spec: ',a1,i5,a1)

#
#      write header information on plot
#
		call movabs(0,360)
		call sb(0)
		if (icall==1) {
			write(outline,60) char(0)
			call gwrite(outline)
			write(outline,61) char(0)
			call gwrite(outline)
		}
60  		format(' FILE REGISTRATION: function F50 ',a1)
61		format(' Please determine number of windows to be used',a1)
		if (icall==2) {
			write(outline,70) char(0)
			call gwrite(outline)
			write(outline,71) char(0)
			call gwrite(outline)
		}
70		format(' CHANGE SCALE TO APPROPRIATE WINDOW ',a1)
71		format('  Remember to move from short wavelengths to long',a1)
		if (icall==3) {
			write(outline,80) char(0)
			call gwrite(outline)
		}
80		format(' This plot uses NEW WAVELENGTHS ',
			 ' to save them exit using e',a1)

		write(outline,100)	char(0)
		call gwrite(outline)
		write(outline,101)	char(0)
		call gwrite(outline)
		write(outline,102)	char(0)
		call gwrite(outline)
100	 	format(' type: c=change scale,  S=window scaling,',a1)
101		format('e=EXIT and WRITE,  x=EXIT NO WRITE,',a1)
102		format(' b = begin OR continue registration of file',a1)
				
#
		call crtin
		j = 1
		call wjfren(j,x,i)

		if (i==ihc) {
			igo=6
			call crtpsc(igo,bbnd,ubnd,nch2,dataa,error,wmina,wmaxa,i,datac, iscale) 
			go to 10
		    }
		if (i==ihcs) {
			call window(bbnd,ubnd,xmin,xmax,diff)
			wmina=xmin
			wmaxa=xmax
			call er
			go to 10
		    }
		if (i==ihe) {
			call movabs(0,360)
			call sb(0)
			ipoint = ihe
			return
		    }
		if (i==ihx) {
			ipoint =ihx
			ixit = ihx
			ictrl = ihx
			return
		    }
		if (i==ihb) {
			ipoint = ihb
			return
		   }
	  }
 
return
30  ixit = ihx
ipoint=ihx
return
end
