    subroutine ercomp(ityp,irror,cx)
    implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc                   this subroutine computes error for multiplication
#cc                   division and subtraction
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    devlun,finfil,
#cc  argument list description:
#cc       arguments: ityp,irror
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/labl2"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/alphabet"
	include "../common/lundefs"


#
#     compute errors for multiplication,division,subtraction
#
#
#     ityp==0 : subtraction or addition
#     ityp!=0 : mult. or div.
#
ifila = ifl1
ifilb = ifl2
call devlun(0,idv1,iss)
call devlun(0,idv2,is)
if (idv2==ihc) {
    is = -1
}
if (iss!=0) {
    if (is!=0) {
	idad = 2
	idad2 = 2
#
#     calculate errors if requested
#
	write(ttyout,30)
	ims = irmasa
	ixa = ifila
	call finfil(ixa,iss,1,ier)
	if (ier==0) {
	    ifilae = ixa +1
	    ixae = ifilae
	    call finfil(ixae,iss,2,ier)
	    if (ier==0) {
		if (ityp==0) {
#
#     ityp=0: add or sub error analysis
#
		    do j = 1,nchans {
			if (datab(j) == -1.23e+34) {
				error(j) = 0.0
			} else {
				error(j) = datab(j)*datab(j)
			}
		    }
		}
		else {
#
#     ityp.ne.0 : mult. or div. error analysis
#
		    do j = 1,nchans {
			if (dataa(j)!=-1.23e34)
			    if (abs(dataa(j))>0.1e-15 && dataa(j) !=
					-1.23e+34 && datab(j) != -1.23e+34) {
				error(j) = (datab(j)*datab(j))/(dataa(j)*dataa(j))
				next 1
				}
			error(j) = 0.0
		   }
		}
		if (idv2==ihc)
		    do j = 1,nchans {
			datab(j) = 0.0
			dataa(j) = cx
			}
		else {
		    ixb = ifilb
		    call finfil(ixb,is,1,ier)
		    if (ier!=0)
			go to 20
		    ifilbe = ixb +1
		    ixbe = ifilbe
		    call finfil(ixbe,is,2,ier)
		    if (ier!=0)
			go to 20
		    if (ityp==0) {
#
#     ityp=0 add or sub error analysis
#
			do j = 1,nchans {
				if (datab(j) == -1.23e+34) {
					error(j)=0.0
				} else {
				    error(j) = error(j)+datab(j)*datab(j)
				    if (error(j) > 0.1e-34) {
					error(j) = error(j)**0.5
				    } else {
					error(j) = 0
				    }
				}
			}
			go to 10
		     }
		}
#
#     ityp.ne.0 : mult or div
#
		do j = 1,nchans
		    if (datac(j)==(-1.23e34)||dataa(j)==(-1.23e34))
			error(j) = 0.0
		    else {
			if (abs(dataa(j))>0.1e-15)
			    error(j) = error(j)+((datab(j)*datab(j))/(dataa(j)*dataa(j)))
			error(j) = (error(j)**0.5)*(abs(datac(j)))
			}
		10  irmas = ims

# restore header info from data set 1

		ixa = ifila
		call finfil(ixa,iss,1,ier)
		if (ier==0) {
		    mhist = ' '   # clear mhist
		    iwtrns = iwtrna
		    if (iwtrnb<iwtrna)
			iwtrns = iwtrnb
		    airmas = irmas/1000.
		    irror = 0
		    return
		}
            }
	}   
    }
}
20  irror = ihe
return
30  format(1x,'***calculating errors***')
end
