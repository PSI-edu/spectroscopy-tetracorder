subroutine wdgsb3(iwdflg)
implicit integer*4(i-n)

#ccc  version date: 06/01/83
#ccc  author(s): roger clark &  jeff hoover
#ccc  language: fortran
#ccc
#ccc  short description:
#ccc         this subroutine prompts the user to type in the
#ccc         number of channels in his spectra
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     crtin,wjfren
#ccc  argument list description:
#ccc           argument: iwdflg
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  notes:
#ccc

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl7"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/lbl3"
	include "../common/label3"
	include "../common/labelf"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/label1"

logical iwdflg
iwdflg = .false.
repeat {
	write(ttyout,10)
	i = 1
	call crtin
	call wjfren(i,x,il)
} until(il==0)
if (x>maxchn) x = maxchn
if (x<1) x = 1
nchans = x+0.5
if (ititle(1:2)=="zx") iwdflg = .true.
return

10  format(2x,"type in the number of channels in your spectra",/)

end



