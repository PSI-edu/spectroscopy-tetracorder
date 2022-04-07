subroutine wdgsb4(iwdflg)
implicit integer*4(i-n)
#ccc  version date: 06/01/83
#ccc  author(s): roger clark & jeff hoover
#ccc  language:  fortran
#ccc
#ccc  short description:
#ccc                    this subroutine prompts the user to enter the
#ccc                    parameters defining observatory location
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                     crtin,getstr,wjfren
#ccc  argument list description:
#ccc        argument: iwdflg
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

logical			iwdflg
character*3		id

iwdflg = .false.
i = 0
repeat {
	write(ttyout,10)
	repeat {
		call crtin
		i = 1
		call getstr(i,id,ier)
		if (ier==0)
			break 1
		write(ttyout,20)
	}
	alat = 0
	if (id=="na ")
		alat = 0.0
	if (id=="mko")
		alat = 19.*3600.+49.*60.+34.
	if (id=="mwo")
		alat = 34.*3600.+12.*60.+59.5
	if (id=="wal")
		alat = 42.*3600.+36.*60.+37.
	if (id=="kpn")
		alat = 31.*3600.+57.*60.+31.
	if (id=="t  ") {
		write(ttyout,30)
		call crtin
		l = 1
		call wjfren(l,x,il)
		if (il!=0 || l>=80 || abs(x)>90.) next 1
		i = x
		call wjfren(l,x,il)
		if (il!=0 || l>=80 || x>59) next 1
		j = x
		call wjfren(l,x,il)
		if (il!=0 || l>=80 || x>60.) next 1
		alat = ((abs(float(i)))*60.+float(j))*60.+x
		if (i<0)
			alat = -1.0*alat
	}
	alat = (alat/3600.)/57.29578
	ictrl = 0
	if (id=="na ")
		break 1
	if (alat!=0)
		break 1
}
if (ititle(1:2)=="zx")
	iwdflg = .true.
return
10  format(2x,
"type in observatory or location: type  mko  for mauna kea observatory",
			  /,35x, "type  na   for not applicable",
			  /,35x, "type  wal  for wallace observatory",
			  /,35x, "type  mwo  for mount wilson observatory",
			  /,35x, "type  kpn  for kitt peak national obs.",
  /,10x,"type  t","  to type in the coordinates of the observing site.",/)

20  format(" error-- reenter")

30  format(2x,"type in latitude in degrees, minutes, seconds; ","two spaces each",/)

end
