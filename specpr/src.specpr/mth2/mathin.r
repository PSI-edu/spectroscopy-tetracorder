	subroutine mathin (infmth,ibncon,dummy,ipcn)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    dread,erored,drite
#ccc  argument list description:
#ccc       arguments: infmth,ibncon,dummy,ipcn
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

    include "../common/lundefs"
#RED
	integer*4 lnb      # function lnb

	character*(*) dummy
	character*(*) ipcn

	integer*4 ier, ilnb

	write(ttyout,1)

50  read(ttllun,rec=27,iostat=ier) dummy

	if (ier == -1) go to 100    #if = -1 EOF encountered
	
	call ertyp('mathin',15,ier)


	ipcn = dummy
	call comprs(ipcn)

	if (infmth==0) write(ttyout,2)
	else {
		write(ttyout,5)
		write(ttyout,6)
	}

	if (ibncon==1) write(ttyout,21)
	else write(ttyout,20)

	ilnb = lnb(ipcn)
	if (ilnb < 1) ilnb = 1
	write(ttyout,200) ipcn(1:ilnb)
	write(ttyout,201)
	return

100 dummy=' '
	do i=1,27
		write(ttllun,rec=i,iostat=ier)dummy
	goto 50

1   format (1x, 24(1h*), ' MATH OPERATIONS ', 24(1h*))
2   format (/,' Type  i  to turn ON information', /)

5   format (' Type: file id  record no.  operation  file id',
            ' record no.  options',/,
            ' operations:  * =MULTIPLY    / =DIVIDE    - =SUBTRACT')
6    format (
	14x,'+ =ADDITION: no file ids required.  adds 30 spectra max.',/,
	' TRIG and ALGEBRA FUNCTIONS: ',
	'type file id record no. : function', /,
	'    exp ln log 10**x 1/x x**c c**x abs int frac',
	' sinh cosh tanh',/,
	'    sin    cos    tan    sind    cosd    tand,',/,
	'    invsin invcos invtan invsind invcosd invtand',
	/,' SPECIAL FUNCTIONS: ',
	'function number; f1=list of special functions', 
	/,' OPTIONS: e',
	' =include','errors;  n =subtraction without airmass calculation',/,
	10x,'b =turn on band normalization; bn =turn off band norm.', /,
	10x,'t =change st;  r =change ra and dec (in subtraction only)', /,
	' EXAMPLE:', 
	' w10-w12,w11-12,v10:exp,f1,v10f12e,d6*c12.43,+',/,
	' EXIT:  e or x  for Program Operations Control')
20      format (' Band normalization: *** OFF ***')
21      format (' Band normalization:  *** ON ***')
200     format (' Previous operation: ',a)
201     format (1x, 73(1h-),/)
	end
