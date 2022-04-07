subroutine ptxt1(itf,j,lpline)
#*********************subrountine ptxtl ************************** 


#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
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

	implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"
	include "../common/lundefs"
	integer*4 txtpos,endpos,linecr,linect



	txtpos=1
	endpos=1
# RED Initialized to 0
	line=0

	while (txtpos<=itxtch) {
			call plpage(lpline,inm)
			linecr=50-lpline
			for (i = txtpos; i <= itxtch; i = i+1)  {
				if (itext(i:i) == char(10)) {
					line = line +1
					if (line == linecr) break
				}
			}
			endpos = i-1
			write (lstlun,500) itext(txtpos:endpos)
			linect=(endpos-txtpos)/80
			lpline=lpline+linect
			txtpos = endpos+1
500		         format(a)
	}

return 
end
