subroutine ptxtc(it,irec,iclin)
#*********************subrountine ptxtc ************************** 


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
	integer*4 textpos,endpos,linecr

# RED Initialize to 0
	line=0

	textpos=1
	endpos=1
		linecr=17-iclin
	while (textpos<=itxtch) {
			for (i = textpos; i <= itxtch; i = i+1)  {
				if (itext(i:i) == char(10)) {
					line = line +1
					iclin=iclin+1
					if (line == linecr) break
				}
			}
			endpos = i-1
			write (ttyout,500) itext(textpos:endpos)
			textpos = endpos+1
500		         format(a)
	}

return 
end
