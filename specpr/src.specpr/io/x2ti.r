	subroutine x2ti
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine converts the data in label common
#ccc         from LSI format to TI format
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    a2ti,i2ti,f2ti
#ccc  argument list description:
#ccc        argument: none
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       this routine converts the data in label1 common         #
#       from LSI format to TI format.                           #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

    include "../common/label1"

#convert strings to TIASCII and to upper case

	i = 76
	call a2ti(ititl,i)      #ititl,cta,ctb,sta,stb,datea,dateb
	i = 356
	call a2ti(ihist,i)     #ihist,mhist

#byte swap integers

	i = 17
	call bswap(revs,i)      #revs,filno,ira,idec,irmas,ifut,itimch
	i = 3
	call bswap(nruns,i)      #nruns,ieros,iwtrns

#convert floating point numbers
	i = 266
	call fpv2t(xnrm,i)     #xnrm,scatim,timint,xfut,data

	return
	end

	subroutine a2ti(str,num)
	implicit integer*4 (i-n)
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
#################################################################
#                                                               #
#       this routine converts the string str from ASCII         #
#       to TI-ASCII and also converts lower case to upper       #
#                                                               #
#################################################################

	character*1 str(num)

	do i=1,num {
		j = ichar(str(i))
		if (j >= ichar('a') && j <= ichar('z')) j = j-32
		str(i) = char(j+128)
	}

	return
	end
