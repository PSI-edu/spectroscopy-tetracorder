	subroutine xfti
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This routine converts the data in labell common
#ccc         from TI format to LSI format
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    afti,bswap,ffti
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
#       from TI format to LSI format.                           #
#                                                               #
#################################################################

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/label1"

#       convert strings to ASCII and to lower case

	i = 76
	call afti(ititl,i)		#ititl,cta,ctb,sta,stb,datea,dateb
	i = 356
	call afti(ihist,i)		#ihist,mhist

#       byte swap integers

	i = 17
	call bswap(revs,i)		#revs,filno,ira,idec,irmas,ifut,itimch
	i = 3
	call bswap(nruns,i)		#nruns,ieros,iwtrns

#convert floating point numbers
	i = 266
	call fpt2v(xnrm,i)     #xnrm,scatim,timint,xfut,data

	return
	end

	subroutine afti(str,num)
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
#       this routine converts the string str from TI_ASCII      #
#       from ASCII and also converts upper case to lower        #
#                                                               #
#################################################################

	character*1 str(num)

	do i=1,num {
		j = ichar(str(i))
		j = j+128			#remove TI-ASCII's msb
		if (j >= ichar('A') && j <= ichar('Z')) j = j+32
		str(i) = char(j)
	}

	return
	end
