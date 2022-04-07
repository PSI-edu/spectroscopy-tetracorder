	subroutine conanl(dataa,datab,datac,idat,datar,data,number,rc,num,it)
	implicit integer*4 (i-q)
	real*4 dataa(number),datab(number),datac(number),
		datar(number),data(number)
#RED
	integer*4 g
	integer*4 rc,num,it,idat(number),f,h

#ccc  name: conanl 
#ccc  version date: May 27, 1987
#ccc  author(s): Noel Gorelick
#ccc  language: ratfor
#ccc
#ccc  short description:  This subroutine contains all the necessary
#ccc			  Calls to do a full continuum peaks analysis
#ccc
#ccc  algorithm description: find local min, then global max.
#ccc  system requirements: none
#ccc  subroutines called:
#ccc
#ccc			hull(dataa,datab,idat,number,data,imp)
#ccc			confix(dataa,datab,idat,datac,number)
#ccc			fill(idat,dataa,datab,number,datac)	
#ccc			remove(datab,datac,datar,number)
#ccc			pfind(idat,number,g,ilast)
#ccc			lowp(datar,ilast,inext,ilow)
#ccc			dpfix(dataa,datab,datac,number)
#ccc
#ccc  argument list description: 
#ccc			dataa == X-data (wavlengths)
#ccc			datab == Y-data (data)
#ccc			datac == Z-data (output)
#ccc			idat == integer data reference array
#ccc			datar == Continuum removed data
#ccc			data == Errors/Tolerance
#ccc			number == number of channels (nchans)
#ccc			rc == Remove final continuum prompt answer
#ccc			num == number of iterations to do
#ccc			it == Original/Continuum removed prompt answer
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables: 
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#	initialize
	g=0
	imp=1
#	Do first Iteration
61	do i=1,number {
		idat(i)=0
	}
	g=0
#	first iteration
	call hull(dataa,datab,idat,number,data,imp)
	call confix(dataa,datab,idat,datac,data,number)
	if (num==1) go to 576 
#----------------------------------------------------
#	start other iterations
 	do i=2,num {
		call fill(idat,dataa,datab,number,datac)	
		call remove(datab,datac,datar,number)
		if (it==1) {
			call remove(datab,datac,datab,number)
			call remove(data,datac,data,number)
		}
 		call pfind(idat,number,g,ilast)
71	 	if (ilast<number) {
			f=0
			h=0
	 		call pfind(idat,number,ilast,inext)
			call lowp(datar,ilast,inext,ilow)
	 		call hull(dataa(ilast),datab(ilast),idat(ilast),
				ilow-ilast+1,data(ilast),f)
	 		call hull(dataa(ilow),datab(ilow),idat(ilow),
				inext-ilow+1,data(ilow),h)
			if (f==1 & h==1) {
				idat(ilow)=1
				call should(dataa(ilast),datab(ilast),
					ilow-ilast+1,idat(ilast),data(ilast),1)
				call should(dataa(ilow),datab(ilow),
					inext-ilow+1,idat(ilow),data(ilow),-1)
				call fill(idat,dataa,datab,number,datac)	
				goto 444
			}
			call confix(dataa(ilast),datab(ilast),idat(ilast),
				datac(ilast),data(ilast),inext-ilast+1)
444	 		ilast=inext
	 		go to 71
		}
#	remove noise
#		call fill(idat,dataa,datab,number,datac)
#			do im=1, number {
#				if (datac(im)<1e-25) next
#				if (abs(datab(im)-datac(im))<=data(im)) {
#					idat(im)=1
#				}
#			}
	}
576	call fill(idat,dataa,datab,number,datac)	
	if (rc==1) call remove (datab,datac,datac,number)
	call dpfix(dataa,datab,datac,number)
#	-----------------------------------------------
	return
	end
