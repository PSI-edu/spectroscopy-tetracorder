integer*4 function iwrite(start,finish,string1)
# A KLUDGE FUNCTION

 	include "../common/lundefs" 
        integer*4 finish
	integer*4 start,istart,ifinish
        character*80 string1 

           istart = start
           ifinish = finish
#DEBUG:	write (ttyout,1) istart, ifinish, string1
#DEBUG:1	format (' istart='i9,5x,' ifinish=',i9,9x, 'string1=',/,a)
           if (istart < 1) {
		write (ttyout,100) istart
100		format ('iwrite error: istaart < 1:',i9)
		istart =1
           }
           if (ifinish > 80) {
		write (ttyout,101) ifinish
101		format ('iwrite error: ifinish >80:',i9)
		ifinish = 80
           }
           write(ttyout,10) string1(istart:ifinish)
%10 	   format(a,$)

	iwrite=istart
return
end
