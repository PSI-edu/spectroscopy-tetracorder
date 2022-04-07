	subroutine fnhist(ifcn,x)
	implicit integer*4 (i-n)

	include "../common/spmaxes"   # max parameters, must be first

	include "../common/blank"
	include "../common/lbl6"
	include "../common/lbl4"
	include "../common/label1"
	include "../common/lundefs"

	character*8  inm
	call namdev(idv1, inm)



	if (ifcn ==  1) write(ttyout,100) ifcn

        if (ifcn ==  2) write (ihist,200) inm, ifl1

        if (ifcn ==  3) write (ihist,300) inm, ifl1

        if (ifcn ==  4) write (ihist,400) inm, ifl1

        if (ifcn ==  5) write (ihist,500) inm, ifl1

        if (ifcn ==  6) write (ihist,600) inm, ifl1

        if (ifcn ==  7) write (ihist,700) inm, ifl1, x
	
        if (ifcn ==  8) write (ihist,800) x, inm, ifl1
	
        if (ifcn ==  9) write (ihist,900) inm, ifl1
	
        if (ifcn == 10) write (ihist,1000) inm, ifl1
	
        if (ifcn == 11) write (ihist,1100) inm, ifl1
	
        if (ifcn == 12) write (ihist,1200) inm, ifl1

        if (ifcn == 13) write (ihist,1300) inm, ifl1

        if (ifcn == 14) write (ihist,1400) inm, ifl1

        if (ifcn == 15) write (ihist,1500) inm, ifl1

        if (ifcn == 16) write (ihist,1600) inm, ifl1
	
        if (ifcn == 17) write (ihist,1700) inm, ifl1

        if (ifcn == 18) write (ihist,1800) inm, ifl1

        if (ifcn == 19) write (ihist,1900) inm, ifl1
	
        if (ifcn == 20) write (ihist,2000) inm, ifl1

        if (ifcn == 21) write (ihist,2100) inm, ifl1

        if (ifcn == 22) write (ihist,2200) inm, ifl1

        if (ifcn == 23) write (ihist,2300) inm, ifl1

        if (ifcn == 24) write (ihist,2400) inm, ifl1

        if (ifcn == 25) write (ihist,2500) inm, ifl1

        if (ifcn == 26) write (ihist,2600) inm, ifl1

        if (ifcn == 27) write (ihist,2700) inm, ifl1

	return
100     format ('ifcn= ',i6)
200     format ('exp(', a8, ' rec', i6, ')', 36(1h ))
300     format ('ln(', a8, ' rec', i6, ')', 37(1h ))
400     format ('log(', a8, ' rec', i6, ')', 36(1h ))
500     format ('10**(', a8, ' rec', i6, ')', 35(1h ))
600     format ('inverse= 1/(', a8, ' rec', i6, ')')
700     format ('(', a8, ' rec' ,i6, ')**(', 1pe13.6, ')', 10(1h ))
800     format ('(', 1pe13.6, ')**(', a8, ' rec', i6, ')', 10(1h ))
900     format ('sin(',a8,' rec',i6,')   argument in radians',5(1h ))
1000    format ('cos(',a8,' rec',i6,')   argument in radians',5(1h ))
1100    format ('tan(',a8,' rec',i6,')   argument in radians',5(1h ))
1200    format ('inv cos(',a8,' rec',i6,')   result in radians')
1300    format ('inv sin(',a8,' rec',i6,')   result in radians')
1400    format ('inv tan(',a8,' rec',i6,')   result in radians')
1500    format ('sind(',a8,' rec',i6,')   argument in degrees',10(1h ))
1600    format ('cosd(',a8,' rec',i6,')   argument in degrees',10(1h ))
1700    format ('tand(',a8,' rec',i6,')   argument in degrees',10(1h ))
1800    format ('inv cosd(',a8,' rec',i6,')   result in degrees')
1900    format ('inv sind(',a8,' rec',i6,')   result in degrees')
2000    format ('inv tand(',a8,' rec',i6,')   result in degrees')
2100    format ('hyperbolic cos(',a8,' rec',i6,
	   	')   argument in radians')
2200    format ('hyperbolic sin(', a8, ' rec', i6,
		')   argument in radians')
2300    format ('hyperbolic tan(', a8, ' rec', i6,
		')   argument in radians')
2400    format ('abs(', a8, ' rec', i6, ')')
2500    format ('integer part(', a8, ' rec', i6, ')')
2600    format ('fractional part (', a8, ' rec', i6, ')')
2700    format('inverse w/errors= 1 / (',a8,' rec',i6,')')
	end
