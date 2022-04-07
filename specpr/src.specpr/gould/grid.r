	subroutine grid(dfrom,dto,dincr,afrom,ato,aincr)
	implicit integer*4 (i-n)
#ccc    name:
#ccc    version date:
#ccc    author(s):
#ccc    language:
#ccc
#ccc    short description:
#ccc
#ccc    algorithm description:
#ccc    system requirements:
#ccc    subroutines called:
#ccc    argument list description:
#ccc    parameter description:
#ccc    common description:
#ccc    message files referenced:
#ccc    internal variables:
#ccc    file description:
#ccc    user command lines:
#ccc    update information:
#ccc    NOTES:
#ccc

	if ((dincr<=0.0) || (dfrom>dto)) go to 5
	down=dfrom
1	call line(down,afrom,down,ato,1)
	down=down+dincr
	if (down<=dto) go to 1
#
#       draw vertical lines
#
5	if ((aincr<=0.0) || (afrom>ato)) go to 10
	across=afrom
6	call line(dfrom,across,dto,across,1)
	across=across+aincr
	if (across<=ato) go to 6
#
10	return
	end
