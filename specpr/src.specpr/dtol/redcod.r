#
# redcod - decodes search command line
#
integer*4 function redcod(renum,restr,strng)

implicit integer*4 (i-n)
	include "../common/spmaxes"   # max parameters, must be first


include "../common/alphabet"
include "../common/lbl4"
include "srchdefs.h"

integer*2 renum(1:MAXFIELD)
integer*4 index
character restr(1:MAXFIELD)*1
character strng(1:MAXFIELD)*80
#data ihvbar /'|'/
ihvbar = ihchar('|')
#data ihamp /'&'/
ihamp = ihchar('&')

#
#  this search stuff, searches different fields, e.g. ititl,
#  mhist etc. At present 4 such fields are search so MAXFIELD
#  is four.
#
#  The peculiar statement recomp(".") initializes some buffer
#  for the regex command and has the effect of matching everything
#  effectively initializing da ting
#
for(i=1;i<=MAXFIELD;i=i+1) {
    restr(i)=' '
    strng(i)=' ' // char(0)
    renum(i)=0
}
i=1
k=0
for(iptr=1;iptr<=MAXFIELD;iptr=iptr+1) {
 
# early finish when the rest of the command line is blank
#
    if (iopcon(k+1:80) == ' ') {
	redcod=1
	return
    }

    call wjfren(i,x,il)
    if (il == iht) 
		restr(iptr) = 't'
	else if (il == ihh)
		restr(iptr) = 'h'
	else if (il == ihd)
		restr(iptr) = 'd'
	else if (il == ihm)
		restr(iptr) = 'm'
	else if (il == ihvbar) {
		renum(iptr) = OR
		iptr=iptr-1
		next 1
	}
	else if (il == ihamp) {
		renum(iptr) = AND
		iptr=iptr-1
		next 1
	}
	else if (il == ihe || il == ihx) {
		for(l=1;l<=MAXFIELD;l=l+1) {
			restr(l) = ' '
			strng(l) = ' ' // char(0)
			renum(l) = 0
		}
		call recomp("." // char(0))
		redcod=-1
		return
	}
	else {
		call what(i)
		print *,"Syntax Error: expected t,h,d or m"
		redcod=0
		return
	}

	j = i-1+index(iopcon(i:80),'"')
	k = j+index(iopcon(j+1:80),'"')

	if ((j==k) || (j == 0 || k == 0)) {
		print *,"Syntax Error: missing double quote"
		redcod=0
		return
	}
	strng(iptr)=iopcon(j+1:k-1) // char(0)
	i=k+1
}
redcod=1
return	
end
