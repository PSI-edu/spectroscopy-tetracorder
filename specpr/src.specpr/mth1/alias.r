subroutine alias(iopcon)
integer gettok, numals

#RED
integer*4 itoa    # function
integer*4 itoartn

character*80 iopcon
character*80 token
character*15 keywrd(70)
character*10 newstr
integer*4 apntr,bpntr
integer*4 OTHER
integer*4 LETTER
integer*4 DIGIT
parameter (OTHER = 0)
parameter (LETTER = 1)
parameter (DIGIT = 2)

include "keywrd.h"


apntr = 1
bpntr = 1
#
# set numals to how big keywrd is dimensioned
#
numals=70

while (iopcon(bpntr:80) != ' '){
    inum = gettok(iopcon,token,apntr,bpntr)
	for(i=1;i<=numals;i=i+1)
		if (token(1:4) == keywrd(i)(1:4)) { 
			iopcon(apntr:bpntr) = ' '
			newstr = ' '
# RED ????
#            call itoa(i,newstr,10)
	     itoartn = itoa(i,newstr,10)
			iopcon(apntr:apntr) = 'f'
			iopcon(apntr+1:bpntr) = newstr(1:3)
			break
		}

    apntr = bpntr + 2
	bpntr = bpntr + 2
}

#debug: print *,"iopcon = ",iopcon
return
end
