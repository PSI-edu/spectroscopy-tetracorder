integer*4 function gettok(iopcon,token,apntr,bpntr)
character*80 iopcon
character*80 token
integer*4 apntr,bpntr
integer*4 type
integer*4 look
integer*4 iflag
integer*4 OTHER
integer*4 LETTER
integer*4 DIGIT
parameter (OTHER = 0)
parameter (LETTER = 1)
parameter (DIGIT = 2)

gettok = type(iopcon(bpntr:bpntr))
look = type(iopcon(bpntr+1:bpntr+1))
if (gettok == LETTER && look == DIGIT) {
	gettok = look
	bpntr = bpntr + 1
	iflag = 0
	while (gettok == DIGIT) {
		gettok = type(iopcon(bpntr:bpntr))
		bpntr = bpntr + 1
		iflag = 1
	}
	if (iflag == 1 && bpntr > 1) bpntr = bpntr -1
    apntr = bpntr
}
		
for(; bpntr <= 80; bpntr = bpntr + 1) {
	gettok = type(iopcon(bpntr:bpntr))
	if (gettok == OTHER) break
}
if (bpntr > 1) bpntr = bpntr - 1

# check range on pointers, otherwise goes out of valid bounds
#     R Clark 4/10/85

if (bpntr > 80) bpntr = 80
if (apntr > bpntr) apntr = bpntr

token = iopcon(apntr:bpntr)

return
end

