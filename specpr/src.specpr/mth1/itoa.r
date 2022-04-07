#itoa - convert integer inta to char string str

integer*4 function itoa(inta,str,size)

integer*4 abs,mod
integer*4 d,i,inta,intval,j,size
character str(size)
character k
character*10 digits 

digits = "0123456789"

intval = abs(inta)

i=1
repeat {
	i = i + 1
	d = mod(intval,10)
	str(i) = digits(d+1:d+1)
	intval = intval/10
} until (intval == 0 || i >= size)

if (inta < 0 && i < size) {
	i = i + 1
	str(i) = '-'
}

itoa = i-1

for (j=1;j<i;j=j+1) {
	k = str(i)
	str(i) = str(j)
	str(j) = k
	i = i - 1
}
return
end
