integer*4 function type(ch)
character ch
integer*4 OTHER
integer*4 LETTER
integer*4 DIGIT
parameter (OTHER = 0)
parameter (LETTER = 1)
parameter (DIGIT = 2)

if (ichar(ch) >= ichar('a') && ichar(ch) <= ichar('z') ||
	ichar(ch)>= ichar('A') && ichar(ch) <= ichar('Z')) {
	type=LETTER
	return
}

if (ichar(ch) >= ichar('0') && ichar(ch) <= ichar('9')) {
	type=DIGIT
	return
}
type=OTHER
return
end
