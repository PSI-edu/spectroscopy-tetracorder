c   Fortran 77  (HP-UX fortran)
c
c   This subroutine returns string i of length m from the command
c   line.  i=1 is the command name.
c
c   string lengths are set to 40 characters to be compatible with
c            specpr
c
	subroutine getarg (i, string)

$ALIAS fgetarg = 'Ftn_getarg'(%ref,%ref,%ref)

	integer fgetarg

	character*80 string
	integer*2 actual_len, i, j, jj
	integer*4 argnum, str_len

	str_len = 40
	argnum = i
	actual_len = fgetarg(argnum,string,str_len)

c  null pad result

	do 100 j = 1,40

		jj = 41 - j
		if (string(jj:jj).ne.' ') go to 200
		string(jj:jj) = char(0)

100	continue

c  if the actual_len is = 1 then set character 2 to a blank
c     because there is a strange bug in HP fortran that repeats
c     the character for all 80 chars when printed with format (a)

200	if (actual_len.eq.1) string(2:2) = ' '

	return
	end
