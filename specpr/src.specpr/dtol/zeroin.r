subroutine zeroin(charvar)

	character *(*) charvar

	do  j=1,len(charvar) {

		if (charvar(j:j)==' ') {
			charvar(j:j)='0'
		}
	}
return	
end
	
