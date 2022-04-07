/*  C subroutine to clear a bit in a 32 bit word.
 *    bits numbered 0 to 31.  If ibit is out of range, iword
 *    is not modified.
 *
 *  3/8/85 Roger N. Clark
 *
 *  from fortran:  call clrbit(iword,ibit)
 */

clrbit(iword, ibit)

unsigned long *iword;
short *ibit;

{
	int i;
	i=1;
	if (*ibit ==  0) {
		*iword = *iword & ~0x00000001L;
	}
	if (*ibit ==  1) {
		*iword = *iword & ~0x00000002L;
	}
	if (*ibit ==  2) {
		*iword = *iword & ~0x00000004L;
	}
	if (*ibit ==  3) {
		*iword = *iword & ~0x00000008L;
	}
	if (*ibit ==  4) {
		*iword = *iword & ~0x00000010L;
	}
	if (*ibit ==  5) {
		*iword = *iword & ~0x00000020L;
	}
	if (*ibit ==  6) {
		*iword = *iword & ~0x00000040L;
	}
	if (*ibit ==  7) {
		*iword = *iword & ~0x00000080L;
	}
	if (*ibit ==  8) {
		*iword = *iword & ~0x00000100L;
	}
	if (*ibit ==  9) {
		*iword = *iword & ~0x00000200L;
	}
	if (*ibit == 10) {
		*iword = *iword & ~0x00000400L;
	}
	if (*ibit == 11) {
		*iword = *iword & ~0x00000800L;
	}
	if (*ibit == 12) {
		*iword = *iword & ~0x00001000L;
	}
	if (*ibit == 13) {
		*iword = *iword & ~0x00002000L;
	}
	if (*ibit == 14) {
		*iword = *iword & ~0x00004000L;
	}
	if (*ibit == 15) {
		*iword = *iword & ~0x00008000L;
	}
	if (*ibit == 16) {
		*iword = *iword & ~0x00010000L;
	}
	if (*ibit == 17) {
		*iword = *iword & ~0x00020000L;
	}
	if (*ibit == 18) {
		*iword = *iword & ~0x00040000L;
	}
	if (*ibit == 19) {
		*iword = *iword & ~0x00080000L;
	}
	if (*ibit == 20) {
		*iword = *iword & ~0x00100000L;
	}
	if (*ibit == 21) {
		*iword = *iword & ~0x00200000L;
	}
	if (*ibit == 22) {
		*iword = *iword & ~0x00400000L;
	}
	if (*ibit == 23) {
		*iword = *iword & ~0x00800000L;
	}
	if (*ibit == 24) {
		*iword = *iword & ~0x01000000L;
	}
	if (*ibit == 25) {
		*iword = *iword & ~0x02000000L;
	}
	if (*ibit == 26) {
		*iword = *iword & ~0x04000000L;
	}
	if (*ibit == 27) {
		*iword = *iword & ~0x08000000L;
	}
	if (*ibit == 28) {
		*iword = *iword & ~0x10000000L;
	}
	if (*ibit == 29) {
		*iword = *iword & ~0x20000000L;
	}
	if (*ibit == 30) {
		*iword = *iword & ~0x40000000L;
	}
	if (*ibit == 31) {
		*iword = *iword & ~0x80000000L;
	}

	return(i);
}

/* below is identical to above except _ added */

clrbit_(iword, ibit)

unsigned long *iword;
short *ibit;

{
	int i;
	i=1;
	if (*ibit ==  0) {
		*iword = *iword & ~0x00000001L;
	}
	if (*ibit ==  1) {
		*iword = *iword & ~0x00000002L;
	}
	if (*ibit ==  2) {
		*iword = *iword & ~0x00000004L;
	}
	if (*ibit ==  3) {
		*iword = *iword & ~0x00000008L;
	}
	if (*ibit ==  4) {
		*iword = *iword & ~0x00000010L;
	}
	if (*ibit ==  5) {
		*iword = *iword & ~0x00000020L;
	}
	if (*ibit ==  6) {
		*iword = *iword & ~0x00000040L;
	}
	if (*ibit ==  7) {
		*iword = *iword & ~0x00000080L;
	}
	if (*ibit ==  8) {
		*iword = *iword & ~0x00000100L;
	}
	if (*ibit ==  9) {
		*iword = *iword & ~0x00000200L;
	}
	if (*ibit == 10) {
		*iword = *iword & ~0x00000400L;
	}
	if (*ibit == 11) {
		*iword = *iword & ~0x00000800L;
	}
	if (*ibit == 12) {
		*iword = *iword & ~0x00001000L;
	}
	if (*ibit == 13) {
		*iword = *iword & ~0x00002000L;
	}
	if (*ibit == 14) {
		*iword = *iword & ~0x00004000L;
	}
	if (*ibit == 15) {
		*iword = *iword & ~0x00008000L;
	}
	if (*ibit == 16) {
		*iword = *iword & ~0x00010000L;
	}
	if (*ibit == 17) {
		*iword = *iword & ~0x00020000L;
	}
	if (*ibit == 18) {
		*iword = *iword & ~0x00040000L;
	}
	if (*ibit == 19) {
		*iword = *iword & ~0x00080000L;
	}
	if (*ibit == 20) {
		*iword = *iword & ~0x00100000L;
	}
	if (*ibit == 21) {
		*iword = *iword & ~0x00200000L;
	}
	if (*ibit == 22) {
		*iword = *iword & ~0x00400000L;
	}
	if (*ibit == 23) {
		*iword = *iword & ~0x00800000L;
	}
	if (*ibit == 24) {
		*iword = *iword & ~0x01000000L;
	}
	if (*ibit == 25) {
		*iword = *iword & ~0x02000000L;
	}
	if (*ibit == 26) {
		*iword = *iword & ~0x04000000L;
	}
	if (*ibit == 27) {
		*iword = *iword & ~0x08000000L;
	}
	if (*ibit == 28) {
		*iword = *iword & ~0x10000000L;
	}
	if (*ibit == 29) {
		*iword = *iword & ~0x20000000L;
	}
	if (*ibit == 30) {
		*iword = *iword & ~0x40000000L;
	}
	if (*ibit == 31) {
		*iword = *iword & ~0x80000000L;
	}

	return(i);
}
