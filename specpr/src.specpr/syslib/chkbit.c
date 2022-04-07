/*  C function to check the bit value in a 32 bit word.
 *    bits numbered 0 to 31
 *
 *  3/8/85 Roger N Clark
 *
 *  call from fortran:  i = chkbit(iword, ibit)
 *
 */

short chkbit(iword, ibit)

unsigned long *iword;
short *ibit;

{
	short result;

	result = 0;

	if (*ibit ==  0) {
		if ((*iword & 0x00000001L) > 0L) result=1;
	}
	if (*ibit ==  1) {
		if ((*iword & 0x00000002L) > 0L) result=1;
	}
	if (*ibit ==  2) {
		if ((*iword & 0x00000004L) > 0L) result=1;
	}
	if (*ibit ==  3) {
		if ((*iword & 0x00000008L) > 0L) result=1;
	}
	if (*ibit ==  4) {
		if ((*iword & 0x00000010L) > 0L) result=1;
	}
	if (*ibit ==  5) {
		if ((*iword & 0x00000020L) > 0L) result=1;
	}
	if (*ibit ==  6) {
		if ((*iword & 0x00000040L) > 0L) result=1;
	}
	if (*ibit ==  7) {
		if ((*iword & 0x00000080L) > 0L) result=1;
	}
	if (*ibit ==  8) {
		if ((*iword & 0x00000100L) > 0L) result=1;
	}
	if (*ibit ==  9) {
		if ((*iword & 0x00000200L) > 0L) result=1;
	}
	if (*ibit == 10) {
		if ((*iword & 0x00000400L) > 0L) result=1;
	}
	if (*ibit == 11) {
		if ((*iword & 0x00000800L) > 0L) result=1;
	}
	if (*ibit == 12) {
		if ((*iword & 0x00001000L) > 0L) result=1;
	}
	if (*ibit == 13) {
		if ((*iword & 0x00002000L) > 0L) result=1;
	}
	if (*ibit == 14) {
		if ((*iword & 0x00004000L) > 0L) result=1;
	}
	if (*ibit == 15) {
		if ((*iword & 0x00008000L) > 0L) result=1;
	}
	if (*ibit == 16) {
		if ((*iword & 0x00010000L) > 0L) result=1;
	}
	if (*ibit == 17) {
		if ((*iword & 0x00020000L) > 0L) result=1;
	}
	if (*ibit == 18) {
		if ((*iword & 0x00040000L) > 0L) result=1;
	}
	if (*ibit == 19) {
		if ((*iword & 0x00080000L) > 0L) result=1;
	}
	if (*ibit == 20) {
		if ((*iword & 0x00100000L) > 0L) result=1;
	}
	if (*ibit == 21) {
		if ((*iword & 0x00200000L) > 0L) result=1;
	}
	if (*ibit == 22) {
		if ((*iword & 0x00400000L) > 0L) result=1;
	}
	if (*ibit == 23) {
		if ((*iword & 0x00800000L) > 0L) result=1;
	}
	if (*ibit == 24) {
		if ((*iword & 0x01000000L) > 0L) result=1;
	}
	if (*ibit == 25) {
		if ((*iword & 0x02000000L) > 0L) result=1;
	}
	if (*ibit == 26) {
		if ((*iword & 0x04000000L) > 0L) result=1;
	}
	if (*ibit == 27) {
		if ((*iword & 0x08000000L) > 0L) result=1;
	}
	if (*ibit == 28) {
		if ((*iword & 0x10000000L) > 0L) result=1;
	}
	if (*ibit == 29) {
		if ((*iword & 0x20000000L) > 0L) result=1;
	}
	if (*ibit == 30) {
		if ((*iword & 0x40000000L) > 0L) result=1;
	}
	if (*ibit == 31) {
		if ((*iword & 0x80000000L) > 0L) result=1;
	}

	return (result);
}

/* below is identical to above except _ added */

short chkbit_(iword, ibit)

unsigned long *iword;
short *ibit;

{
	short result;

	result = 0;

	if (*ibit ==  0) {
		if ((*iword & 0x00000001L) > 0L) result=1;
	}
	if (*ibit ==  1) {
		if ((*iword & 0x00000002L) > 0L) result=1;
	}
	if (*ibit ==  2) {
		if ((*iword & 0x00000004L) > 0L) result=1;
	}
	if (*ibit ==  3) {
		if ((*iword & 0x00000008L) > 0L) result=1;
	}
	if (*ibit ==  4) {
		if ((*iword & 0x00000010L) > 0L) result=1;
	}
	if (*ibit ==  5) {
		if ((*iword & 0x00000020L) > 0L) result=1;
	}
	if (*ibit ==  6) {
		if ((*iword & 0x00000040L) > 0L) result=1;
	}
	if (*ibit ==  7) {
		if ((*iword & 0x00000080L) > 0L) result=1;
	}
	if (*ibit ==  8) {
		if ((*iword & 0x00000100L) > 0L) result=1;
	}
	if (*ibit ==  9) {
		if ((*iword & 0x00000200L) > 0L) result=1;
	}
	if (*ibit == 10) {
		if ((*iword & 0x00000400L) > 0L) result=1;
	}
	if (*ibit == 11) {
		if ((*iword & 0x00000800L) > 0L) result=1;
	}
	if (*ibit == 12) {
		if ((*iword & 0x00001000L) > 0L) result=1;
	}
	if (*ibit == 13) {
		if ((*iword & 0x00002000L) > 0L) result=1;
	}
	if (*ibit == 14) {
		if ((*iword & 0x00004000L) > 0L) result=1;
	}
	if (*ibit == 15) {
		if ((*iword & 0x00008000L) > 0L) result=1;
	}
	if (*ibit == 16) {
		if ((*iword & 0x00010000L) > 0L) result=1;
	}
	if (*ibit == 17) {
		if ((*iword & 0x00020000L) > 0L) result=1;
	}
	if (*ibit == 18) {
		if ((*iword & 0x00040000L) > 0L) result=1;
	}
	if (*ibit == 19) {
		if ((*iword & 0x00080000L) > 0L) result=1;
	}
	if (*ibit == 20) {
		if ((*iword & 0x00100000L) > 0L) result=1;
	}
	if (*ibit == 21) {
		if ((*iword & 0x00200000L) > 0L) result=1;
	}
	if (*ibit == 22) {
		if ((*iword & 0x00400000L) > 0L) result=1;
	}
	if (*ibit == 23) {
		if ((*iword & 0x00800000L) > 0L) result=1;
	}
	if (*ibit == 24) {
		if ((*iword & 0x01000000L) > 0L) result=1;
	}
	if (*ibit == 25) {
		if ((*iword & 0x02000000L) > 0L) result=1;
	}
	if (*ibit == 26) {
		if ((*iword & 0x04000000L) > 0L) result=1;
	}
	if (*ibit == 27) {
		if ((*iword & 0x08000000L) > 0L) result=1;
	}
	if (*ibit == 28) {
		if ((*iword & 0x10000000L) > 0L) result=1;
	}
	if (*ibit == 29) {
		if ((*iword & 0x20000000L) > 0L) result=1;
	}
	if (*ibit == 30) {
		if ((*iword & 0x40000000L) > 0L) result=1;
	}
	if (*ibit == 31) {
		if ((*iword & 0x80000000L) > 0L) result=1;
	}

	return (result);
}
